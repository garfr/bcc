#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "bcc/error.h"
#include "bcc/pp.h"
#include "bcc/lexer.h"
#include "bcc/utils.h"
#include "bcc/parser.h"

#define SYM_TABLE_INIT_SIZE 8

/* Holds the current state of the parser */
typedef struct {
  Scope *currentScope;
  Lexer *lex;
  Hashtbl *typeTable;
} Parser;

static Type *VoidType = &(Type){.type = TYP_VOID, {}};
static Type *BooleanType = &(Type){.type = TYP_BOOL, {}};
static Type *CharType = &(Type){.type = TYP_CHAR, {}};

/* Bit flags used by continueUntil to allow continuing until one of several
 * tokens is reached */
enum TokTypeBits {
  TOK_INT_BITS = 1 << 0,
  TOK_COLON_BITS = 1 << 1,
  TOK_SEMICOLON_BITS = 1 << 2,
  TOK_EQUAL_BITS = 1 << 3,
  TOK_SYM_BITS = 1 << 4,
  TOK_LET_BITS = 1 << 5,
  TOK_ARROW_BITS = 1 << 6,
  TOK_LPAREN_BITS = 1 << 7,
  TOK_RETURN_BITS = 1 << 8,
  TOK_PERIOD_BITS = 1 << 9,
  TOK_DO_BITS = 1 << 10,
  TOK_RBRACKET_BITS = 1 << 11,
};

/* Runs through tokens until a token passed in bitflags is reached, which is
 * then returned */
static Token
continueUntil(Lexer *lex, int bitFlags) {
  Token tok;
  for (;;) {
    tok = peekToken(lex);
    if (tok.type == TOK_EOF) {
      printErrors();
      exit(1);
    }
    if ((tok.type == TOK_SYM && bitFlags & TOK_SYM_BITS) ||
        (tok.type == TOK_INT && bitFlags & TOK_INT_BITS) ||
        (tok.type == TOK_SEMICOLON && bitFlags & TOK_SEMICOLON_BITS) ||
        (tok.type == TOK_COLON && bitFlags & TOK_COLON_BITS) ||
        (tok.type == TOK_LET && bitFlags & TOK_LET_BITS) ||
        (tok.type == TOK_ARROW && bitFlags & TOK_ARROW_BITS) ||
        (tok.type == TOK_LPAREN && bitFlags & TOK_LPAREN_BITS) ||
        (tok.type == TOK_PERIOD && bitFlags & TOK_PERIOD_BITS) ||
        (tok.type == TOK_DO && bitFlags & TOK_DO_BITS) ||
        (tok.type == TOK_RBRACKET && bitFlags & TOK_RBRACKET_BITS) ||
        (tok.type == TOK_RETURN && bitFlags & TOK_RETURN_BITS)) {
      break;
    } else {
      nextToken(lex);
    }
  }
  nextToken(lex);
  return tok;
}

static void
pushScope(Parser *parser) {
  Scope *sc = newScope(parser->currentScope);

  parser->currentScope = sc;
}

/* Creates a new expression with the same position information as given
 * token */
static Expr *
exprFromToken(Token tok, enum ExprType type) {
  Expr *exp = calloc(1, sizeof(Expr));
  exp->type = type;
  exp->start = tok.start;
  exp->end = tok.end;
  return exp;
}

static Expr *
exprFromTwoPoints(size_t start, size_t end, enum ExprType type) {
  Expr *exp = calloc(1, sizeof(Expr));
  exp->type = type;
  exp->start = start;
  exp->end = end;
  return exp;
}

static Stmt *
stmtFromTwoPoints(size_t start, size_t end, enum StmtType type) {
  Stmt *ret = calloc(1, sizeof(Stmt));
  ret->start = start;
  ret->end = end;
  ret->type = type;
  return ret;
}

static HashEntry *
addToScope(Scope *scope, Symbol sym, bool isMut) {
  TypedEntry *entry = calloc(1, sizeof(TypedEntry));
  entry->isMut = isMut;
  entry->type = NULL;
  entry->onStack = false;
  return insertHashtbl(scope->vars, sym, entry);
}

/* This is a bit janky, but the idea is that given a token, this returns an
 * integer of the number of bytes the type uses if it fits the form x[integer]
 * where x can be any character. It's used to find the bit size of builtin
 * integer types */
static int64_t
convertSymbolInt(Token tok) {
  assert(tok.type == TOK_SYM);

  int64_t ret = 0;
  for (size_t i = 1; i < tok.sym.len; i++) {
    if (!isdigit(tok.sym.text[i])) {
      return -1;
    }
    ret *= 10;
    ret += tok.sym.text[i] - '0';
  }
  if (ret % 8 != 0 || ret > 64) {
    queueError(msprintf("Signed and unsigned integer types must have a "
                        "bitsize that is a power of two and be less than "
                        "64, not %zd",
                        ret),
               tok.start, tok.end);
    printErrors();
  }

  return ret;
}

static Type *
parseType(Parser *parser) {
  Type *ret;

  Token tok = nextToken(parser->lex);

  switch (tok.type) {
    case TOK_AMP:
      {
        Type *ret = calloc(1, sizeof(Type));
        if (peekToken(parser->lex).type == TOK_MUT) {
          nextToken(parser->lex);
          ret->ptr.mut = true;
        } else {
          ret->ptr.mut = false;
        }

        Type *nextType = parseType(parser);
        ret->type = TYP_PTR;
        ret->ptr.type = nextType;
        return ret;
      }
    case TOK_LBRACKET:
      {
        // Array/slice try
        Token numToken = nextToken(parser->lex);
        if (numToken.type == TOK_INT || numToken.type == TOK_UNDERLINE) {
          // Must be an array type
          Token rBracketTok = nextToken(parser->lex);
          if (rBracketTok.type != TOK_RBRACKET) {
            queueError("Expected ']' after size of array", rBracketTok.start,
                       rBracketTok.end);
            rBracketTok = continueUntil(parser->lex, TOK_RBRACKET_BITS);
          }

          Type *arrayType = parseType(parser);
          Type *ret = calloc(1, sizeof(Type));
          if (numToken.type == TOK_INT) {
            if (!symbolToInt(numToken.intnum, &ret->array.size)) {
              queueError("Invalid size of array", rBracketTok.start,
                         rBracketTok.end);
              printErrors();
            }
          } else {
            ret->array.size = -1;
          }
          ret->array.type = arrayType;
          if (ret->array.type == NULL) {
            printf("WHATO\n");
            exit(1);
          }
          ret->type = TYP_ARRAY;
          return ret;
        }
        if (numToken.type == TOK_RBRACKET) {
          // This is a slice

          assert(false);
          exit(1);
        }
      }
      assert(false);
      return NULL;
    case TOK_SYM:
      switch (tok.sym.text[0]) {
        case 's':
          ret = calloc(1, sizeof(Type));
          switch (convertSymbolInt(tok)) {
            case 8:
              ret->type = TYP_S8;
              break;
            case 16:
              ret->type = TYP_S16;
              break;
            case 32:
              ret->type = TYP_S32;
              break;
            case 64:
              ret->type = TYP_S64;
              break;
          }
          return ret;
        case 'u':
          ret = calloc(1, sizeof(Type));
          switch (convertSymbolInt(tok)) {
            case 8:
              ret->type = TYP_U8;
              break;
            case 16:
              ret->type = TYP_U16;
              break;
            case 32:
              ret->type = TYP_U32;
              break;
            case 64:
              ret->type = TYP_U64;
              break;
          }
          return ret;
        default:
          {
            HashEntry *entry = findHashtbl(parser->typeTable, tok.sym);
            if (entry == NULL) {
              queueError(msprintf("Could not find a type with name %.*s",
                                  (int)tok.sym.len, tok.sym.text),
                         tok.start, tok.end);
              printErrors();
            }
            ret = calloc(1, sizeof(Type));
            ret->type = TYP_BINDING;
            ret->typeEntry = entry;
            return ret;
          }
      }
      assert(false);
      return NULL;
    case TOK_VOID:
      return VoidType;
    case TOK_CHAR:
      return CharType;
    case TOK_BOOL:
      return BooleanType;
    case TOK_LPAREN:
      {
        // This is a function type
        Vector *params = newVector(sizeof(Type *), 0);
        while (peekToken(parser->lex).type != TOK_RPAREN) {
          Type *tempType = parseType(parser);
          pushVector(params, &tempType);
        }

        nextToken(parser->lex); // Skip past the last parenthesis

        Token arrowTok = nextToken(parser->lex);
        if (arrowTok.type != TOK_ARROW) {
          queueError("Expected '->' after function parameters", arrowTok.start,
                     arrowTok.end);
          printErrors();
        }

        Type *retType = parseType(parser);
        Type *finalType = calloc(1, sizeof(Type));
        finalType->fun.args = params;
        finalType->fun.retType = retType;
        finalType->type = TYP_FUN;
        return finalType;
      }

    default:
      queueError("This token cannot start a type", tok.start, tok.end);
      printErrors();
      return NULL;
  }
}

/* Includes all the above types, plus records and such */
static Type *
parseComplexType(Parser *parser) {
  Token firstTok = peekToken(parser->lex);

  if (firstTok.type == TOK_RECORD) {
    nextToken(parser->lex);
    Hashtbl *recordFields = newHashtbl(0);
    Vector *vec = newVector(sizeof(HashEntry *), 0);

    while (peekToken(parser->lex).type != TOK_END) {
      Token symTok = nextToken(parser->lex);
      if (symTok.type != TOK_SYM) {
        queueError("Expected name of record field", symTok.start, symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
      }
      Token colonTok = nextToken(parser->lex);
      if (colonTok.type != TOK_COLON) {
        queueError("Expected ':' after name of record field", colonTok.start,
                   colonTok.end);
        colonTok = continueUntil(parser->lex, TOK_COLON_BITS);
      }

      Type *fieldType = parseType(parser);

      Token commaToken = peekToken(parser->lex);
      if (commaToken.type == TOK_COMMA) {
        nextToken(parser->lex);
      }
      if (peekToken(parser->lex).type == TOK_NEWLINE) {
        nextToken(parser->lex);
      }
      HashEntry *entry = insertHashtbl(recordFields, symTok.sym, fieldType);

      pushVector(vec, &entry);

      if (entry == NULL) {
        queueError(msprintf("Cannot redeclare record field '%.*s'",
                            (int)symTok.sym.len, symTok.sym.text),
                   symTok.start, symTok.end);
      }
    }

    // Skip over the end token
    nextToken(parser->lex);

    Type *type = calloc(1, sizeof(Type));
    type->type = TYP_RECORD;
    type->record.recordFields = recordFields;
    type->record.vec = vec;
    return type;
  }
  return parseType(parser);
}

static Expr *parseExpr(Parser *parser);

static Expr *
parseFuncall(Parser *parser, Token symTok) {
  HashEntry *entry = findInScope(parser->currentScope, symTok.sym);

  Vector *args = newVector(sizeof(Expr *), 0);

  while (peekToken(parser->lex).type != TOK_RPAREN) {
    Expr *expr = parseExpr(parser);
    pushVector(args, &expr);
  }

  Token endTok = nextToken(parser->lex);

  Expr *funcall = calloc(1, sizeof(Expr));
  funcall->type = EXP_FUNCALL;
  funcall->funcall.arguments = args;
  funcall->start = symTok.start;
  funcall->funcall.name = symTok.sym;

  if (entry != NULL) {
    funcall->funcall.entry = entry;
    funcall->typeExpr = ((TypedEntry *)entry->data)->type;
  }

  funcall->end = endTok.end;
  return funcall;
}

static Expr *
parseRecordLit(Parser *parser, Token symTok) {
  HashEntry *entry = findHashtbl(parser->typeTable, symTok.sym);

  if (entry == NULL) {
    queueError(msprintf("Could not find a type binding for name '%.*s'",
                        (int)symTok.sym.len, symTok.sym.text),
               symTok.start, symTok.end);
  }

  Hashtbl *recordLitFields = newHashtbl(0);

  while (peekToken(parser->lex).type != TOK_RCURLY) {
    Token periodTok = nextToken(parser->lex);
    if (periodTok.type != TOK_PERIOD) {
      queueError("Expected '.' before name of record field", periodTok.start,
                 periodTok.end);
      periodTok = continueUntil(parser->lex, TOK_PERIOD_BITS);
    }

    Token symTok = nextToken(parser->lex);
    if (symTok.type != TOK_SYM) {
      queueError("Expected name of record field", symTok.start, symTok.end);
      symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    Token equalsTok = nextToken(parser->lex);
    if (equalsTok.type != TOK_EQUAL) {
      queueError("Expected ':' after name of record field", equalsTok.start,
                 equalsTok.end);
      equalsTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
    }

    Expr *fieldExpr = parseExpr(parser);

    Token commaToken = peekToken(parser->lex);
    if (commaToken.type == TOK_COMMA) {
      nextToken(parser->lex);
    }

    if (insertHashtbl(recordLitFields, symTok.sym, fieldExpr) == NULL) {
      queueError(msprintf("Duplicate field '%.*s' in record literal",
                          (int)symTok.sym.len, symTok.sym.text),
                 symTok.start, symTok.end);
    }
  }

  Token rparenToken = nextToken(parser->lex);

  Expr *ret = exprFromTwoPoints(symTok.start, rparenToken.end, EXP_RECORDLIT);
  ret->reclit.fields = recordLitFields;
  ret->reclit.type = entry;
  return ret;
}

static Expr *
parseArrayLit(Parser *parser) {
  lexerStepBack(parser->lex);

  Token startTok = peekToken(parser->lex);

  Type *arrayType = parseType(parser);

  Token lBracketTok = nextToken(parser->lex);
  if (lBracketTok.type != TOK_LBRACKET) {
    queueError("Expected '[' for array literal", lBracketTok.start,
               lBracketTok.end);
    printErrors();
  }

  Vector *items = newVector(sizeof(Expr *), 0);

  for (;;) {
    Expr *tempExpr = parseExpr(parser);
    pushVector(items, &tempExpr);

    if (peekToken(parser->lex).type == TOK_COMMA) {
      nextToken(parser->lex);
    } else {
      break;
    }
  }

  if (arrayType->array.size == -1) {
    arrayType->array.size = items->numItems;
  }

  Token closingBracketTok = nextToken(parser->lex);
  if (closingBracketTok.type != TOK_RBRACKET) {
    queueError("Expected ']' after expression without command in array literal",
               closingBracketTok.start, closingBracketTok.end);
    closingBracketTok = continueUntil(parser->lex, TOK_RBRACKET_BITS);
  }

  Expr *ret = calloc(1, sizeof(Expr));
  ret->start = startTok.start;
  ret->end = closingBracketTok.end;
  ret->type = EXP_ARRAY;
  ret->array.items = items;
  ret->array.type = arrayType;
  return ret;
}

static Expr *
parsePrimary(Parser *parser) {
  Expr *ret;

  Token tok = nextToken(parser->lex);
  switch (tok.type) {
    case TOK_INT:
      ret = exprFromToken(tok, EXP_INT);
      ret->intlit = tok.intnum;
      ret->typeExpr = NULL;
      return ret;
    case TOK_LBRACKET:
      return parseArrayLit(parser);
    case TOK_CHARLIT:
      ret = exprFromToken(tok, EXP_CHAR);
      ret->character = tok.character;
      ret->typeExpr = NULL;
      return ret;
    case TOK_SYM:

      switch (peekToken(parser->lex).type) {
        case TOK_LPAREN:
          nextToken(parser->lex);
          return parseFuncall(parser, tok);
        case TOK_LCURLY:
          nextToken(parser->lex);
          return parseRecordLit(parser, tok);

        default:
          ret = exprFromToken(tok, EXP_VAR);

          HashEntry *entry = findInScope(parser->currentScope, tok.sym);
          if (entry == NULL) {
            queueError(msprintf("Cannot find variable: '%.*s' in any "
                                "scope. Must be undeclared",
                                tok.sym.len, (char *)tok.sym.text),
                       tok.start, tok.end);
            /* Must fail */
            printErrors();
          }
          ret->var = entry;
          ret->typeExpr = NULL;

          while (peekToken(parser->lex).type == TOK_LBRACKET) {
            nextToken(parser->lex);
            Expr *innerExpr = parseExpr(parser);
            Token endIndexTok = nextToken(parser->lex);
            if (endIndexTok.type != TOK_RBRACKET) {
              queueError("Expected ']' after expression in array index",
                         endIndexTok.start, endIndexTok.end);
              printErrors();
            }

            Expr *newRet =
                exprFromTwoPoints(ret->start, endIndexTok.end, EXP_INDEX);
            newRet->index.lval = ret;
            newRet->index.indexVal = innerExpr;
            ret = newRet;
          }

          return ret;
      }
    case TOK_FALSE:
      ret = exprFromToken(tok, EXP_BOOL);
      ret->boolean = false;
      ret->typeExpr = NULL;
      return ret;
    case TOK_TRUE:
      ret = exprFromToken(tok, EXP_BOOL);
      ret->boolean = true;
      ret->typeExpr = NULL;
      return ret;

    default:
      queueError(msprintf("Expected an integer or variable name for "
                          "expressions, not another token"),
                 tok.start, tok.end);
      printErrors();
      /* This never gets called */ exit(1);
  }
}

static int
parseBinop(Parser *parser) {
  Token tok = nextToken(parser->lex);
  switch (tok.type) {
    case TOK_PLUS:
      return BINOP_ADD;
    case TOK_MINUS:
      return BINOP_SUB;
    case TOK_STAR:
      return BINOP_MUL;
    case TOK_SLASH:
      return BINOP_DIV;
    case TOK_DOUBLEEQUAL:
      return BINOP_EQUAL;
    case TOK_NOTEQUAL:
      return BINOP_NOTEQUAL;
    case TOK_AND:
      return BINOP_AND;
    case TOK_OR:
      return BINOP_OR;
    case TOK_LANGLE:
      return BINOP_LESS;
    case TOK_RANGLE:
      return BINOP_GREAT;
    case TOK_LANGLE_EQ:
      return BINOP_LESS_EQ;
    case TOK_RANGLE_EQ:
      return BINOP_GREAT_EQ;
    default:
      queueError(msprintf("Expected arithmetic operation"), tok.start, tok.end);
      /* This doesn't need to fail but idc */
      printErrors();
      exit(1);
  }
}

static Expr *
parseUnary(Parser *parser) {
  switch (peekToken(parser->lex).type) {
    case TOK_AT:
      {
        Token startTok = nextToken(parser->lex);
        Expr *right = parseUnary(parser);
        Expr *ret = calloc(1, sizeof(Expr));
        ret->type = EXP_DEREF;
        ret->start = startTok.start;
        ret->end = right->end;
        ret->deref = right;
        return ret;
      }
    case TOK_AMP:
      {
        Expr *ret = calloc(1, sizeof(Expr));
        Token startTok = nextToken(parser->lex);
        if (peekToken(parser->lex).type == TOK_MUT) {
          nextToken(parser->lex);
          ret->addr.mut = true;
        } else {
          ret->addr.mut = false;
        }
        ret->type = EXP_ADDROF;
        Expr *right = parseUnary(parser);
        ret->start = startTok.start;
        ret->end = right->end;
        ret->addr.expr = right;
        return ret;
      };
    default:
      return parsePrimary(parser);
  }
}

static Expr *
parseFactor(Parser *parser) {
  Expr *exp = parseUnary(parser);

  while (peekToken(parser->lex).type == TOK_STAR ||
         peekToken(parser->lex).type == TOK_SLASH) {
    int op = parseBinop(parser);
    Expr *right = parseUnary(parser);
    Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
    newExpr->binop.exp1 = exp;
    newExpr->binop.exp2 = right;
    newExpr->binop.op = op;
    newExpr->typeExpr = NULL;
    exp = newExpr;
  }

  return exp;
}

static Expr *
parseTerm(Parser *parser) {
  Expr *exp = parseFactor(parser);

  while (peekToken(parser->lex).type == TOK_PLUS ||
         peekToken(parser->lex).type == TOK_MINUS) {
    int op = parseBinop(parser);
    Expr *right = parseFactor(parser);
    Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
    newExpr->binop.exp1 = exp;
    newExpr->binop.exp2 = right;
    newExpr->binop.op = op;
    newExpr->typeExpr = NULL;
    exp = newExpr;
  }

  return exp;
}

static Expr *
parseComparison(Parser *parser) {
  Expr *exp = parseTerm(parser);

  // So the while loop doesn't need to call
  // peekToken for each case
  int whileType = peekToken(parser->lex).type;

  while (whileType == TOK_EQUAL || whileType == TOK_NOTEQUAL ||
         whileType == TOK_LANGLE || whileType == TOK_RANGLE ||
         whileType == TOK_LANGLE_EQ || whileType == TOK_RANGLE_EQ) {
    int op = parseBinop(parser);
    Expr *right = parseTerm(parser);
    Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
    newExpr->binop.exp1 = exp;
    newExpr->binop.exp2 = right;
    newExpr->binop.op = op;
    newExpr->typeExpr = NULL;
    exp = newExpr;

    whileType = peekToken(parser->lex).type;
  }

  return exp;
}

static Expr *
parseBoolean(Parser *parser) {
  Expr *exp = parseComparison(parser);

  while (peekToken(parser->lex).type == TOK_AND ||
         peekToken(parser->lex).type == TOK_OR) {
    int op = parseBinop(parser);
    Expr *right = parseComparison(parser);
    Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
    newExpr->binop.exp1 = exp;
    newExpr->binop.exp2 = right;
    newExpr->binop.op = op;
    newExpr->typeExpr = NULL;
    exp = newExpr;
  }

  return exp;
}

static Expr *
parseExpr(Parser *parser) {
  return parseBoolean(parser);
}

static Stmt *
parseDec(Parser *parser, Token nameTok, bool isMut) {
  Type *type = parseType(parser);

  lexerStepBack(parser->lex);
  Token prev = nextToken(parser->lex);

  /* Should either be a semicolon or an equals sign */
  Token equalTok = nextToken(parser->lex);

  if (equalTok.type == TOK_EQUAL) {
    Expr *exp = parseExpr(parser);

    lexerStepBack(parser->lex);
    prev = nextToken(parser->lex);

    Token endingTok = nextToken(parser->lex);
    Stmt *stmt;

    if (endingTok.type == TOK_SEMICOLON) {
      stmt = stmtFromTwoPoints(nameTok.start, endingTok.end, STMT_DEC_ASSIGN);
    } else if (endingTok.type == TOK_NEWLINE) {
      stmt = stmtFromTwoPoints(nameTok.start, prev.end, STMT_DEC_ASSIGN);
    } else {
      queueError("Expected ';', '=', or newline after type in a variable "
                 "declaration",
                 endingTok.start, endingTok.end);
      printErrors();
      exit(1);
    }

    HashEntry *entry = addToScope(parser->currentScope, nameTok.sym, isMut);

    if (entry == NULL) {
      queueError(msprintf("Cannot redeclare variable: '%.*s' in "
                          "the same scope",
                          nameTok.sym.len, (char *)nameTok.sym.text),
                 stmt->start, stmt->end);
      /* Must fail */
      printErrors();
      exit(1);
    }

    stmt->dec_assign.type = type;
    stmt->dec_assign.var = entry;
    stmt->dec_assign.value = exp;
    return stmt;
  }

  Stmt *stmt;
  if (equalTok.type == TOK_SEMICOLON) {
    stmt = stmtFromTwoPoints(nameTok.start, equalTok.end, STMT_DEC);
  } else if (equalTok.type == TOK_NEWLINE) {
    stmt = stmtFromTwoPoints(nameTok.start, prev.end, STMT_DEC);
  } else {
    queueError("Expected ';', '=', or newline after type in a variable "
               "declaration",
               nameTok.start, equalTok.end);
    printErrors();
    exit(1);
  }

  stmt = stmtFromTwoPoints(nameTok.start, prev.end, STMT_DEC);

  HashEntry *entry = addToScope(parser->currentScope, nameTok.sym, isMut);

  if (entry == NULL) {
    queueError(msprintf("Cannot redeclare variable: '%.*s' in "
                        "the same scope",
                        nameTok.sym.len, (char *)nameTok.sym.text),
               nameTok.start, nameTok.end);
    /* Must fail */
    printErrors();
  }

  stmt->dec.var = entry;
  stmt->dec.type = type;
  return stmt;
}

static Stmt *
parseInferredDec(Parser *parser, Token varTok, bool isMut) {
  Expr *value = parseExpr(parser);

  Stmt *stmt;

  Token semicolonTok = nextToken(parser->lex);
  if (semicolonTok.type == TOK_SEMICOLON) {
    stmt = stmtFromTwoPoints(varTok.start, semicolonTok.end, STMT_DEC_ASSIGN);
  } else if (semicolonTok.type == TOK_NEWLINE) {
    stmt = stmtFromTwoPoints(varTok.start, value->end, STMT_DEC_ASSIGN);
  } else {
    queueError(
        "Expected ';' or newline after expression in inferred declaration",
        varTok.start, value->end);
    stmt = stmtFromTwoPoints(varTok.start, value->end, STMT_DEC_ASSIGN);
  }

  stmt->dec_assign.type = NULL;
  stmt->dec_assign.value = value;

  HashEntry *entry = addToScope(parser->currentScope, varTok.sym, isMut);

  if (entry == NULL) {
    queueError(msprintf("Cannot redeclare variable: '%.*s' in the same scope",
                        (int)varTok.sym.len, varTok.sym.text),
               stmt->start, stmt->end);
    printErrors();
  }

  stmt->dec_assign.var = entry;

  return stmt;
}

static Symbol
getSymbolLVal(LVal *lval) {

  if (lval->type == LVAL_VAR) {
    return lval->var.sym;
  } else if (lval->type == LVAL_DEREF) {
    return lval->deref.sym;
  } else {
    assert(false);
    exit(1);
  }
}

static LVal *
parseLVal(Parser *parser) {

  Token tok = nextToken(parser->lex);
  switch (tok.type) {
    case TOK_AT:
      {
        Token tok2 = nextToken(parser->lex);
        if (tok2.type != TOK_SYM) {
          queueError("Expected symbol after dereference in l-value\n",
                     tok2.start, tok2.end);
          printErrors();
        }
        LVal *new = calloc(1, sizeof(LVal));
        new->type = LVAL_DEREF;
        new->deref.sym = tok2.sym;

        new->deref.entry = NULL;
        new->start = tok.start;
        new->end = tok2.start;
        return new;
      }
    case TOK_SYM:
      {
        LVal *new = calloc(1, sizeof(LVal));
        new->type = LVAL_VAR;
        new->var.sym = tok.sym;
        new->var.entry = NULL;
        new->start = tok.start;
        new->end = tok.start;
        return new;
      }
    default:
      queueError("Expected '@' or name for left hand side of assignment",
                 tok.start, tok.end);
      printErrors();
      exit(1);
  }
}

static Stmt *
parseCompoundAssign(Parser *parser, LVal *lval, int type) {
  Expr *expr = parseExpr(parser);

  Token finalTok = nextToken(parser->lex);
  if (finalTok.type != TOK_NEWLINE && finalTok.type != TOK_SEMICOLON) {
    queueError("Expected newline or semicolon after compound assignment",
               finalTok.start, finalTok.end);
    printErrors();
  }

  Stmt *ret =
      stmtFromTwoPoints(lval->start, finalTok.end, STMT_COMPOUND_ASSIGN);

  HashEntry *entry = findInScope(parser->currentScope, getSymbolLVal(lval));

  if (entry == NULL) {
    queueError(msprintf("Cannot find variable: '%.*s' in scope",
                        getSymbolLVal(lval).len,
                        (char *)getSymbolLVal(lval).text),
               ret->start, ret->end);
    printErrors();
  }

  if (lval->type == LVAL_VAR) {
    lval->var.entry = entry;
  } else if (lval->type == LVAL_DEREF) {
    lval->deref.entry = entry;
  } else {
    assert(false);
    exit(1);
  }

  ret->compound_assign.lval = lval;
  ret->compound_assign.op = type;
  ret->compound_assign.value = expr;

  return ret;
}

static Stmt *
parseAssignment(Parser *parser) {
  LVal *lval = parseLVal(parser);

  Token equalTok = nextToken(parser->lex);
  switch (equalTok.type) {
    case TOK_PLUS_EQ:
      return parseCompoundAssign(parser, lval, BINOP_ADD);
    case TOK_MINUS_EQ:
      return parseCompoundAssign(parser, lval, BINOP_SUB);
    case TOK_STAR_EQ:
      return parseCompoundAssign(parser, lval, BINOP_MUL);
    case TOK_SLASH_EQ:
      return parseCompoundAssign(parser, lval, BINOP_DIV);
    case TOK_EQUAL:
      break;
    default:
      queueError(
          "Expected '=', '+=', '-=', '*=', or '/=' after l-value in assignment",
          equalTok.start, equalTok.end);
      equalTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
  }

  Expr *value = parseExpr(parser);

  Token semiTok = nextToken(parser->lex);
  if (semiTok.type != TOK_SEMICOLON && semiTok.type != TOK_NEWLINE) {
    queueError(
        msprintf("Expected ';' or newline after expression in assignment"),
        semiTok.start, semiTok.end);
    printErrors();
  }

  Stmt *ret = stmtFromTwoPoints(lval->start, semiTok.end, STMT_ASSIGN);

  HashEntry *entry = findInScope(parser->currentScope, getSymbolLVal(lval));

  if (entry == NULL) {
    queueError(msprintf("Cannot find variable: '%.*s' in scope",
                        getSymbolLVal(lval).len,
                        (char *)getSymbolLVal(lval).text),
               ret->start, ret->end);
    printErrors();
  }

  if (lval->type == LVAL_VAR) {
    lval->var.entry = entry;
  } else if (lval->type == LVAL_DEREF) {
    lval->deref.entry = entry;
  } else {
    assert(false);
    exit(1);
  }

  ret->assign.lval = lval;
  ret->assign.value = value;
  return ret;
}

static Stmt *
parseReturn(Parser *parser, Token firstTok) {
  Token semiTok = peekToken(parser->lex);

  if (semiTok.type == TOK_SEMICOLON) {
    Stmt *stmt = stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
    stmt->returnExp = NULL;
    nextToken(parser->lex);
    return stmt;
  } else if (semiTok.type == TOK_NEWLINE) {
    Stmt *stmt = stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
    stmt->returnExp = NULL;
    nextToken(parser->lex);
    return stmt;
  }

  Expr *returnExp = parseExpr(parser);

  Stmt *stmt;
  semiTok = nextToken(parser->lex);
  if (semiTok.type == TOK_SEMICOLON) {
    stmt = stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
  } else if (semiTok.type == TOK_NEWLINE) {
    stmt = stmtFromTwoPoints(firstTok.start, returnExp->end, STMT_RETURN);
  } else {
    queueError("Expected ';' after return statment", semiTok.start,
               semiTok.end);
    printErrors();
    exit(1);
  }

  stmt->returnExp = returnExp;
  return stmt;
}

static Stmt *
parseStmt(Parser *parser); // Forward declare this for mutal recursion

static Stmt *
parseWhile(Parser *parser) {
  Expr *cond = parseExpr(parser);

  Token doTok = nextToken(parser->lex);
  if (doTok.type != TOK_DO) {
    queueError("Expected keyword 'do' after expression", doTok.start,
               doTok.end);
    doTok = continueUntil(parser->lex, TOK_DO_BITS);
  }
  Vector *block = newVector(sizeof(Stmt *), 0);

  Scope *oldScope = parser->currentScope;

  pushScope(parser);

  while (peekToken(parser->lex).type != TOK_END) {
    Stmt *tempStmt = parseStmt(parser);
    pushVector(block, &tempStmt);
  }

  nextToken(parser->lex);

  Scope *newScope = parser->currentScope;

  parser->currentScope = oldScope;

  Stmt *ret = calloc(1, sizeof(Stmt));
  ret->type = STMT_WHILE;
  ret->while_block.block = block;
  ret->while_block.cond = cond;
  ret->while_block.scope = newScope;

  return ret;
}

static Stmt *
parseIf(Parser *parser) {
  Expr *cond = parseExpr(parser);

  Token thenTok = nextToken(parser->lex);
  if (thenTok.type != TOK_DO) {
    queueError("Expected keyword 'do' after expression", thenTok.start,
               thenTok.end);
    thenTok = continueUntil(parser->lex, TOK_DO_BITS);
  }

  Vector *block1 = newVector(sizeof(Stmt *), 0);

  Scope *oldScope = parser->currentScope;

  pushScope(parser);
  while (peekToken(parser->lex).type != TOK_END &&
         peekToken(parser->lex).type != TOK_ELSE) {
    Stmt *tempStmt = parseStmt(parser);
    pushVector(block1, &tempStmt);
  }

  Scope *scope1 = parser->currentScope;
  parser->currentScope = oldScope;

  Token elseEndTok = nextToken(parser->lex);
  if (elseEndTok.type == TOK_END) {
    Stmt *ret = calloc(1, sizeof(Stmt));
    ret->type = STMT_IF;
    ret->if_block.block = block1;
    ret->if_block.cond = cond;
    ret->if_block.scope = scope1;
    return ret;
  }

  if (elseEndTok.type == TOK_ELSE) {
    Vector *block2 = newVector(sizeof(Stmt *), 0);

    oldScope = parser->currentScope;

    pushScope(parser);
    while (peekToken(parser->lex).type != TOK_END) {
      Stmt *tempStmt = parseStmt(parser);
      pushVector(block2, &tempStmt);
    }

    Scope *scope2 = parser->currentScope;
    parser->currentScope = oldScope;

    // Skip the end token
    nextToken(parser->lex);

    Stmt *ret = calloc(1, sizeof(Stmt));
    ret->type = STMT_IF_ELSE;
    ret->if_else.block1 = block1;
    ret->if_else.block2 = block2;
    ret->if_else.cond = cond;
    ret->if_else.scope1 = scope1;
    ret->if_else.scope2 = scope2;
    return ret;
  }
  return NULL;
}

static Stmt *
parseStmt(Parser *parser) {
  Token tok = peekToken(parser->lex);

  switch (tok.type) {
    case TOK_RETURN:
      nextToken(parser->lex);
      return parseReturn(parser, tok);
    case TOK_AT:
      return parseAssignment(parser);
    case TOK_MUT:
      {
        Token firstTok = nextToken(parser->lex);
        Token symTok = nextToken(parser->lex);
        if (symTok.type != TOK_SYM) {
          queueError("Expected symbol after keyword 'mut' in statement",
                     symTok.start, symTok.end);
          printErrors();
        }

        Token opTok = nextToken(parser->lex);
        switch (opTok.type) {
          case TOK_COLON:
            return parseDec(parser, symTok, true);
          case TOK_COLONEQUAL:
            return parseInferredDec(parser, symTok, true);
          default:
            queueError("Expected ':=' or ':' after keyword 'mut' and symbol",
                       firstTok.start, opTok.start);
            printErrors();
            exit(1);
        }
      }
    case TOK_SYM:
      {
        Token equalsTok = lookaheadToken(parser->lex);
        switch (equalsTok.type) {
          case TOK_PLUS_EQ:
          case TOK_MINUS_EQ:
          case TOK_STAR_EQ:
          case TOK_SLASH_EQ:
          case TOK_EQUAL:
            return parseAssignment(parser);
          case TOK_COLON:
            nextToken(parser->lex);
            nextToken(parser->lex);
            return parseDec(parser, tok, false);
          case TOK_COLONEQUAL:
            nextToken(parser->lex);
            nextToken(parser->lex);
            return parseInferredDec(parser, tok, false);
          default:
            {
              Expr *expr = parseExpr(parser);
              Token semiTok = nextToken(parser->lex);
              if (semiTok.type != TOK_SEMICOLON &&
                  semiTok.type != TOK_NEWLINE) {
                queueError(
                    "Expected ';' or newline after expression parsed as a "
                    "standalone "
                    "statement",
                    expr->start, semiTok.end);
                semiTok = continueUntil(parser->lex, TOK_SEMICOLON_BITS);
              }

              Stmt *exprStmt =
                  stmtFromTwoPoints(expr->start, semiTok.end, STMT_EXPR);
              exprStmt->singleExpr = expr;
              return exprStmt;
            }
        }
      }
    case TOK_IF:
      {
        nextToken(parser->lex);
        return parseIf(parser);
      }
    case TOK_WHILE:
      {
        nextToken(parser->lex);
        return parseWhile(parser);
      }

    default:
      {
        Expr *expr = parseExpr(parser);

        Token semiTok = nextToken(parser->lex);
        if (semiTok.type != TOK_SEMICOLON && semiTok.type != TOK_NEWLINE) {
          queueError("Expected ';' after expression parsed as a standalone "
                     "statement",
                     expr->start, semiTok.end);
          semiTok = continueUntil(parser->lex, TOK_SEMICOLON_BITS);
        }

        Stmt *exprStmt = stmtFromTwoPoints(expr->start, semiTok.end, STMT_EXPR);
        exprStmt->singleExpr = expr;
        return exprStmt;
      }
  }
}

static Param
parseParam(Scope *scope, Parser *parser) {
  Token symTok = nextToken(parser->lex);
  if (symTok.type != TOK_SYM) {
    queueError("Expected parameter name", symTok.start, symTok.end);
    symTok = continueUntil(parser->lex, TOK_SYM_BITS);
  }
  Token colonType = nextToken(parser->lex);
  if (colonType.type != TOK_COLON) {
    queueError("Expected ':' after parameter name", symTok.start, symTok.end);
    symTok = continueUntil(parser->lex, TOK_COLON_BITS);
  }

  Type *type = parseType(parser);

  Token commaType = peekToken(parser->lex);
  if (commaType.type == TOK_COMMA) {
    nextToken(parser->lex);
  }
  TypedEntry *entry = calloc(1, sizeof(TypedEntry));
  entry->isMut = false;
  entry->type = type;

  return (Param){.var = insertHashtbl(scope->vars, symTok.sym, entry),
                 .type = type};
}

static Vector *
parseParams(Scope *scope, Parser *parser) {
  Vector *ret = newVector(sizeof(Param), 0);

  Token lparenTok = nextToken(parser->lex);
  if (lparenTok.type != TOK_LPAREN) {
    queueError("Expected '(' before function paremeters", lparenTok.start,
               lparenTok.end);
    lparenTok = continueUntil(parser->lex, TOK_LPAREN_BITS);
  }

  for (;;) {
    Token tok = peekToken(parser->lex);
    if (tok.type == TOK_RPAREN) {
      nextToken(parser->lex);
      return ret;
    } else {
      Param param = parseParam(scope, parser);
      pushVector(ret, &param);
    }
  }
}

static Function *
parseFunction(Parser *parser, Token keywordTok) {
  Token symTok = nextToken(parser->lex);

  if (symTok.type != TOK_SYM) {
    queueError("Expected variable name after 'proc' keyword", symTok.start,
               symTok.end);
    symTok = continueUntil(parser->lex, TOK_SYM_BITS);
  }

  /* Allocate a new scope for the function */
  parser->currentScope = newScope(parser->currentScope);

  Vector *params = parseParams(parser->currentScope, parser);

  Token arrowTok = peekToken(parser->lex);
  Type *retType;
  if (arrowTok.type == TOK_ARROW) {
    nextToken(parser->lex);

    retType = parseType(parser);
  } else {
    retType = VoidType;
  }

  if (peekToken(parser->lex).type == TOK_NEWLINE) {
    nextToken(parser->lex);
  }

  Vector *paramTypes = newVector(sizeof(Type *), params->numItems);

  for (size_t i = 0; i < params->numItems; i++) {
    Type *paramType = ((Param *)indexVector(params, i))->type;
    pushVector(paramTypes, &paramType);
  }

  Type *functionType = calloc(1, sizeof(Type));
  functionType->type = TYP_FUN;
  functionType->fun.args = paramTypes;
  functionType->fun.retType = retType;

  Vector *stmts = newVector(sizeof(Stmt *), 0);

  Token endTok;
  for (;;) {
    Token tok = peekToken(parser->lex);
    if (tok.type == TOK_END) {
      endTok = nextToken(parser->lex);
      break;
    }
    Stmt *stmt = parseStmt(parser);
    pushVector(stmts, &stmt);
  }

  Function *fun = calloc(1, sizeof(Function));
  fun->name = symTok.sym;
  fun->params = params;
  fun->retType = retType;
  fun->scope = parser->currentScope;
  fun->stmts = stmts;
  fun->start = keywordTok.start;
  fun->end = endTok.end;

  TypedEntry *funEntry = calloc(1, sizeof(TypedEntry));
  funEntry->isMut = false;
  funEntry->type = functionType;
  funEntry->fun = fun;

  insertHashtbl(parser->currentScope->upScope->vars, symTok.sym, funEntry);
  return fun;
}

static void
parseTypeDec(Parser *parser) {
  Token symTok = nextToken(parser->lex);
  if (symTok.type != TOK_SYM) {
    queueError("Expected type name after 'type' keyword", symTok.start,
               symTok.end);
    symTok = continueUntil(parser->lex, TOK_SYM_BITS);
  }

  Token equalsTok = nextToken(parser->lex);
  if (equalsTok.type != TOK_EQUAL) {
    queueError("Expected '=' after name in type declaration", equalsTok.start,
               equalsTok.end);
    equalsTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
  }

  Type *type = parseComplexType(parser);
  HashEntry *entry = insertHashtbl(parser->typeTable, symTok.sym, type);
  if (entry == NULL) {
    queueError(msprintf("Cannot rebind name '%.*s' to a type",
                        (int)symTok.sym.len, symTok.sym.text),
               symTok.start, symTok.end);
  }
}

static Toplevel
parseExternal(Parser *parser) {

  Token symTok = nextToken(parser->lex);
  if (symTok.type != TOK_SYM) {
    queueError("Expected name of external value in external declaration",
               symTok.start, symTok.end);
    printErrors();
  }

  Token colonTok = nextToken(parser->lex);
  if (colonTok.type != TOK_COLON) {
    queueError("Expected ':' after name of external declaration",
               colonTok.start, colonTok.end);
    printErrors();
  }

  Type *type = parseType(parser);

  Token endTok = nextToken(parser->lex);
  if (endTok.type != TOK_NEWLINE && endTok.type != TOK_SEMICOLON) {
    queueError("Expected ';' or newline after external declaration",
               endTok.start, endTok.end);
  }

  TypedEntry *info = calloc(1, sizeof(TypedEntry));
  info->isMut = false;
  info->type = type;

  HashEntry *entry =
      insertHashtbl(parser->currentScope->vars, symTok.sym, info);
  if (entry == NULL) {

    queueError(
        msprintf("Cannot declare variable '%,*s' again in the same scope",
                 symTok.sym.len, symTok.sym.text),
        symTok.start, symTok.end);
    printErrors();
  }

  Toplevel ret;
  ret.type = TOP_EXTERN;
  ret.external.entry = entry;
  ret.external.type = type;
  return ret;
}

static Toplevel
parseToplevel(Parser *parser) {
  Token keywordTok = nextToken(parser->lex);
  switch (keywordTok.type) {
    case TOK_PROC:
      return (Toplevel){.type = TOP_PROC,
                        .fn = parseFunction(parser, keywordTok)};
    case TOK_TYPE:
      parseTypeDec(parser);
      return parseToplevel(parser);
    case TOK_EXTERN:
      return parseExternal(parser);
    default:
      queueError("Unexpected token. NOTE: There are no global vars yet",
                 keywordTok.start, keywordTok.end);
      printErrors();
      exit(1);
  }
}

static Parser
newParser(Lexer *lex) {
  Parser ret;
  ret.lex = lex;
  ret.currentScope = calloc(1, sizeof(Scope));
  ret.currentScope->upScope = NULL;
  ret.currentScope->vars = newHashtbl(SYM_TABLE_INIT_SIZE);
  ret.typeTable = newHashtbl(0);
  return ret;
}

/* Traverses to the top of a lexical scope */
static Scope *
getGlobalScope(Scope *scope) {
  for (; scope->upScope != NULL; scope = scope->upScope)
    ; // NOOP

  return scope;
}

AST *
parseSource(Lexer *lex) {
  Parser parser = newParser(lex);
  Vector *decs = newVector(sizeof(Toplevel), 0);

  for (;;) {
    Token tok = peekToken(parser.lex);

    switch (tok.type) {
      case TOK_EOF:
        /* Im sorry djikstra but no named break is a deal breaker */
        goto done;
      default:
        {
          Toplevel top = parseToplevel(&parser);
          pushVector(decs, &top);
        }
    }
  }
done:
  {
    AST *ret = calloc(1, sizeof(AST));
    ret->decs = decs;
    ret->globalScope = getGlobalScope(parser.currentScope);
    return ret;
  }
}
