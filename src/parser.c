//===----------- parser.c - Parses a source file into an AST -------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Using a lexer, this parses a source file into the AST described in parser.h.
// It is designed to continue for as long as possible after an error until it is
// no longer possible to continue.
// It also performs type checking while parsing, although this will probably be
// given its own phase soon.
//
//===------------------------------ Todo ---------------------------------===//
//
// * Continue parsing for even longer after errors occur
//
//===---------------------------------------------------------------------===//

#include <assert.h>
#include <ctype.h>
#include <error.h>
#include <lexer.h>
#include <parser.h>
#include <pp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>

#define SYM_TABLE_INIT_SIZE 8

/* Holds the current state of the parser */
typedef struct {
    Scope *currentScope;
    Lexer *lex;
    Hashtbl *typeTable;
} Parser;

/* These types dont need to be dynamically allocated */
static UR_Type *VoidType = &(UR_Type){.type = TYP_VOID, {}};
static UR_Type *BooleanType = &(UR_Type){.type = TYP_BOOL, {}};
static UR_Type *S8Type = &(UR_Type){.type = TYP_S8, {}};
static UR_Type *S16Type = &(UR_Type){.type = TYP_S16, {}};
static UR_Type *S32Type = &(UR_Type){.type = TYP_S32, {}};
static UR_Type *S64Type = &(UR_Type){.type = TYP_S64, {}};
static UR_Type *U8Type = &(UR_Type){.type = TYP_U8, {}};
static UR_Type *U16Type = &(UR_Type){.type = TYP_U16, {}};
static UR_Type *U32Type = &(UR_Type){.type = TYP_U32, {}};
static UR_Type *U64Type = &(UR_Type){.type = TYP_U64, {}};

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
};

/* Runs through tokens until a token passed in bitflags is reached, which is
 * then returned */
Token continueUntil(Lexer *lex, int bitFlags) {
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
            (tok.type == TOK_EQUAL && bitFlags & TOK_EQUAL_BITS)) {
            break;
        } else {
            nextToken(lex);
        }
    }
    nextToken(lex);
    return tok;
}

/* Creates a new expression with the same position information as given
 * token */
UR_Expr *exprFromToken(Token tok, enum ExprType type) {
    UR_Expr *exp = calloc(1, sizeof(UR_Expr));
    exp->type = type;
    exp->start = tok.start;
    exp->end = tok.end;
    return exp;
}

UR_Expr *exprFromTwoPoints(size_t start, size_t end, enum ExprType type) {
    UR_Expr *exp = calloc(1, sizeof(UR_Expr));
    exp->type = type;
    exp->start = start;
    exp->end = end;
    return exp;
}

UR_Stmt *stmtFromTwoPoints(size_t start, size_t end, enum StmtType type) {
    UR_Stmt *ret = calloc(1, sizeof(UR_Stmt));
    ret->start = start;
    ret->end = end;
    ret->type = type;
    return ret;
}

static HashEntry *addToScope(Scope *scope, Symbol sym, bool isMut) {
    UnresolvedEntry *entry = calloc(1, sizeof(UnresolvedEntry));
    entry->isMut = isMut;
    entry->type = NULL;
    return insertHashtbl(scope->vars, sym, entry);
}

/* This is a bit janky, but the idea is that given a token, this returns an
 * integer of the number of bytes the type uses if it fits the form x[integer]
 * where x can be any character. It's used to find the bit size of builtin
 * integer types */
int64_t convertSymbolInt(Token tok) {
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

    return ret / 8;
}

UR_Type *parseType(Parser *parser) {
    Token tok = nextToken(parser->lex);
    if (tok.type != TOK_SYM && tok.type != TOK_VOID && tok.type != TOK_BOOL) {
        queueError(msprintf("Unexpected token, expected type to be a "
                            "single symbol, arrays "
                            "and pointers are not supported"),
                   tok.start, tok.end);
        tok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    switch (tok.type) {
        case TOK_SYM:
            switch (tok.sym.text[0]) {
                case 's':
                    switch (convertSymbolInt(tok)) {
                        case 1:
                            return S8Type;
                        case 2:
                            return S16Type;
                        case 4:
                            return S32Type;
                        case 8:
                            return S64Type;
                        default:
                            printf("%ld\n", convertSymbolInt(tok));
                            queueError(
                                "Signed integer types can only be 8, 16, 32, "
                                "or 64 bits",
                                tok.start, tok.end);
                            printErrors();
                            exit(1);
                    }
                case 'u':
                    switch (convertSymbolInt(tok)) {
                        case 1:
                            return U8Type;
                        case 2:
                            return U16Type;
                        case 4:
                            return U32Type;
                        case 8:
                            return U64Type;
                        default:
                            queueError(
                                "Unisigned integer types can only be 8, 16, "
                                "32, "
                                "or 64 bits",
                                tok.start, tok.end);
                            printErrors();
                            exit(1);
                    }
                default: {
                    UR_Type *ret = calloc(1, sizeof(UR_Type));
                    ret->type = TYP_BINDING;
                    ret->bindingName = tok.sym;
                    return ret;
                }
            }
            break;
        case TOK_VOID:
            return VoidType;
        case TOK_BOOL:
            return BooleanType;

        default:
            assert(false);
            return NULL;
    }
}

/* Includes all the above types, plus records and other types which are not
 * allowed except in type declarations */
UR_Type *parseComplexType(Parser *parser) {
    Token firstTok = peekToken(parser->lex);

    if (firstTok.type == TOK_RECORD) {
        nextToken(parser->lex);
        Hashtbl *recordFields = newHashtbl(0);
        Vector *vec = newVector(sizeof(HashEntry *), 0);

        while (peekToken(parser->lex).type != TOK_END) {
            Token symTok = nextToken(parser->lex);
            if (symTok.type != TOK_SYM) {
                queueError("Expected name of record field", symTok.start,
                           symTok.end);
                symTok = continueUntil(parser->lex, TOK_SYM_BITS);
            }

            Token colonTok = nextToken(parser->lex);
            if (colonTok.type != TOK_COLON) {
                queueError("Expected ':' after name of record field",
                           colonTok.start, colonTok.end);
                colonTok = continueUntil(parser->lex, TOK_COLON_BITS);
            }

            UR_Type *fieldType = parseType(parser);

            Token commaToken = peekToken(parser->lex);
            if (commaToken.type == TOK_COMMA) {
                nextToken(parser->lex);
            }

            HashEntry *entry =
                insertHashtbl(recordFields, symTok.sym, fieldType);

            pushVector(vec, &entry);

            if (entry == NULL) {
                queueError(msprintf("Cannot redeclare record field '%.*s'",
                                    (int)symTok.sym.len, symTok.sym.text),
                           symTok.start, symTok.end);
            }
        }

        // Skip over the end token
        nextToken(parser->lex);

        UR_Type *type = calloc(1, sizeof(UR_Type));
        type->type = TYP_RECORD;
        type->record.recordFields = recordFields;
        type->record.vec = vec;
        return type;
    }
    return parseType(parser);
}

UR_Expr *parseExpr(Parser *parser);

UR_Expr *parseFuncall(Parser *parser, Token symTok) {
    Vector *args = newVector(sizeof(UR_Expr *), 0);

    while (peekToken(parser->lex).type != TOK_RPAREN) {
        UR_Expr *expr = parseExpr(parser);
        pushVector(args, &expr);
    }

    Token endTok = nextToken(parser->lex);

    UR_Expr *funcall = calloc(1, sizeof(UR_Expr));
    funcall->type = EXP_FUNCALL;
    funcall->funcall.arguments = args;
    funcall->funcall.name = symTok.sym;
    funcall->start = symTok.start;
    funcall->end = endTok.end;
    return funcall;
}

UR_Expr *parseRecordLit(Parser *parser, Token symTok) {
    HashEntry *entry = findHashtbl(parser->typeTable, symTok.sym);

    if (entry == NULL) {
        queueError(msprintf("Could not find a type binding for name '%.*s'",
                            (int)symTok.sym.len, symTok.sym.text),
                   symTok.start, symTok.end);
    }

    Hashtbl *recordLitFields = newHashtbl(0);

    while (peekToken(parser->lex).type != TOK_RBRACKET) {
        Token symTok = nextToken(parser->lex);
        if (symTok.type != TOK_SYM) {
            queueError("Expected name of record field", symTok.start,
                       symTok.end);
            symTok = continueUntil(parser->lex, TOK_SYM_BITS);
        }

        Token colonTok = nextToken(parser->lex);
        if (colonTok.type != TOK_EQUAL) {
            queueError("Expected ':' after name of record field",
                       colonTok.start, colonTok.end);
            colonTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
        }

        UR_Expr *fieldExpr = parseExpr(parser);

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

    UR_Expr *ret =
        exprFromTwoPoints(symTok.start, rparenToken.end, EXP_RECORDLIT);
    ret->reclit.fields = recordLitFields;
    ret->reclit.recordName = symTok.sym;
    return ret;
}

UR_Expr *parsePrimary(Parser *parser) {
    UR_Expr *ret;
    Token tok = nextToken(parser->lex);
    switch (tok.type) {
        case TOK_INT:
            ret = exprFromToken(tok, EXP_INT);
            ret->intlit = tok.intnum;
            return ret;
        case TOK_SYM:

            switch (peekToken(parser->lex).type) {
                case TOK_LPAREN:
                    nextToken(parser->lex);
                    return parseFuncall(parser, tok);
                case TOK_LBRACKET:
                    nextToken(parser->lex);
                    return parseRecordLit(parser, tok);

                default:
                    ret = exprFromToken(tok, EXP_VAR);

                    ret->var = tok.sym;
                    return ret;
            }
        case TOK_FALSE:
            ret = exprFromToken(tok, EXP_BOOL);
            ret->boolean = false;
            return ret;
        case TOK_TRUE:
            ret = exprFromToken(tok, EXP_BOOL);
            ret->boolean = true;
            return ret;

        default:
            printToken(tok);
            printToken(nextToken(parser->lex));
            queueError(msprintf("Expected an integer or variable name for "
                                "expressions, not another token"),
                       tok.start, tok.end);
            printErrors();
            /* This never gets called */ exit(1);
    }
}

int parseBinop(Parser *parser) {
    Token tok = nextToken(parser->lex);
    switch (tok.type) {
        case TOK_PLUS:
            return BINOP_ADD;
        case TOK_MINUS:
            return BINOP_SUB;
        case TOK_STAR:
            return BINOP_MULT;
        case TOK_SLASH:
            return BINOP_DIV;
        case TOK_DOUBLEEQUAL:
            return BINOP_EQUAL;
        case TOK_AND:
            return BINOP_AND;
        case TOK_OR:
            return BINOP_OR;
        default:
            queueError(msprintf("Expected arithmetic operation"), tok.start,
                       tok.end);
            /* This doesn't need to fail but idc */
            printErrors();
            exit(1);
    }
}

UR_Expr *parseFactor(Parser *parser) {
    UR_Expr *exp = parsePrimary(parser);

    while (peekToken(parser->lex).type == TOK_STAR ||
           peekToken(parser->lex).type == TOK_SLASH) {
        int op = parseBinop(parser);
        UR_Expr *right = parsePrimary(parser);
        UR_Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
        newExpr->binop.exp1 = exp;
        newExpr->binop.exp2 = right;
        newExpr->binop.op = op;
        exp = newExpr;
    }

    return exp;
}

UR_Expr *parseTerm(Parser *parser) {
    UR_Expr *exp = parseFactor(parser);

    while (peekToken(parser->lex).type == TOK_PLUS ||
           peekToken(parser->lex).type == TOK_MINUS) {
        int op = parseBinop(parser);
        UR_Expr *right = parseFactor(parser);
        UR_Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
        newExpr->binop.exp1 = exp;
        newExpr->binop.exp2 = right;
        newExpr->binop.op = op;
        exp = newExpr;
    }

    return exp;
}

UR_Expr *parseComparison(Parser *parser) {
    UR_Expr *exp = parseTerm(parser);

    while (peekToken(parser->lex).type == TOK_DOUBLEEQUAL) {
        int op = parseBinop(parser);
        UR_Expr *right = parseTerm(parser);
        UR_Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
        newExpr->binop.exp1 = exp;
        newExpr->binop.exp2 = right;
        newExpr->binop.op = op;

        exp = newExpr;
    }

    return exp;
}

UR_Expr *parseBoolean(Parser *parser) {
    UR_Expr *exp = parseComparison(parser);

    while (peekToken(parser->lex).type == TOK_AND ||
           peekToken(parser->lex).type == TOK_OR) {
        int op = parseBinop(parser);
        UR_Expr *right = parseComparison(parser);
        UR_Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
        newExpr->binop.exp1 = exp;
        newExpr->binop.exp2 = right;
        newExpr->binop.op = op;
        exp = newExpr;
    }

    return exp;
}

UR_Expr *parseExpr(Parser *parser) { return parseBoolean(parser); }

static UR_Stmt *parseDec(Parser *parser, Token nameTok, bool isMut) {
    UR_Type *type = parseType(parser);

    Token prev = parser->lex->previousTok;

    /* Should either be a semicolon or an equals sign */
    Token equalTok = nextToken(parser->lex);

    if (equalTok.type == TOK_EQUAL) {
        UR_Expr *exp = parseExpr(parser);

        prev = parser->lex->previousTok;

        Token endingTok = nextToken(parser->lex);
        UR_Stmt *stmt;

        if (endingTok.type == TOK_SEMICOLON) {
            stmt = stmtFromTwoPoints(nameTok.start, endingTok.end,
                                     STMT_DEC_ASSIGN);
        } else if (endingTok.type == TOK_NEWLINE) {
            stmt = stmtFromTwoPoints(nameTok.start, prev.end, STMT_DEC_ASSIGN);
        } else {
            queueError(
                "Expected ';', '=', or newline after type in a variable "
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
        stmt->dec_assign.entry = entry;
        stmt->dec_assign.value = exp;
        return stmt;
    }

    UR_Stmt *stmt;
    if (equalTok.type == TOK_SEMICOLON) {
        stmt = stmtFromTwoPoints(nameTok.start, equalTok.end, STMT_DEC);
    } else if (equalTok.type == TOK_NEWLINE) {
        stmt = stmtFromTwoPoints(nameTok.start, prev.end, STMT_DEC);
    } else {
        queueError(
            "Expected ';', '=', or newline after type in a variable "
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

    stmt->dec.entry = entry;
    return stmt;
}

UR_Stmt *parseInferredDec(Parser *parser, Token varTok, bool isMut) {
    UR_Expr *value = parseExpr(parser);

    UR_Stmt *stmt;

    Token semicolonTok = nextToken(parser->lex);
    if (semicolonTok.type == TOK_SEMICOLON) {
        stmt =
            stmtFromTwoPoints(varTok.start, semicolonTok.end, STMT_DEC_ASSIGN);
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
        queueError(
            msprintf("Cannot redeclare variable: '%.*s' in the same scope",
                     (int)varTok.sym.len, varTok.sym.text),
            stmt->start, stmt->end);
        printErrors();
    }

    stmt->dec_assign.entry = entry;

    return stmt;
}

UR_Stmt *parseAssignment(Parser *parser, Token symTok) {
    UR_Expr *value = parseExpr(parser);

    Token semiTok = nextToken(parser->lex);

    UR_Stmt *ret;
    if (semiTok.type == TOK_SEMICOLON) {
        ret = stmtFromTwoPoints(symTok.start, semiTok.end, STMT_ASSIGN);
    } else if (semiTok.type == TOK_NEWLINE) {
        ret = stmtFromTwoPoints(symTok.start, value->end, STMT_ASSIGN);
    } else {
        queueError(msprintf("Expected ';' after expression in assignment"),
                   semiTok.start, semiTok.end);
        printErrors();
        exit(1);
    }

    ret->assign.name = symTok.sym;
    ret->assign.value = value;
    return ret;
}

UR_Stmt *parseReturn(Parser *parser, Token firstTok) {
    Token semiTok = peekToken(parser->lex);

    if (semiTok.type == TOK_SEMICOLON) {
        UR_Stmt *stmt =
            stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
        stmt->returnExp = NULL;
        nextToken(parser->lex);
        return stmt;
    } else if (semiTok.type == TOK_NEWLINE) {
        UR_Stmt *stmt =
            stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
        stmt->returnExp = NULL;
        nextToken(parser->lex);
        return stmt;
    }

    UR_Expr *returnExp = parseExpr(parser);

    UR_Stmt *stmt;
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

UR_Stmt *parseStmt(Parser *parser) {
    Token tok = peekToken(parser->lex);

    switch (tok.type) {
        case TOK_RETURN:
            nextToken(parser->lex);
            return parseReturn(parser, tok);
        case TOK_SYM: {
            Token equalsTok = lookaheadToken(parser->lex);
            switch (equalsTok.type) {
                case TOK_EQUAL:
                    nextToken(parser->lex);
                    nextToken(parser->lex);
                    return parseAssignment(parser, tok);
                case TOK_COLON:
                    nextToken(parser->lex);
                    nextToken(parser->lex);
                    return parseDec(parser, tok, true);
                case TOK_COLONEQUAL:
                    nextToken(parser->lex);
                    nextToken(parser->lex);
                    return parseInferredDec(parser, tok, true);
                default: {
                    UR_Expr *expr = parseExpr(parser);
                    Token semiTok = nextToken(parser->lex);
                    if (semiTok.type != TOK_SEMICOLON) {
                        queueError(
                            "Expected ';' after expression parsed as a "
                            "standalone "
                            "statement",
                            expr->start, semiTok.end);
                        semiTok =
                            continueUntil(parser->lex, TOK_SEMICOLON_BITS);
                    }

                    UR_Stmt *exprStmt =
                        stmtFromTwoPoints(expr->start, semiTok.end, STMT_EXPR);
                    exprStmt->singleExpr = expr;
                    return exprStmt;
                }
            }
        }
        default: {
            UR_Expr *expr = parseExpr(parser);

            Token semiTok = nextToken(parser->lex);
            if (semiTok.type != TOK_SEMICOLON) {
                queueError(
                    "Expected ';' after expression parsed as a standalone "
                    "statement",
                    expr->start, semiTok.end);
                semiTok = continueUntil(parser->lex, TOK_SEMICOLON_BITS);
            }

            UR_Stmt *exprStmt =
                stmtFromTwoPoints(expr->start, semiTok.end, STMT_EXPR);
            exprStmt->singleExpr = expr;
            return exprStmt;
        }
    }
}

UR_Param parseParam(Scope *scope, Parser *parser) {
    Token symTok = nextToken(parser->lex);
    if (symTok.type != TOK_SYM) {
        queueError("Expected parameter name", symTok.start, symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }
    Token colonType = nextToken(parser->lex);
    if (colonType.type != TOK_COLON) {
        queueError("Expected ':' after parameter name", symTok.start,
                   symTok.end);
        symTok = continueUntil(parser->lex, TOK_COLON_BITS);
    }

    UR_Type *type = parseType(parser);

    Token commaType = peekToken(parser->lex);
    if (commaType.type == TOK_COMMA) {
        nextToken(parser->lex);
    }

    UnresolvedEntry *entry = calloc(1, sizeof(UnresolvedEntry));
    entry->isMut = false;
    entry->type = type;

    return (UR_Param){.entry = insertHashtbl(scope->vars, symTok.sym, entry),
                      .type = type};
}

Vector *parseParams(Scope *scope, Parser *parser) {
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
            UR_Param param = parseParam(scope, parser);
            pushVector(ret, &param);
        }
    }
}

UR_Function *parseFunction(Parser *parser, Token keywordTok) {
    Token symTok = nextToken(parser->lex);

    if (symTok.type != TOK_SYM) {
        queueError("Expected variable name after 'proc' keyword", symTok.start,
                   symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    /* Allocate a new scope for the function */
    Scope *newScope = calloc(1, sizeof(Scope));
    newScope->upScope = parser->currentScope;
    newScope->vars = newHashtbl(0);
    parser->currentScope = newScope;

    Vector *params = parseParams(parser->currentScope, parser);

    Token arrowTok = nextToken(parser->lex);
    if (arrowTok.type != TOK_ARROW) {
        queueError("Expected '->' after function parameters", arrowTok.start,
                   arrowTok.end);
        arrowTok = continueUntil(parser->lex, TOK_ARROW_BITS);
    }

    UR_Type *retType = parseType(parser);

    if (peekToken(parser->lex).type == TOK_NEWLINE) {
        nextToken(parser->lex);
    }

    Vector *paramTypes = newVector(sizeof(UR_Type *), params->numItems);

    for (size_t i = 0; i < params->numItems; i++) {
        UR_Type *paramType = ((UR_Param *)indexVector(params, i))->type;
        pushVector(paramTypes, &paramType);
    }

    UR_Type *functionType = calloc(1, sizeof(UR_Type));
    functionType->type = TYP_FUN;
    functionType->fun.args = paramTypes;

    functionType->fun.retType = retType;

    UnresolvedEntry *funEntry = calloc(1, sizeof(UnresolvedEntry));
    funEntry->isMut = false;
    funEntry->type = functionType;

    insertHashtbl(parser->currentScope->vars, symTok.sym, funEntry);

    Vector *stmts = newVector(sizeof(UR_Stmt *), 0);

    Token endTok;
    for (;;) {
        Token tok = peekToken(parser->lex);
        if (tok.type == TOK_END) {
            endTok = nextToken(parser->lex);
            break;
        }
        UR_Stmt *stmt = parseStmt(parser);
        pushVector(stmts, &stmt);
    }

    UR_Function *fun = calloc(1, sizeof(UR_Function));
    fun->name = symTok.sym;
    fun->params = params;
    fun->retType = retType;
    fun->scope = parser->currentScope;
    fun->stmts = stmts;
    fun->start = keywordTok.start;
    fun->end = endTok.end;

    return fun;
}

void parseTypeDec(Parser *parser) {
    Token symTok = nextToken(parser->lex);
    if (symTok.type != TOK_SYM) {
        queueError("Expected type name after 'type' keyword", symTok.start,
                   symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    Token equalsTok = nextToken(parser->lex);
    if (equalsTok.type != TOK_EQUAL) {
        queueError("Expected '=' after name in type declaration",
                   equalsTok.start, equalsTok.end);
        equalsTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
    }

    UR_Type *type = parseComplexType(parser);
    HashEntry *entry = insertHashtbl(parser->typeTable, symTok.sym, type);
    if (entry == NULL) {
        queueError(msprintf("Cannot rebind name '%.*s' to a type",
                            (int)symTok.sym.len, symTok.sym.text),
                   symTok.start, symTok.end);
    }
}

UR_Toplevel parseToplevel(Parser *parser) {
    Token keywordTok = nextToken(parser->lex);
    switch (keywordTok.type) {
        case TOK_PROC:
            return (UR_Toplevel){.type = TOP_PROC,
                                 .fn = parseFunction(parser, keywordTok)};
        case TOK_TYPE:
            parseTypeDec(parser);
            return parseToplevel(parser);
        default:
            printToken(keywordTok);
            printf("Internal compiler error: No global vars yet. 1\n");
            exit(1);
    }
}

static Parser newParser(Lexer *lex) {
    Parser ret;
    ret.lex = lex;
    ret.currentScope = calloc(1, sizeof(Scope));
    ret.currentScope->upScope = NULL;
    ret.currentScope->vars = newHashtbl(SYM_TABLE_INIT_SIZE);
    ret.typeTable = newHashtbl(0);
    return ret;
}

/* Traverses to the top of a lexical scope */
static Scope *getGlobalScope(Scope *scope) {
    for (; scope->upScope != NULL; scope = scope->upScope)
        ;  // NOOP

    return scope;
}

UR_AST *parseSource(Lexer *lex) {
    Parser parser = newParser(lex);
    Vector *decs = newVector(sizeof(UR_Toplevel), 0);

    for (;;) {
        Token tok = peekToken(parser.lex);

        switch (tok.type) {
            case TOK_EOF:
                /* Im sorry djikstra but no named break is a deal breaker */
                goto done;
            default: {
                UR_Toplevel top = parseToplevel(&parser);
                pushVector(decs, &top);
            }
        }
    }
done : {
    UR_AST *ret = calloc(1, sizeof(UR_AST));
    ret->decs = decs;
    ret->globalScope = getGlobalScope(parser.currentScope);
    return ret;
}
}
