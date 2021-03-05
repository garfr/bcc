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

static Type *VoidType = &(Type){.type = TYP_VOID, {}};

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
Expr *exprFromToken(Token tok, enum ExprType type) {
    Expr *exp = malloc(sizeof(Expr));
    exp->type = type;
    exp->start = tok.start;
    exp->end = tok.end;
    return exp;
}

Expr *exprFromTwoPoints(size_t start, size_t end, enum ExprType type) {
    Expr *exp = malloc(sizeof(Expr));
    exp->type = type;
    exp->start = start;
    exp->end = end;
    return exp;
}

Stmt *stmtFromTwoPoints(size_t start, size_t end, enum StmtType type) {
    Stmt *ret = malloc(sizeof(Stmt));
    ret->start = start;
    ret->end = end;
    ret->type = type;
    return ret;
}

static HashEntry *addToScope(Scope *scope, Symbol sym, bool isMut) {
    TypedEntry *entry = calloc(1, sizeof(TypedEntry));
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

Type *parseType(Parser *parser) {
    Type *ret;

    Token tok = nextToken(parser->lex);
    if (tok.type != TOK_SYM && tok.type != TOK_VOID) {
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
                    ret = malloc(sizeof(Type));
                    ret->type = TYP_SINT;
                    ret->intsize = convertSymbolInt(tok);
                    return ret;
                case 'u':
                    ret = malloc(sizeof(Type));
                    ret->type = TYP_UINT;
                    ret->intsize = convertSymbolInt(tok);
                    return ret;
                default: {
                    HashEntry *entry = findHashtbl(parser->typeTable, tok.sym);
                    if (entry == NULL) {
                        queueError(
                            msprintf("Could not find a type with name %.*s",
                                     (int)tok.sym.len, tok.sym.text),
                            tok.start, tok.end);
                        printErrors();
                    }
                    ret = malloc(sizeof(Type));
                    ret->type = TYP_BINDING;
                    ret->typeEntry = entry;
                    return ret;
                }
            }
            break;
        case TOK_VOID:
            return VoidType;

        default:
            assert(false);
            return NULL;
    }
}

/* Includes all the above types, plus records and such */
Type *parseComplexType(Parser *parser) {
    Token firstTok = peekToken(parser->lex);

    if (firstTok.type == TOK_RECORD) {
        nextToken(parser->lex);
        Hashtbl *recordFields = newHashtbl(0);

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

            Type *fieldType = parseType(parser);

            Token commaToken = peekToken(parser->lex);
            if (commaToken.type == TOK_COMMA) {
                nextToken(parser->lex);
            }

            if (insertHashtbl(recordFields, symTok.sym, fieldType) == NULL) {
                queueError(msprintf("Cannot redeclare record field '%.*s'",
                                    (int)symTok.sym.len, symTok.sym.text),
                           symTok.start, symTok.end);
            }
        }

        // Skip over the end token
        nextToken(parser->lex);

        Type *type = malloc(sizeof(Type));
        type->type = TYP_RECORD;
        type->recordFields = recordFields;
        return type;
    }
    return parseType(parser);
}

Expr *parseExpr(Parser *parser);

Expr *parseFuncall(Parser *parser, Token symTok) {
    HashEntry *value = findInScope(parser->currentScope, symTok.sym);
    if (value == NULL) {
        queueError(msprintf("Cannot find variable '%.*s' in scope",
                            symTok.sym.len, symTok.sym.text),
                   symTok.start, symTok.end);
        printErrors();
    }

    Vector *args = newVector(sizeof(Expr *), 0);

    while (peekToken(parser->lex).type != TOK_RPAREN) {
        Expr *expr = parseExpr(parser);
        pushVector(args, &expr);
    }

    Token endTok = nextToken(parser->lex);

    Expr *funcall = malloc(sizeof(Expr));
    funcall->type = EXP_FUNCALL;
    funcall->funcall.arguments = args;
    funcall->funcall.name = value;
    funcall->start = symTok.start;
    funcall->typeExpr = ((TypedEntry *)value->data)->type->fun.retType;
    funcall->end = endTok.end;
    return funcall;
}

Expr *parseRecordLit(Parser *parser, Token symTok) {
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

        Token equalsTok = nextToken(parser->lex);
        if (equalsTok.type != TOK_EQUAL) {
            queueError("Expected ':' after name of record field",
                       equalsTok.start, equalsTok.end);
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

Expr *parsePrimary(Parser *parser) {
    Expr *ret;

    Token tok = nextToken(parser->lex);
    switch (tok.type) {
        case TOK_INT:
            ret = exprFromToken(tok, EXP_INT);
            ret->intlit = tok.intnum;
            ret->typeExpr = NULL;
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

                    HashEntry *entry =
                        findInScope(parser->currentScope, tok.sym);
                    if (entry == NULL) {
                        queueError(
                            msprintf("Cannot find variable: '%.*s' in any "
                                     "scope. Must be undeclared",
                                     tok.sym.len, (char *)tok.sym.text),
                            tok.start, tok.end);
                        /* Must fail */
                        printErrors();
                    }

                    ret->var = entry;
                    ret->typeExpr = NULL;
                    return ret;
            }

        default:
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
        default:
            queueError(msprintf("Expected arithmetic operation"), tok.start,
                       tok.end);
            /* This doesn't need to fail but idc */
            printErrors();
            exit(1);
    }
}

Expr *parseFactor(Parser *parser) {
    Expr *exp = parsePrimary(parser);

    while (peekToken(parser->lex).type == TOK_STAR ||
           peekToken(parser->lex).type == TOK_SLASH) {
        int op = parseBinop(parser);
        Expr *right = parsePrimary(parser);
        Expr *newExpr = exprFromTwoPoints(exp->start, right->end, EXP_BINOP);
        newExpr->binop.exp1 = exp;
        newExpr->binop.exp2 = right;
        newExpr->binop.op = op;
        newExpr->typeExpr = NULL;
        exp = newExpr;
    }

    return exp;
}

Expr *parseTerm(Parser *parser) {
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

Expr *parseExpr(Parser *parser) { return parseTerm(parser); }

static Stmt *parseDec(Parser *parser, Token varTok, bool isMut) {
    Stmt *retStmt;
    Token nameTok = nextToken(parser->lex);
    if (nameTok.type != TOK_SYM) {
        queueError(msprintf("Expected variable name after keyword 'var'"),
                   nameTok.start, nameTok.end);
        nameTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    /* Expect a colon or an equals size */
    Token colonEqualTok = peekToken(parser->lex);
    if (colonEqualTok.type != TOK_COLON && colonEqualTok.type != TOK_EQUAL) {
        queueError(
            msprintf("Expected ':' or '=' after variable name and before "
                     "variable type in variable declaration, not "),
            colonEqualTok.start, colonEqualTok.end);
        colonEqualTok =
            continueUntil(parser->lex, TOK_COLON_BITS | TOK_EQUAL_BITS);
    }

    Type *type;

    switch (colonEqualTok.type) {
        case TOK_COLON:
            nextToken(parser->lex);
            type = parseType(parser);
            if (type == VoidType) {
                queueError("Cannot declare a variable with type void",
                           nameTok.start, nameTok.end);
                printErrors();
            }
            break;
        case TOK_EQUAL:
            /* The type will be inferred by the following expression */
            type = NULL;
            break;
        default:
            // This is unreachable, given the if statement above
            exit(1);
    }

    /* Should either be a semicolon or an equals sign */
    Token semicolonOrEqual = nextToken(parser->lex);

    if (semicolonOrEqual.type != TOK_SEMICOLON &&
        semicolonOrEqual.type != TOK_EQUAL) {
        printToken(semicolonOrEqual);
        queueError(msprintf("Expected '=' or ';' after declaring the "
                            "type and name of a variable"),
                   semicolonOrEqual.start, semicolonOrEqual.end);
        semicolonOrEqual =
            continueUntil(parser->lex, TOK_SEMICOLON_BITS | TOK_EQUAL_BITS);
    }

    switch (semicolonOrEqual.type) {
        /* Just a variable declaration */
        case TOK_SEMICOLON:
            if (type == NULL) {
                queueError("Cannot infer type when no value is given",
                           varTok.start, semicolonOrEqual.end);
                printErrors();
            }
            retStmt =
                stmtFromTwoPoints(varTok.start, semicolonOrEqual.end, STMT_DEC);

            HashEntry *entry =
                addToScope(parser->currentScope, nameTok.sym, isMut);

            if (entry == NULL) {
                queueError(msprintf("Cannot redeclare variable: '%.*s' in "
                                    "the same scope",
                                    nameTok.sym.len, (char *)nameTok.sym.text),
                           retStmt->start, retStmt->end);
                /* Must fail */
                printErrors();
            }

            retStmt->dec.var = entry;
            return retStmt;

        /* Compound variable declaration and assignment */
        case TOK_EQUAL: {
            Expr *exp = parseExpr(parser);

            Token semicolonTok = nextToken(parser->lex);
            if (semicolonTok.type != TOK_SEMICOLON) {
                queueError(msprintf("Expected ';' after expression.\n"),
                           semicolonTok.start, semicolonTok.end);
                printErrors();
                exit(1);
            }
            Stmt *stmt = stmtFromTwoPoints(varTok.start, semicolonTok.end,
                                           STMT_DEC_ASSIGN);

            HashEntry *entry =
                addToScope(parser->currentScope, nameTok.sym, isMut);

            if (entry == NULL) {
                printf("ERROR COCCURED\n");
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
            default:
                /* Unreachable */
                exit(1);
        }
    }
}

Stmt *parseAssignment(Parser *parser, Token symTok) {
    Token equalTok = nextToken(parser->lex);
    if (equalTok.type != TOK_EQUAL) {
        queueError(msprintf("Expected '=' after variable name in assignment"),
                   equalTok.start, equalTok.end);
        equalTok = continueUntil(parser->lex, TOK_EQUAL_BITS);
    }
    Expr *value = parseExpr(parser);

    Token semiTok = nextToken(parser->lex);
    if (semiTok.type != TOK_SEMICOLON) {
        queueError(msprintf("Expected ';' after expression in assignment"),
                   semiTok.start, semiTok.end);
        printErrors();
    }
    Stmt *ret = stmtFromTwoPoints(symTok.start, semiTok.end, STMT_ASSIGN);

    HashEntry *entry = findInScope(parser->currentScope, symTok.sym);

    if (entry == NULL) {
        queueError(msprintf("Cannot find variable: '%.*s' in scope",
                            symTok.sym.len, (char *)symTok.sym.text),
                   ret->start, ret->end);
        printErrors();
    }

    ret->assign.var = entry;
    ret->assign.value = value;
    return ret;
}

Stmt *parseReturn(Parser *parser, Token firstTok) {
    Token semiTok = peekToken(parser->lex);
    if (semiTok.type == TOK_SEMICOLON) {
        Stmt *stmt =
            stmtFromTwoPoints(firstTok.start, semiTok.end, STMT_RETURN);
        stmt->returnExp = NULL;
        nextToken(parser->lex);
        return stmt;
    }

    Expr *returnExp = parseExpr(parser);

    semiTok = nextToken(parser->lex);
    if (semiTok.type != TOK_SEMICOLON) {
        queueError("Expected ';' after return statment", semiTok.start,
                   semiTok.end);
        semiTok = continueUntil(parser->lex, TOK_SEMICOLON_BITS);
    }

    Stmt *stmt = stmtFromTwoPoints(firstTok.start, returnExp->end, STMT_RETURN);
    stmt->returnExp = returnExp;
    return stmt;
}

Stmt *parseStmt(Parser *parser) {
    Token tok = peekToken(parser->lex);

    switch (tok.type) {
        case TOK_LET:
            nextToken(parser->lex);
            return parseDec(parser, tok, false);
        case TOK_MUT:
            nextToken(parser->lex);
            return parseDec(parser, tok, true);
        case TOK_RETURN:
            nextToken(parser->lex);
            return parseReturn(parser, tok);
        case TOK_SYM: {
            Token equalsTok = lookaheadToken(parser->lex);
            if (equalsTok.type == TOK_EQUAL) {
                nextToken(parser->lex);
                return parseAssignment(parser, tok);
            } else {
                Expr *expr = parseExpr(parser);
                Token semiTok = nextToken(parser->lex);
                if (semiTok.type != TOK_SEMICOLON) {
                    queueError(
                        "Expected ';' after expression parsed as a standalone "
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
        default: {
            Expr *expr = parseExpr(parser);

            Token semiTok = nextToken(parser->lex);
            if (semiTok.type != TOK_SEMICOLON) {
                queueError(
                    "Expected ';' after expression parsed as a standalone "
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

Param parseParam(Scope *scope, Parser *parser) {
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

    Type *type = parseType(parser);

    Token commaType = peekToken(parser->lex);
    if (commaType.type == TOK_COMMA) {
        nextToken(parser->lex);
    }
    TypedEntry *entry = malloc(sizeof(TypedEntry));
    entry->isMut = false;
    entry->type = type;

    return (Param){.var = insertHashtbl(scope->vars, symTok.sym, entry),
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
            Param param = parseParam(scope, parser);
            pushVector(ret, &param);
        }
    }
}

Function *parseFunction(Parser *parser, Token keywordTok) {
    Token symTok = nextToken(parser->lex);

    if (symTok.type != TOK_SYM) {
        queueError("Expected variable name after 'proc' keyword", symTok.start,
                   symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    /* Allocate a new scope for the function */
    Scope *newScope = malloc(sizeof(Scope));
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

    Type *retType = parseType(parser);

    Vector *paramTypes = newVector(sizeof(Type *), params->numItems);

    for (size_t i = 0; i < params->numItems; i++) {
        Type *paramType = ((Param *)indexVector(params, i))->type;
        pushVector(paramTypes, &paramType);
    }

    Type *functionType = malloc(sizeof(Type));
    functionType->type = TYP_FUN;
    functionType->fun.args = paramTypes;
    functionType->fun.retType = retType;

    TypedEntry *funEntry = malloc(sizeof(TypedEntry));
    funEntry->isMut = false;
    funEntry->type = functionType;

    insertHashtbl(parser->currentScope->vars, symTok.sym, funEntry);

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

    Function *fun = malloc(sizeof(Function));
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

    Type *type = parseComplexType(parser);
    HashEntry *entry = insertHashtbl(parser->typeTable, symTok.sym, type);
    if (entry == NULL) {
        queueError(msprintf("Cannot rebind name '%.*s' to a type",
                            (int)symTok.sym.len, symTok.sym.text),
                   symTok.start, symTok.end);
    }
}

Toplevel parseToplevel(Parser *parser) {
    Token keywordTok = nextToken(parser->lex);
    switch (keywordTok.type) {
        case TOK_PROC:
            return (Toplevel){.type = TOP_PROC,
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
    ret.currentScope = malloc(sizeof(Scope));
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

AST *parseSource(Lexer *lex) {
    Parser parser = newParser(lex);
    Vector *decs = newVector(sizeof(Toplevel), 0);

    for (;;) {
        Token tok = peekToken(parser.lex);

        switch (tok.type) {
            case TOK_EOF:
                /* Im sorry djikstra but no named break is a deal breaker */
                goto done;
            default: {
                Toplevel top = parseToplevel(&parser);
                pushVector(decs, &top);
            }
        }
    }
done : {
    AST *ret = malloc(sizeof(AST));
    ret->decs = decs;
    ret->globalScope = getGlobalScope(parser.currentScope);
    return ret;
}
}
