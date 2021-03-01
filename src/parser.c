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
// * Support arithmetic expressions with correct order of operations
// * Type inference
// * Continue parsing for even longer after errors occur
//
//===---------------------------------------------------------------------===//

#include <ctype.h>
#include <error.h>
#include <lexer.h>
#include <parser.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>

#define SYM_TABLE_INIT_SIZE 8

/* Holds the current state of the parser */
typedef struct {
    Scope *currentScope;
    Lexer *lex;
} Parser;

/* Bit flags used by continueUntil to allow continuing until one of several
 * tokens is reached */
enum TokTypeBits {
    TOK_INT_BITS = 1 << 0,
    TOK_COLON_BITS = 1 << 1,
    TOK_SEMICOLON_BITS = 1 << 2,
    TOK_EQUAL_BITS = 1 << 3,
    TOK_SYM_BITS = 1 << 4,
    TOK_LET_BITS = 1 << 5,
};

/* Runs through tokens until a token passed in bitflags is reached, which is
 * then returned */
Token continueUntil(Lexer *lex, int bitFlags) {
    Token tok;
    /* TODO: Make this not shit */
    for (;;) {
        tok = peekToken(lex);
        if (tok.type == TOK_EOF) {
            printErrors();
            exit(1);
        }
        /* Bit flags suck and so does C */
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

/* Creates a new statement with a start and end point */
Stmt *stmtFromTwoLocations(size_t start, size_t end, enum StmtType type) {
    Stmt *ret = malloc(sizeof(Stmt));
    ret->start = start;
    ret->end = end;
    ret->type = type;
    return ret;
}

static HashEntry *addToScope(Scope *scope, Symbol sym, Type *type) {
    TypedEntry *value = malloc(sizeof(TypedEntry));
    value->type = type;
    return insertHashtbl(scope->vars, sym, (void *)value);
}

/* Compares types for equality */
static bool compareTypes(Type *typ1, Type *typ2) {
    if (typ1->type != typ2->type) {
        return false;
    }
    switch (typ1->type) {
        case TYP_SINT:
            if (typ1->intsize != typ2->intsize) {
                return false;
            }
            break;
        case TYP_INTLIT:
            break;
    }
    return true;
}

/* This is only needed because the current error handling system does not allow
 * you to just pass a type and have the error printer call printType This means
 * an actual string must be allocated
 * When the error system is improved this can be deleted */
char *stringOfType(Type *type) {
    switch (type->type) {
        case TYP_SINT:
            return msprintf("TYP_SINT: 's%ld'", type->intsize * 8);
        case TYP_INTLIT:
            return msprintf("TYP_INTLIT");
    }
    return NULL;
}

/* This is a bit janky, but the idea is that given a symbol, this returns an
 * integer if it fits the form x[integer] where x can be any character
 * Its used to find the bit size of builtin integer types */
int64_t convertSymbolInt(Symbol sym) {
    int64_t ret = 0;
    for (size_t i = 1; i < sym.len; i++) {
        if (!isdigit(sym.text[i])) {
            return -1;
        }
        ret *= 10;
        ret += sym.text[i] - '0';
    }
    return ret;
}

Type *parseType(Parser *parser) {
    Type *ret;

    Token tok = nextToken(parser->lex);
    if (tok.type != TOK_SYM) {
        queueError(msprintf("Unexpected token, expected type to be a "
                            "single symbol, arrays "
                            "and pointers are not supported"),
                   tok.start, tok.end);
        tok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    switch (tok.sym.text[0]) {
        case 's':
            ret = malloc(sizeof(Type));
            ret->type = TYP_SINT;
            int64_t parsedSize = convertSymbolInt(tok.sym);
            if (parsedSize % 8 != 0) {
                queueError(
                    msprintf("Signed and unsigned integer types must have a "
                             "bitsize that is a power of two, not %zd",
                             parsedSize),
                    tok.start, tok.end);
                printErrors();
            }
            ret->intsize = parsedSize / 8;
            return ret;
        default:
            queueError(
                msprintf(
                    "Expected type to be a symbol with first character 's' "
                    "and ending with a number, not '%.*s'",
                    tok.sym.len, tok.sym.text),
                tok.start, tok.end);
            printErrors();
            exit(1);
    }
}

Expr *parsePrimary(Parser *parser) {
    Expr *ret;

    Token tok = nextToken(parser->lex);
    switch (tok.type) {
        case TOK_INT:
            ret = exprFromToken(tok, EXP_INT);
            ret->intlit = tok.intnum;
            ret->typeExpr = malloc(sizeof(Type));
            ret->typeExpr->type = TYP_INTLIT;
            return ret;
        case TOK_SYM:
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
            ret->typeExpr = ((TypedEntry *)entry->data)->type;
            return ret;
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

/* Returns NULL if the type cannot be coerced */
Type *coerceType(Type *type1, Type *type2) {
    switch (type1->type) {
        case TYP_INTLIT:
            switch (type2->type) {
                case TYP_INTLIT:
                    return type1;
                case TYP_SINT:
                    /* Cant cast that direction */
                    return NULL;
            }
            break;
        case TYP_SINT:
            switch (type2->type) {
                case TYP_INTLIT:
                    return type1;
                case TYP_SINT:
                    if (type1->intsize == type2->intsize) {
                        return type1;
                    }
                    return NULL;
            }
    }
    return NULL;
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
        newExpr->typeExpr = coerceType(exp->typeExpr, right->typeExpr);
        if (newExpr == NULL) {
            queueError(msprintf("Cannot coerce type '%s' to '%s'",
                                stringOfType(right->typeExpr),
                                stringOfType(exp->typeExpr)),
                       exp->start, right->end);
            printErrors();
        }
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
        newExpr->typeExpr = coerceType(exp->typeExpr, right->typeExpr);
        if (newExpr == NULL) {
            queueError(msprintf("Cannot coerce type '%s' to '%s'",
                                stringOfType(right->typeExpr),
                                stringOfType(exp->typeExpr)),
                       exp->start, right->end);
            printErrors();
        }
        exp = newExpr;
    }

    return exp;
}

Expr *parseExpr(Parser *parser) { return parseTerm(parser); }

static Stmt *parseDec(Parser *parser, Token varTok) {
    Stmt *retStmt;
    Token nameTok = nextToken(parser->lex);
    if (nameTok.type != TOK_SYM) {
        queueError(msprintf("Expected variable name after keyword 'var'"),
                   nameTok.start, nameTok.end);
        nameTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    /* Expect a colon */
    Token colonTok = nextToken(parser->lex);
    if (colonTok.type != TOK_COLON) {
        queueError(msprintf("Expected ':' after variable name and before "
                            "variable type in variable declaration"),
                   colonTok.start, colonTok.end);
        colonTok = continueUntil(parser->lex, TOK_COLON_BITS);
    }

    Type *type = parseType(parser);

    /* Should either be a semicolon or an equals sign */
    Token semicolonOrEqual = nextToken(parser->lex);

    if (semicolonOrEqual.type != TOK_SEMICOLON &&
        semicolonOrEqual.type != TOK_EQUAL) {
        queueError(msprintf("Expected '=' or ';' after declaring the "
                            "type and name of a variable"),
                   semicolonOrEqual.start, semicolonOrEqual.end);
        semicolonOrEqual =
            continueUntil(parser->lex, TOK_SEMICOLON_BITS | TOK_EQUAL_BITS);
    }

    switch (semicolonOrEqual.type) {
        /* Just a variable declaration */
        case TOK_SEMICOLON:
            retStmt = stmtFromTwoLocations(varTok.start, semicolonOrEqual.end,
                                           STMT_DEC);
            retStmt->dec.type = type;

            HashEntry *entry =
                addToScope(parser->currentScope, nameTok.sym, type);
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

            Type *exprType = exp->typeExpr;
            Type *varType = type;

            /* TODO: Do some sanity checks on the size of the integer */
            if (compareTypes(exprType, varType) == false &&
                (exprType->type != TYP_INTLIT || varType->type != TYP_SINT)) {
                queueError(
                    msprintf("Type of '%s' cannot be casted to "
                             "declared type of '%s'",
                             stringOfType(exp->typeExpr), stringOfType(type)),
                    exp->start, exp->end);
            }

            Token semicolonTok = nextToken(parser->lex);
            if (semicolonTok.type != TOK_SEMICOLON) {
                queueError(msprintf("Expected ';' after expression.\n"),
                           semicolonTok.start, semicolonTok.end);
                printErrors();
                exit(1);
            }
            Stmt *stmt = stmtFromTwoLocations(varTok.start, semicolonTok.end,
                                              STMT_DEC_ASSIGN);

            HashEntry *entry =
                addToScope(parser->currentScope, nameTok.sym, varType);
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
    Stmt *ret = stmtFromTwoLocations(symTok.start, semiTok.end, STMT_ASSIGN);

    HashEntry *entry = findInScope(parser->currentScope, symTok.sym);
    if (entry == NULL) {
        queueError(msprintf("Cannot find variable: '%.*s' in scope",
                            symTok.sym.len, (char *)symTok.sym.text),
                   ret->start, ret->end);
        printErrors();
    }

    Type *varType = ((TypedEntry *)entry->data)->type;
    Type *exprType = value->typeExpr;

    if (compareTypes(varType, exprType) == false &&
        ((exprType->type != TYP_INTLIT) || ((varType->type != TYP_SINT)))) {
        queueError(msprintf("Type of '%s' cannot be casted to "
                            "declared type of '%s'",
                            stringOfType(value->typeExpr),
                            stringOfType(((TypedEntry *)entry->data)->type)),
                   value->start, value->end);
    }
    ret->assign.var = entry;
    ret->assign.value = value;
    return ret;
}

Stmt *parseStmt(Parser *parser) {
    Token tok = nextToken(parser->lex);

    if (tok.type != TOK_LET && tok.type != TOK_SYM) {
        queueError(msprintf("Expeted 'var' or a symbol to begin a statement"),
                   tok.start, tok.end);
        tok = continueUntil(parser->lex, TOK_LET_BITS | TOK_SYM_BITS);
    }
    switch (tok.type) {
        case TOK_LET:
            return parseDec(parser, tok);
        case TOK_SYM:
            return parseAssignment(parser, tok);
        default:
            /* Unreachable */
            exit(1);
    }
}

static Parser newParser(Lexer *lex) {
    Parser ret;
    ret.lex = lex;
    ret.currentScope = malloc(sizeof(Scope));
    ret.currentScope->upScope = NULL;
    ret.currentScope->vars = newHashtbl(SYM_TABLE_INIT_SIZE);
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
    Vector *stmts = newVector(sizeof(Stmt *), 0);

    for (;;) {
        Token tok = peekToken(parser.lex);

        if (tok.type == TOK_EOF) {
            break;
        }
        Stmt *stmt = parseStmt(&parser);
        pushVector(stmts, &stmt);
    }

    AST *ret = malloc(sizeof(AST));
    ret->stmts = stmts;
    ret->globalScope = getGlobalScope(parser.currentScope);
    return ret;
}
