#include <ctype.h>
#include <error.h>
#include <lexer.h>
#include <parser.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>

#define SYM_TABLE_INIT_SIZE 8

enum TokTypeBits {
    TOK_INT_BITS = 1 << 0,
    TOK_COLON_BITS = 1 << 1,
    TOK_SEMICOLON_BITS = 1 << 2,
    TOK_EQUAL_BITS = 1 << 3,
    TOK_SYM_BITS = 1 << 4,
    TOK_VAR_BITS = 1 << 5,
};

Parser *newParser(Lexer *lex) {
    Parser *ret = malloc(sizeof(Parser));
    ret->lex = lex;
    ret->currentScope = malloc(sizeof(Scope));
    ret->currentScope->upScope = NULL;
    ret->currentScope->vars = newHashtbl(SYM_TABLE_INIT_SIZE);
    return ret;
}

Expr *exprFromToken(Token tok, enum ExprType type) {
    Expr *exp = malloc(sizeof(Expr));
    exp->type = type;
    exp->start = tok.start;
    exp->end = tok.end;
    return exp;
}

void printExpr(Expr *exp) {
    switch (exp->type) {
        case EXP_INT:
            printf("EXP_INT: '");
            printSymbol(exp->intlit);
            printf("'");
            break;
        case EXP_VAR:
            printf("EXP_VAR: '");
            printSymbol(exp->var->id);
            printf("'");
            break;
    }

    printf(" %zd-%zd", exp->start, exp->end);
}

HashEntry *findInScope(Parser *parser, Symbol sym) {
    for (Scope *scope = parser->currentScope; scope != NULL;
         scope = scope->upScope) {
        HashEntry *entry = findHashtbl(scope->vars, sym);
        if (entry != NULL) {
            return entry;
        }
    }
    return NULL;
}

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

/* Parses a symbol into a positive integer, ignoring the first number */
int64_t parseSymbolToInt(Symbol sym) {
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
        queueError(dynamicSprintf("Unexpected token, expected type to be a "
                                  "single symbol, arrays "
                                  "and pointers are not supported"),
                   tok.start, tok.end);
        tok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    switch (tok.sym.text[0]) {
        case 's':
            ret = malloc(sizeof(Type));
            ret->type = TYP_SINT;
            ret->intbits = parseSymbolToInt(tok.sym);
            return ret;
        default:
            queueError(
                dynamicSprintf(
                    "Expected type to be a symbol with first character 's' "
                    "and ending with a number, not '%.*s'",
                    tok.sym.len, tok.sym.text),
                tok.start, tok.end);
            ret = malloc(sizeof(Type));
            ret->type = TYP_SINT;
            ret->intbits = -1;
            return ret;
    }
}

Expr *parseExpr(Parser *parser) {
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

            HashEntry *entry = findInScope(parser, tok.sym);
            if (entry == NULL) {
                queueError(dynamicSprintf("Cannot find variable: '%.*s' in any "
                                          "scope. Must be undeclared",
                                          tok.sym.len, (char *)tok.sym.text),
                           tok.start, tok.end);
            }
            /* Must fail */
            printErrors();
            ret->var = entry;
            ret->typeExpr = ((TypedEntry *)entry->data)->type;
            return ret;
        default:
            queueError(
                dynamicSprintf("Expected an integer or variable name for "
                               "expressions, not another token"),
                tok.start, tok.end);
            printErrors();
            /* This never gets called */ exit(1);
    }
}

char *stringOfType(Type *type) {
    switch (type->type) {
        case TYP_SINT:
            return dynamicSprintf("TYP_SINT: 's%ld'", type->intbits);
        case TYP_INTLIT:
            return dynamicSprintf("TYP_INTLIT");
    }
    return NULL;
}

void printType(Type *type) {
    switch (type->type) {
        case TYP_SINT:
            printf("TYP_SINT: 's%ld'", type->intbits);
            break;
        case TYP_INTLIT:
            printf("TYP_INTLIT");
    }
}
void printStmt(Stmt *stmt) {
    switch (stmt->type) {
        case STMT_DEC:
            printf("STMT_DEC: '");
            printSymbol(stmt->dec.var->id);
            printf("' : '");
            printType(stmt->dec.type);
            printf("'");
            break;
        case STMT_DEC_ASSIGN:
            printf("STMT_DEC_ASSIGN: '");
            printSymbol(stmt->dec_assign.var->id);
            printf("' : '");
            printType(stmt->dec_assign.type);
            printf("' = (");
            printExpr(stmt->dec_assign.value);
            printf(")");
            break;
        case STMT_ASSIGN:
            printf("STMT_ASSIGN: '");
            printSymbol(stmt->assign.var->id);
            printf("' = (");
            printExpr(stmt->assign.value);
            printf(")");
            break;
    }

    printf(" %zd-%zd", stmt->start, stmt->end);
}
int compareTypes(Type *typ1, Type *typ2) {
    if (typ1->type != typ2->type) {
        return false;
    }
    switch (typ1->type) {
        case TYP_SINT:
            if (typ1->intbits != typ2->intbits) {
                return false;
            }
            break;
        case TYP_INTLIT:
            break;
    }
    return true;
}

Stmt *stmtFromTwoTokens(Token tok1, Token tok2, enum StmtType type) {
    Stmt *ret = malloc(sizeof(Stmt));
    ret->start = tok1.start;
    ret->end = tok2.end;
    ret->type = type;
    return ret;
}

HashEntry *addToScope(Parser *parser, Symbol sym, Type *type) {
    TypedEntry *value = malloc(sizeof(TypedEntry));
    value->type = type;
    return insertHashtbl(parser->currentScope->vars, sym, (void *)value);
}

Stmt *parseDec(Parser *parser, Token varTok) {
    Stmt *retStmt;
    Token symTok = nextToken(parser->lex);
    if (symTok.type != TOK_SYM) {
        queueError(dynamicSprintf("Expected variable name after keyword 'var'"),
                   symTok.start, symTok.end);
        symTok = continueUntil(parser->lex, TOK_SYM_BITS);
    }

    /* Expect a colon */
    Token colonTok = nextToken(parser->lex);
    if (colonTok.type != TOK_COLON) {
        queueError(dynamicSprintf("Expected ':' after variable name and before "
                                  "variable type in variable declaration"),
                   colonTok.start, colonTok.end);
        colonTok = continueUntil(parser->lex, TOK_COLON_BITS);
    }

    Type *type = parseType(parser);
    Token semicolonOrEqual = nextToken(parser->lex);

    if (semicolonOrEqual.type != TOK_SEMICOLON &&
        semicolonOrEqual.type != TOK_EQUAL) {
        queueError(dynamicSprintf("Expected '=' or ';' after declaring the "
                                  "type and name of a variable"),
                   semicolonOrEqual.start, semicolonOrEqual.end);
        semicolonOrEqual =
            continueUntil(parser->lex, TOK_SEMICOLON_BITS | TOK_EQUAL_BITS);
    }
    switch (semicolonOrEqual.type) {
        case TOK_SEMICOLON:
            retStmt = stmtFromTwoTokens(varTok, semicolonOrEqual, STMT_DEC);
            retStmt->dec.type = type;

            HashEntry *entry = addToScope(parser, symTok.sym, type);
            if (entry == NULL) {
                queueError(
                    dynamicSprintf("Cannot redeclare variable: '%.*s' in "
                                   "the same scope",
                                   entry->id.len, (char *)entry->id.text),
                    retStmt->start, retStmt->end);
                /* Must fail */
                printErrors();
            }
            retStmt->dec.var = entry;
            return retStmt;
        case TOK_EQUAL: {
            Expr *exp = parseExpr(parser);

            /* TODO: Do some sanity checks on the size of the integer */
            if (compareTypes(exp->typeExpr, type) == false &&
                (exp->typeExpr->type != TYP_INTLIT || type->type != TYP_SINT)) {
                queueError(dynamicSprintf("Type of '%s' cannot be casted to "
                                          "declared type of '%s'",
                                          stringOfType(exp->typeExpr),
                                          stringOfType(type)),
                           exp->start, exp->end);
            }

            Token semicolonTok = nextToken(parser->lex);
            if (semicolonTok.type != TOK_SEMICOLON) {
                queueError(dynamicSprintf("Expected ';' after expression.\n"),
                           semicolonTok.start, semicolonTok.end);
                printErrors();
            }
            Stmt *stmt =
                stmtFromTwoTokens(varTok, semicolonTok, STMT_DEC_ASSIGN);

            HashEntry *entry = addToScope(parser, symTok.sym, type);

            stmt->dec_assign.type = type;
            stmt->dec_assign.var = entry;
            stmt->dec_assign.value = exp;
            return stmt;
            default:
                /* This never gets called */
                printErrors();
                exit(1);
        }
    }
}

Stmt *parseAssign(Parser *parser, Token symTok) {
    Token equalTok = nextToken(parser->lex);
    if (equalTok.type != TOK_EQUAL) {
        queueError(
            dynamicSprintf("Expected '=' after variable name in assignment"),
            equalTok.start, equalTok.end);
        printErrors();
    }
    Expr *value = parseExpr(parser);

    Token semiTok = nextToken(parser->lex);
    if (semiTok.type != TOK_SEMICOLON) {
        queueError(
            dynamicSprintf("Expected ';' after expression in assignment"),
            semiTok.start, semiTok.end);
        printErrors();
    }
    Stmt *ret = stmtFromTwoTokens(symTok, semiTok, STMT_ASSIGN);

    HashEntry *entry = findInScope(parser, symTok.sym);
    if (entry == NULL) {
        queueError(dynamicSprintf("Cannot find variable: '%.*s' in scope",
                                  symTok.sym.len, (char *)symTok.sym.text),
                   ret->start, ret->end);
        printErrors();
    }
    ret->assign.var = entry;
    ret->assign.value = value;
    return ret;
}
Stmt *parseStmt(Parser *parser) {
    Token tok = nextToken(parser->lex);

    if (tok.type != TOK_VAR && tok.type != TOK_SYM) {
        queueError(
            dynamicSprintf("Expeted 'var' or a symbol to begin a statement"),
            tok.start, tok.end);
        tok = continueUntil(parser->lex, TOK_VAR_BITS | TOK_SYM_BITS);
    }
    switch (tok.type) {
        case TOK_VAR:
            return parseDec(parser, tok);
        case TOK_SYM:
            return parseAssign(parser, tok);
        default:
            printErrors();
            exit(1);
    }
}

