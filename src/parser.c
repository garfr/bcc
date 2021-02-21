#include <error.h>
#include <lexer.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>

#define SYM_TABLE_INIT_SIZE 8

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

    printf(" %zd-%zd\n", exp->start, exp->end);
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

Expr *parseExpr(Parser *parser) {
    Expr *ret;

    Token tok = nextToken(parser->lex);
    switch (tok.type) {
    case TOK_INT:
        ret = exprFromToken(tok, EXP_INT);
        ret->intlit = tok.intnum;
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
        printErrors();
        ret->var = entry;
        return ret;
    default:
        queueError(dynamicSprintf("Expected an integer or variable name for "
                                  "expressions, not another token"),
                   tok.start, tok.end);
        printErrors();
        /* This never gets called */ exit(1);
    }
}
void printStmt(Stmt *stmt) {
    switch (stmt->type) {
    case STMT_DEC:
        printf("STMT_DEC: '");
        printSymbol(stmt->dec.var->id);
        printf("' : '");
        printSymbol(stmt->dec.type);
        printf("'");
        break;
    case STMT_DEC_ASSIGN:
        printf("STMT_DEC_ASSIGN: '");
        printSymbol(stmt->dec_assign.var->id);
        printf("' : '");
        printSymbol(stmt->dec_assign.type);
        printf("' = (");
        printExpr(stmt->dec_assign.value);
        printf("\b)");
        break;
    case STMT_ASSIGN:
        printf("STMT_DEC_ASSIGN: '");
        printf("' = (");
        printExpr(stmt->dec_assign.value);
        printf("\b)");
        break;
    }

    printf(" %zd-%zd\n", stmt->start, stmt->end);
}

Stmt *stmtFromTwoTokens(Token tok1, Token tok2, enum StmtType type) {
    Stmt *ret = malloc(sizeof(Stmt));
    ret->start = tok1.start;
    ret->end = tok2.end;
    ret->type = type;
    return ret;
}

/* TODO: Make this take a type parameter */
HashEntry *addToScope(Parser *parser, Symbol sym, Symbol type) {
    Symbol *mallocedType = malloc(sizeof(Symbol));
    *mallocedType = type;
    return insertHashtbl(parser->currentScope->vars, sym, (void *)mallocedType);
}

Stmt *parseDec(Parser *parser, Token varTok) {
    Stmt *retStmt;
    Token symTok = nextToken(parser->lex);
    if (symTok.type != TOK_SYM) {
        queueError(dynamicSprintf("Expected variable name after keyword 'var'"),
                   symTok.start, symTok.end);
        printErrors();
    }

    /* Expect a colon */
    Token colonTok = nextToken(parser->lex);
    if (colonTok.type != TOK_COLON) {
        queueError(dynamicSprintf("Expected ':' after variable name and before "
                                  "variable type in variable declaration"),
                   colonTok.start, colonTok.end);
        printErrors();
    }

    /* TODO: Add more complex types */
    Token typeTok = nextToken(parser->lex);
    if (typeTok.type != TOK_SYM) {
        queueError(dynamicSprintf(
                       "Expected type name after ':' in variable declaration"),
                   colonTok.start, colonTok.end);
        printErrors();
    }

    Token semicolonOrEqual = nextToken(parser->lex);

    switch (semicolonOrEqual.type) {
    case TOK_SEMICOLON:
        retStmt = stmtFromTwoTokens(varTok, semicolonOrEqual, STMT_DEC);
        retStmt->dec.type = typeTok.sym;

        HashEntry *entry = addToScope(parser, symTok.sym, typeTok.sym);
        if (entry == NULL) {
            queueError(
                dynamicSprintf(
                    "Cannot redeclare variable: '%.*s' in the same scope",
                    entry->id.len, (char *)entry->id.text),
                retStmt->start, retStmt->end);
            printErrors();
        }
        retStmt->dec.var = entry;
        return retStmt;
    case TOK_EQUAL: {
        Expr *exp = parseExpr(parser);

        Token semicolonTok = nextToken(parser->lex);
        if (semicolonTok.type != TOK_SEMICOLON) {
            queueError(dynamicSprintf("Expected ';' after expression.\n"),
                       semicolonTok.start, semicolonTok.end);
            printErrors();
        }
        Stmt *stmt = stmtFromTwoTokens(varTok, semicolonTok, STMT_DEC_ASSIGN);

        HashEntry *entry = addToScope(parser, symTok.sym, typeTok.sym);

        stmt->dec_assign.type = typeTok.sym;
        stmt->dec_assign.var = entry;
        stmt->dec_assign.value = exp;
        return stmt;
    default:
        queueError(dynamicSprintf("Expected '=' or ';' after declaring the "
                                  "type and name of a variable"),
                   semicolonOrEqual.start, semicolonOrEqual.end);
        printErrors();
        /* This never gets called */ exit(1);
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
    if (semiTok.type != TOK_EQUAL) {
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

    switch (tok.type) {
    case TOK_VAR:
        return parseDec(parser, tok);
    case TOK_SYM:
        return parseAssign(parser, tok);
    default:
        queueError(
            dynamicSprintf("Expeted 'var' or a symbol to begin a statement"),
            tok.start, tok.end);
        printErrors();
        printf("ERROR: Expected 'var' or a symbol to begin a statement.\n");
        exit(1);
    }
}

