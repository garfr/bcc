#pragma once

#include <lexer.h>
#include <stddef.h>
#include <utils.h>

enum ExprType { EXP_INT, EXP_VAR };
enum StmtType { STMT_DEC, STMT_DEC_ASSIGN, STMT_ASSIGN };

typedef struct Expr {
    size_t start;
    size_t end;

    enum ExprType type;

    union {
        HashEntry *var;
        Symbol intlit;
    };
} Expr;

typedef struct Stmt {
    size_t start;
    size_t end;

    enum StmtType type;

    union {
        struct {
            HashEntry *var;
            /* This will get its own type soon. TODO */
            Symbol type;
        } dec;

        struct {
            HashEntry *var;
            /* This will get its own type soon. TODO */
            Symbol type;
            Expr *value;
        } dec_assign;
        struct {
            HashEntry *var;
            Expr *value;
        } assign;
    };
} Stmt;

/* upScope is NULL if its global */
typedef struct Scope {
    struct Scope *upScope;
    Hashtbl *vars;
} Scope;

typedef struct {
    Lexer *lex;
    Scope *currentScope;
} Parser;

Parser *newParser(Lexer *lex);

void printExpr(Expr *exp);
Expr *parseExpr(Parser *parser);

void printStmt(Stmt *stmt);
Stmt *parseStmt(Parser *parser);
