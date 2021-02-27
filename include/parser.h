#pragma once

#include <lexer.h>
#include <stddef.h>
#include <stdint.h>
#include <utils.h>

enum TypeType {
    TYP_SINT,
    TYP_INTLIT,
};

typedef struct Type {
    enum TypeType type;

    union {
        int64_t intbits;
    };
} Type;

enum ExprType { EXP_INT, EXP_VAR };

typedef struct Expr {
    size_t start;
    size_t end;

    enum ExprType type;

    Type *typeExpr;
    union {
        HashEntry *var;
        Symbol intlit;
    };
} Expr;

enum StmtType { STMT_DEC, STMT_DEC_ASSIGN, STMT_ASSIGN };

typedef struct Stmt {
    size_t start;
    size_t end;

    enum StmtType type;

    union {
        struct {
            HashEntry *var;
            /* This will get its own type soon. TODO */
            Type *type;
        } dec;

        struct {
            HashEntry *var;
            /* This will get its own type soon. TODO */
            Type *type;
            Expr *value;
        } dec_assign;
        struct {
            HashEntry *var;
            Expr *value;
        } assign;
    };
} Stmt;

typedef struct {
    Stmt **stmts;
    size_t numStmts;
    Hashtbl *symTable;
} AST;

/* Value of symbols hashed into symbol tables */
typedef struct {
    Type *type;
} TypedEntry;

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

AST *parseSource(Lexer *lex);
