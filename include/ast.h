//===------------------ ast.h - Defines the abstract syntax tree --------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides type definitions for the AST.
//
//===---------------------------------------------------------------------===//

#pragma once 

#include <utils.h>
#include <stdint.h>
#include <stddef.h>

/* A variant enum representing a compound type expression, which will include
 * arrays, pointers, and user defined records, enums, and tuples later */
typedef struct Type {
    enum TypeType {
        TYP_SINT,
        TYP_INTLIT,
    } type;

    union {
        /* Size of the integer in bytes, must be a power of 2 */
        int64_t intsize;
    };
} Type;

/* A variant enum representing an expression in the AST */
typedef struct Expr {
    size_t start;
    size_t end;

    enum ExprType { EXP_INT, EXP_VAR, EXP_BINOP } type;

    Type *typeExpr;
    union {
        HashEntry *var;
        Symbol intlit;

        struct {
            struct Expr *exp1;
            struct Expr *exp2;
            enum { BINOP_ADD, BINOP_SUB, BINOP_MULT, BINOP_DIV } op;
        } binop;
    };
} Expr;

/* A variant enum representing a single statment in the AST */
typedef struct Stmt {
    size_t start;
    size_t end;

    enum StmtType { STMT_DEC, STMT_DEC_ASSIGN, STMT_ASSIGN } type;

    union {
        struct {
            HashEntry *var;
            Type *type;
        } dec;

        struct {
            HashEntry *var;
            Type *type;
            Expr *value;
        } dec_assign;
        struct {
            HashEntry *var;
            Expr *value;
        } assign;
    };
} Stmt;

/* The value that the AST symbol table hashes too
 * This will grow as more information is collected */
typedef struct {
    Type *type;
} TypedEntry;

/* The full abstract syntax tree that currently is just a vector of
 * statments and a global scope */
typedef struct {
    Vector *stmts;
    Scope *globalScope;
} AST;
