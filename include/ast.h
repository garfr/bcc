//===------------- ast.h - Defines the abstract syntax tree --------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides type definitions for the AST.
// ===---------------------------------------------------------------------===//

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <unresolved_ast.h>
#include <utils.h>

/* A variant enum representing a compound type expression, which will include
 * arrays, pointers, and user defined records, enums, and tuples later */
typedef struct Type {
    enum TypeType type;

    union {
        struct {
            Vector *args;  // Types*
            struct Type *retType;
        } fun;
        HashEntry *typeEntry;
        struct {
            Hashtbl *recordFields;  // RecordField
            Vector *vec;  // A linear list of HashEntries, that represents the
        } record;
    };
} Type;

/* A variant enum representing an expression in the AST */
typedef struct Expr {
    size_t start;
    size_t end;

    enum ExprType type;

    Type *typeExpr;
    union {
        HashEntry *var;
        Symbol intlit;

        bool boolean;
        struct {
            struct Expr *exp1;
            struct Expr *exp2;
            enum BinopType op;
        } binop;

        struct {
            HashEntry *name;
            Vector *arguments;  // Expr*
        } funcall;

        struct {
            HashEntry *type;
            Hashtbl *fields;
        } reclit;
    };
} Expr;

/* A variant enum representing a single statment in the AST */
typedef struct Stmt {
    size_t start;
    size_t end;

    enum StmtType type;

    union {
        struct {
            HashEntry *var;
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

        Expr *returnExp;
        Expr *singleExpr;
    };
} Stmt;

/* A function definition */

typedef struct {
    HashEntry *var;
    Type *type;
} Param;

typedef struct {
    Symbol name;
    Vector *params;  // Param
    Type *retType;
    Vector *stmts;  // *Stmt
    Scope *scope;

    size_t start;
    size_t end;
} Function;

/* The value that the AST symbol table hashes too
 * This will grow as more information is collected */
typedef struct {
    Type *type;
    int64_t stackOffset;
    bool isMut;
} TypedEntry;

typedef struct {
    enum ToplevelType type;
    union {
        Stmt *var;
        Function *fn;
    };
} Toplevel;

/* The full abstract syntax tree that currently is just a vector of
 * statments and a global scope */
typedef struct {
    Vector *decs;  // Toplevel
    Scope *globalScope;
} AST;
