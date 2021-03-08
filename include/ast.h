//===------------- ast.h - Defines the abstract syntax tree --------------===//
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

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <utils.h>

/* A variant enum representing a compound type expression, which will include
 * arrays, pointers, and user defined records, enums, and tuples later */
typedef struct Type {
    enum TypeType {
        TYP_S8,
        TYP_S16,
        TYP_S32,
        TYP_S64,
        TYP_U8,
        TYP_U16,
        TYP_U32,
        TYP_U64,
        TYP_RECORD,
        TYP_VOID,
        TYP_INTLIT,
        TYP_FUN,
        TYP_BOOL,
        // A binding of a more complex type to a single name
        TYP_BINDING,
    } type;

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

    enum ExprType {
        EXP_INT,
        EXP_VAR,
        EXP_BINOP,
        EXP_FUNCALL,
        EXP_BOOL,
        EXP_RECORDLIT
    } type;

    Type *typeExpr;
    union {
        HashEntry *var;
        Symbol intlit;

        bool boolean;
        struct {
            struct Expr *exp1;
            struct Expr *exp2;
            enum {
                BINOP_ADD,
                BINOP_SUB,
                BINOP_MULT,
                BINOP_DIV,
                BINOP_EQUAL
            } op;
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

    enum StmtType {
        STMT_DEC,
        STMT_DEC_ASSIGN,
        STMT_ASSIGN,
        STMT_RETURN,
        STMT_EXPR
    } type;

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
    enum {
        TOP_VAR,
        TOP_PROC,
    } type;
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
