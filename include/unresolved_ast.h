//===------- ast.h - Returned by the parser, doesn't resolve scopes ------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Defines an AST without any symbol table resolution, allowing the programmer
// to define variables in any order
//
// ===---------------------------------------------------------------------===//
//
#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <utils.h>

/* A variant enum representing a compound type expression, which will include
 * arrays, pointers, and user defined records, enums, and tuples later */
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
};

typedef struct UR_Type {
    enum TypeType type;
    union {
        struct {
            Vector *args;  // Types*
            struct UR_Type *retType;
        } fun;
        Symbol bindingName;
        struct {
            Hashtbl *recordFields;  // UR_Type*
            Vector *vec;  // A linear list of HashEntries, that represents the
                          // order of the record
        } record;
    };
} UR_Type;

enum ExprType {
    EXP_INT,
    EXP_VAR,
    EXP_BINOP,
    EXP_FUNCALL,
    EXP_BOOL,
    EXP_RECORDLIT
};

enum BinopType {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MULT,
    BINOP_DIV,
    BINOP_EQUAL,
    BINOP_AND,
    BINOP_OR,
};
/* A variant enum representing an expression in the AST */
typedef struct UR_Expr {
    size_t start;
    size_t end;

    enum ExprType type;

    union {
        Symbol var;
        Symbol intlit;

        bool boolean;
        struct {
            struct UR_Expr *exp1;
            struct UR_Expr *exp2;
            enum BinopType op;
        } binop;

        struct {
            Symbol name;
            Vector *arguments;  // UR_Expr*
        } funcall;

        struct {
            Symbol recordName;
            Hashtbl *fields;  // UR_Expr*
        } reclit;
    };
} UR_Expr;

enum StmtType {
    STMT_DEC,
    STMT_DEC_ASSIGN,
    STMT_ASSIGN,
    STMT_RETURN,
    STMT_EXPR
};
/* A variant enum representing a single statment in the AST */
typedef struct UR_Stmt {
    size_t start;
    size_t end;

    enum StmtType type;
    union {
        struct {
            HashEntry *entry;
            UR_Type *type;
        } dec;

        struct {
            HashEntry *entry;
            UR_Type *type;
            UR_Expr *value;
        } dec_assign;

        struct {
            Symbol name;
            UR_Expr *value;
        } assign;

        UR_Expr *returnExp;
        UR_Expr *singleExpr;
    };
} UR_Stmt;

/* A function definition */

typedef struct {
    HashEntry *entry;
    UR_Type *type;
} UR_Param;

typedef struct {
    Symbol name;
    Vector *params;  // Param
    UR_Type *retType;
    Vector *stmts;  // UR_Stmt*
    Scope *scope;

    size_t start;
    size_t end;
} UR_Function;

/* The value that the AST symbol table hashes too
 * This will grow as more information is collected */
typedef struct {
    UR_Type *type;
    bool isMut;
} UnresolvedEntry;

enum ToplevelType {
    TOP_VAR,
    TOP_PROC,
};
typedef struct {
    enum ToplevelType type;
    union {
        UR_Stmt *var;
        UR_Function *fn;
    };
} UR_Toplevel;

/* The full abstract syntax tree that currently is just a vector of
 * statments and a global scope */
typedef struct {
    Vector *decs;  // UR_Toplevel
    Scope *globalScope;
} UR_AST;
