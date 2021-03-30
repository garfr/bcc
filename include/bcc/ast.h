#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "bcc/utils.h"

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
        TYP_CHAR,
        TYP_RECORD,
        TYP_VOID,
        TYP_INTLIT,
        TYP_FUN,
        TYP_BOOL,
        TYP_PTR,
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

        struct {
            bool mut;
            struct Type *type;
        } ptr;
    };
} Type;

struct Function;
typedef struct Function Function;

/* A variant enum representing an expression in the AST */
typedef struct Expr {
    size_t start;
    size_t end;

    enum ExprType {
        EXP_ADDROF,
        EXP_DEREF,
        EXP_CHAR,
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
        Symbol character;

        bool boolean;
        struct {
            struct Expr *exp1;
            struct Expr *exp2;
            enum {
                BINOP_ADD,
                BINOP_SUB,
                BINOP_MULT,
                BINOP_DIV,
                BINOP_EQUAL,
                BINOP_NOTEQUAL,
                BINOP_AND,
                BINOP_OR,
            } op;
        } binop;

        struct {
            struct Expr *expr;
            bool mut;
        } addr;

        struct Expr *deref;
        struct {
            Vector *arguments;  // Expr*
            Symbol name;        // This is kept so function declarations can be
                                // resolved without forward declarations
            HashEntry *entry;
        } funcall;

        struct {
            HashEntry *type;
            Hashtbl *fields;
        } reclit;
    };
} Expr;

/* Left hand side of an assignment */
typedef struct LVal {
    enum LValType {
        LVAL_DEREF,
        LVAL_VAR,
    } type;

    union {
        struct {
            Symbol sym;
            HashEntry *entry;
        } deref;

        struct {
            Symbol sym;
            HashEntry *entry;
        } var;
    };

    size_t start;
    size_t end;
} LVal;

/* A variant enum representing a single statment in the AST */
typedef struct Stmt {
    size_t start;
    size_t end;

    enum StmtType {
        STMT_DEC,
        STMT_DEC_ASSIGN,
        STMT_ASSIGN,
        STMT_RETURN,
        STMT_EXPR,
        STMT_IF,
        STMT_IF_ELSE,
    } type;

    union {
        struct {
            HashEntry *var;
            Type *type;
        } dec;

        struct {
            Scope *scope;
            Vector *block;  // Stmt*
            Expr *cond;
        } if_block;

        struct {
            Scope *scope1;
            Scope *scope2;
            Vector *block1;  // Stmt*
            Vector *block2;  // Stmt*
            Expr *cond;
        } if_else;

        struct {
            HashEntry *var;
            Type *type;
            Expr *value;
        } dec_assign;

        struct {
            LVal *lval;
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

struct Function {
    Symbol name;
    Vector *params;  // Param
    Type *retType;
    Vector *stmts;  // *Stmt
    Scope *scope;

    size_t start;
    size_t end;
};

/* The value that the AST symbol table hashes too
 * This will grow as more information is collected */
typedef struct {
    Type *type;
    bool isMut;
    bool onStack;
    Function *fun;  // Function pointers
} TypedEntry;

typedef struct {
    enum {
        TOP_VAR,
        TOP_PROC,
        TOP_EXTERN,
    } type;
    union {
        Stmt *var;
        Function *fn;
        struct {
            HashEntry *entry;
            Type *type;
        } external;
    };
} Toplevel;

/* The full abstract syntax tree that currently is just a vector of
 * statments and a global scope */
typedef struct {
    Vector *decs;  // Toplevel
    Scope *globalScope;
} AST;
