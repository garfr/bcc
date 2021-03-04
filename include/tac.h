//===-------------- tac.h - The Three Address Code Header ----------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// The header file for the tac.c
//
//===---------------------------------------------------------------------===//

#pragma once

#include <parser.h>
#include <stdbool.h>
#include <utils.h>

/* Operation */
typedef enum {
    OP_COPY,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,

    OP_GETPARAM,
    OP_ADDPARAM,
    OP_RETURN,
    OP_CALL,
} TACOp;

/* A variant enum representing a single address in the TAC */
typedef struct {
    enum {
        ADDR_VAR,
        ADDR_TEMP,
        ADDR_INTLIT,
        ADDR_EMPTY,
        ADDR_TAG,
    } type;

    union {
        Symbol intlit;
        HashEntry* var;
        struct {
            size_t num;
            Type* type;
        } temp;
        HashEntry* tag;
    };
} TACAddr;

/* One isntruction in the three address code, represented as quadruples */
typedef struct {
    enum {
        INST_OP,
        INST_TAG,
    } type;
    union {
        struct {
            TACOp op;
            TACAddr args[3];
        } op;
        Symbol sym;
    };
} TACInst;

typedef struct {
    Vector* codes;
    Hashtbl* tags;
} TAC;

TAC convertAST(AST* ast);

