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
} TACOp;

/* A variant enum representing a single address in the TAC */
typedef struct {
    enum {
        ADDR_VAR,
        ADDR_TEMP,
        ADDR_INTLIT,
        ADDR_EMPTY,
    } type;

    union {
        Symbol intlit;
        HashEntry* var;
        Symbol temp;
    };
} TACAddr;

/* One isntruction in the three address code, represented as quadruples */
typedef struct {
    TACOp op;
    TACAddr args[3];
} TACInst;

typedef struct {
    Vector* codes;
} TAC;

TAC convertAST(AST* ast);

