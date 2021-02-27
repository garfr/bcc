#pragma once

#include <parser.h>
#include <stdbool.h>
#include <utils.h>

/* Operation */
typedef enum {
    OP_COPY,
} TACOp;

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

/* One isntruction in the three address code
 * Represented as quadruples, although this may change to indirect triples later
 */

typedef struct {
    TACOp op;
    TACAddr args[3];
} TACInst;

typedef struct {
    Vector* codes;
} TAC;

TAC* convertAST(AST* ast);
void printTAC(TAC* tac);
