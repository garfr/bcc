//===--------------- tac.c - The Three Address Code IR -------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Taking a highlevel IR (the AST) as input, this outputs a representation of
// the program in the form of a three address code (TAC). It expects that any
// semantic or syntactical errors were located and reported during parser /
// semantic analysis on the higher level IR.
//
// The TAC is represented as quadruples, where along with a TACOP enum
// specifying the operation, each instruction has 3 address, which the 3rd being
// the output location of the operation
//
//===---------------------------------------------------------------------===//

#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <tac.h>

static TAC newTAC() {
    TAC ret;
    ret.codes = newVector(sizeof(TACInst*), 0);
    return ret;
}

/* Adds a copy instruction to the code */
void addCopy(TACAddr dest, TACAddr value, TAC* tac) {
    TACInst* code = malloc(sizeof(TACInst));
    code->op = OP_COPY;
    code->args[0] = value;
    code->args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
    code->args[2] = dest;
    pushVector(tac->codes, &code);
}

/* Very simpler conversion algorithm, will become more complex with compound
 * expressions, arrays, control structures, etc. */
void convertStmt(TAC* tac, Stmt* stmt) {
    switch (stmt->type) {
        case STMT_DEC:
            // This translates to noop, as there are no declarations in three
            // address code
            break;
        case STMT_ASSIGN: {
            // Assignment is just a copy atm, because there are no compound
            // expressions
            TACAddr value;
            switch (stmt->assign.value->type) {
                case EXP_INT:
                    value.intlit = stmt->assign.value->intlit;
                    value.type = ADDR_INTLIT;
                    break;
                case EXP_VAR:
                    value.var = stmt->assign.value->var;
                    value.type = ADDR_VAR;
            }
            TACAddr dest;
            dest.type = ADDR_VAR;
            dest.var = stmt->assign.var;
            addCopy(dest, value, tac);
        } break;

        case STMT_DEC_ASSIGN: {
            // This is the same as an assignment
            TACAddr value;
            switch (stmt->dec_assign.value->type) {
                case EXP_INT:
                    value.intlit = stmt->dec_assign.value->intlit;
                    value.type = ADDR_INTLIT;
                    break;
                case EXP_VAR:
                    value.var = stmt->dec_assign.value->var;
                    value.type = ADDR_VAR;
            }
            TACAddr dest;
            dest.type = ADDR_VAR;
            dest.var = stmt->dec_assign.var;
            addCopy(dest, value, tac);
        }
    }
}

TAC convertAST(AST* ast) {
    TAC tac = newTAC();
    for (size_t i = 0; i < ast->stmts->numItems; i++) {
        convertStmt(&tac, *((Stmt**)indexVector(ast->stmts, i)));
    }
    return tac;
}
