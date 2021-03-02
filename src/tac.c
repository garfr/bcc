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
#include <pp.h>

static TAC newTAC() {
    TAC ret;
    ret.codes = newVector(sizeof(TACInst*), 0);
    return ret;
}

TACInst* newInstruction(TACOp op) {
    TACInst* ret = malloc(sizeof(TACInst));
    ret->op = op;
    return ret;
}
/* Adds a copy instruction to the code */
void addCopy(TACAddr dest, TACAddr value, TAC* tac) {
    TACInst* code = newInstruction(OP_COPY);
    code->args[0] = value;
    code->args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
    code->args[2] = dest;
    pushVector(tac->codes, &code);
}

TACAddr newTemp(Type* type) {
    static size_t currentTemp = 0;
    return (TACAddr){.type = ADDR_TEMP,
                     .temp = {.num = currentTemp++, .type = type}};
}

static TACOp astOpToTACOp(int binop) {
    switch (binop) {
        case BINOP_ADD:
            return OP_ADD;
        case BINOP_SUB:
            return OP_SUB;
        case BINOP_MULT:
            return OP_MUL;
        case BINOP_DIV:
            return OP_DIV;
    }
    printf("internal compiler error: cannot convert ast op to TAC op.\n");
    exit(1);
}

/* Returns the new temp addr that the expression is stored in */
TACAddr convertExpr(TAC* tac, Expr* expr) {
    switch (expr->type) {
        case EXP_INT:
            return (TACAddr){.type = ADDR_INTLIT, .intlit = expr->intlit};
        case EXP_VAR:
            return (TACAddr){.type = ADDR_VAR, .var = expr->var};
        case EXP_BINOP: {
            TACAddr addr1 = convertExpr(tac, expr->binop.exp1);
            TACAddr addr2 = convertExpr(tac, expr->binop.exp2);
            TACAddr newAddr = newTemp(expr->typeExpr);

            TACInst* addInst = newInstruction(astOpToTACOp(expr->binop.op));
            addInst->args[0] = addr1;
            addInst->args[1] = addr2;
            addInst->args[2] = newAddr;
            pushVector(tac->codes, &addInst);
            return newAddr;
        }
    }
    printf("Internal compiler error: Cant reach this point.\n");
    exit(1);
}

/* Very simpler conversion algorithm, will become more complex with compound
 * expressions, arrays, control structures, etc. */
void convertStmt(TAC* tac, Stmt* stmt) {
    switch (stmt->type) {
        case STMT_DEC:
            // This translates to noop, as there are no declarations in
            // three address code
            break;
        case STMT_ASSIGN: {
            TACAddr value = convertExpr(tac, stmt->assign.value);
            TACAddr dest;
            dest.type = ADDR_VAR;
            dest.var = stmt->assign.var;
            addCopy(dest, value, tac);
        } break;

        case STMT_DEC_ASSIGN: {
            // This is the same as an assignment
            TACAddr value = convertExpr(tac, stmt->dec_assign.value);
            TACAddr dest;
            dest.type = ADDR_VAR;
            dest.var = stmt->dec_assign.var;
            addCopy(dest, value, tac);
        }
    }
}

/* AMD64 expects arithmetic ops to be in the form "[op] dest, value" with
 * semantics resembling "dest [op]= value" In comparison, this TAC stores
 * arithmetic ops in the standard "[op] val1, val2, dest" format.  To solve
 * this, a pass will be made over the TAC to convert TAC arithmetic ops to
 * something more easily translated to assembly. */
Vector* fixArithmetic(Vector* oldCodes) {
    Vector* newCodes = newVector(sizeof(TACInst*), oldCodes->numItems);
    /* Loop over each code and translate when needed */
    for (size_t i = 0; i < oldCodes->numItems; i++) {
        TACInst* inst = *((TACInst**)indexVector(oldCodes, i));
        switch (inst->op) {
            /* Arithmetic ops will be translated to copy the arg1 value into
             * dest, and then add arg2 into dest */
            case OP_ADD:
            case OP_SUB:
            case OP_MUL:
            case OP_DIV: {
                TACInst* copyInst = newInstruction(OP_COPY);
                copyInst->args[0] = inst->args[0];
                copyInst->args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
                copyInst->args[2] = inst->args[2];
                pushVector(newCodes, &copyInst);

                inst->args[0] = inst->args[2];
                pushVector(newCodes, &inst);
                break;
                default:
                    pushVector(newCodes, &inst);
            }
        }
    }
    return newCodes;
}
TAC convertAST(AST* ast) {
    TAC tac = newTAC();
    for (size_t i = 0; i < ast->stmts->numItems; i++) {
        convertStmt(&tac, *((Stmt**)indexVector(ast->stmts, i)));
    }

    tac.codes = fixArithmetic(tac.codes);
    return tac;
}
