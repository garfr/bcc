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
#include <pp.h>
#include <stdio.h>
#include <stdlib.h>
#include <tac.h>

static TAC newTAC() {
    TAC ret;
    ret.tags = newHashtbl(0);
    ret.codes = newVector(sizeof(TACInst*), 0);
    return ret;
}

TACInst* newInstruction(TACOp op) {
    TACInst* ret = calloc(1, sizeof(TACInst));
    ret->type = INST_OP;
    ret->op.op = op;
    return ret;
}
/* Adds a copy instruction to the code */
void addCopy(TACAddr dest, TACAddr value, TAC* tac) {
    TACInst* code = newInstruction(OP_COPY);
    code->op.args[0] = value;
    code->op.args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
    code->op.args[2] = dest;
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
            addInst->op.args[0] = addr1;
            addInst->op.args[1] = addr2;
            addInst->op.args[2] = newAddr;
            pushVector(tac->codes, &addInst);
            return newAddr;
        }
        case EXP_FUNCALL:
            /* Push the parameters */
            for (size_t i = 0; i < expr->funcall.arguments->numItems; i++) {
                TACAddr addr = convertExpr(
                    tac, *(Expr**)indexVector(expr->funcall.arguments, i));
                TACInst* paramPush = newInstruction(OP_ADDPARAM);
                paramPush->op.op = OP_ADDPARAM;
                paramPush->op.args[0] = (TACAddr){.type = ADDR_EMPTY, {}};
                paramPush->op.args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
                paramPush->op.args[2] = addr;
                pushVector(tac->codes, &paramPush);
            }

            TACInst* callInst = newInstruction(OP_CALL);
            callInst->op.args[0] = (TACAddr){
                .type = ADDR_TAG,
                .tag = findHashtbl(tac->tags, expr->funcall.name->id)};
            callInst->op.args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
            TACAddr ret = newTemp(
                ((TypedEntry*)expr->funcall.name->data)->type->fun.retType);
            callInst->op.args[2] = ret;
            pushVector(tac->codes, &callInst);
            return ret;
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
void convertParam(TAC* tac, HashEntry* var) {
    TACInst* op = newInstruction(OP_GETPARAM);
    op->op.args[0] = (TACAddr){.type = ADDR_EMPTY, {}};
    op->op.args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
    op->op.args[2] = (TACAddr){.type = ADDR_VAR, .var = var};
    pushVector(tac->codes, &op);
}

void convertToplevel(TAC* tac, Toplevel top) {
    TACInst* tag = calloc(1, sizeof(TACInst));
    tag->type = INST_TAG;
    tag->sym = top.fn->name;
    pushVector(tac->codes, &tag);

    /* Add this location to the symbol table */
    insertHashtbl(tac->tags, tag->sym, tag);

    for (size_t i = 0; i < top.fn->params->numItems; i++) {
        convertParam(tac, ((Param*)indexVector(top.fn->params, i))->var);
    }
    for (size_t i = 0; i < top.fn->stmts->numItems; i++) {
        convertStmt(tac, *((Stmt**)indexVector(top.fn->stmts, i)));
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
        switch (inst->type) {
            case INST_TAG:
                pushVector(newCodes, &inst);
                break;
            case INST_OP: {
                switch (inst->op.op) {
                    /* Arithmetic ops will be translated to copy the arg1
                     * value into dest, and then add arg2 into dest */
                    case OP_ADD:
                    case OP_SUB:
                    case OP_MUL:
                    case OP_DIV: {
                        TACInst* copyInst = newInstruction(OP_COPY);
                        copyInst->op.args[0] = inst->op.args[0];
                        copyInst->op.args[1] =
                            (TACAddr){.type = ADDR_EMPTY, {}};
                        copyInst->op.args[2] = inst->op.args[2];
                        pushVector(newCodes, &copyInst);

                        inst->op.args[0] = inst->op.args[2];
                        pushVector(newCodes, &inst);
                        break;
                        default:
                            pushVector(newCodes, &inst);
                    }
                }
            }
        }
    }
    return newCodes;
}

TAC convertAST(AST* ast) {
    TAC tac = newTAC();

    for (size_t i = 0; i < ast->decs->numItems; i++) {
        convertToplevel(&tac, *((Toplevel*)indexVector(ast->decs, i)));
    }

    tac.codes = fixArithmetic(tac.codes);
    return tac;
}
