#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <tac.h>

static TAC* newTAC() {
    TAC* ret = malloc(sizeof(TAC));
    ret->codes = NULL;
    ret->numCodes = 0;
    return ret;
}

void printAddr(TACAddr addr) {
    switch (addr.type) {
        case ADDR_VAR:
            printf("'");
            printSymbol(addr.var->id);
            printf("'");
            break;
        case ADDR_INTLIT:
            printSymbol(addr.intlit);
            break;
        case ADDR_TEMP:
            printf("(temp: %zd)", addr.tempnum);
            break;
        case ADDR_EMPTY:
            break;
    }
}

void printOp(TACOp op) {
    switch (op) {
        case OP_COPY:
            printf("copy");
    }
}

void printInst(TACInst* inst) {
    printOp(inst->op);
    printf(": [");
    printAddr(inst->args[0]);
    printf(", ");
    printAddr(inst->args[1]);
    printf(", ");
    printAddr(inst->args[2]);
    printf("]");
}

void printTAC(TAC* tac) {
    for (size_t i = 0; i < tac->numCodes; i++) {
        printInst(tac->codes[i]);
        printf("\n");
    }
}

static void addCode(TACInst* code, TAC* tac) {
    tac->codes = realloc(tac->codes, sizeof(TACInst) * (tac->numCodes + 1));
    tac->codes[tac->numCodes++] = code;
}

void addCopy(TACAddr dest, TACAddr value, TAC* tac) {
    TACInst* code = malloc(sizeof(TACInst));
    code->op = OP_COPY;
    code->args[0] = value;
    code->args[1] = (TACAddr){.type = ADDR_EMPTY, {}};
    code->args[2] = dest;
    addCode(code, tac);
}

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

TAC* convertAST(AST* ast) {
    TAC* tac = newTAC();
    for (size_t i = 0; i < ast->numStmts; i++) {
        convertStmt(tac, ast->stmts[i]);
    }
    return tac;
}
