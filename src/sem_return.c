#include <stdio.h>
#include <stdlib.h>

#include "bcc/ast.h"
#include "bcc/error.h"
#include "bcc/semantics.h"

void checkReturns(AST *ast) {

    for (size_t i = 0; i < ast->decs->numItems; i++) {

        Toplevel top = *((Toplevel *)indexVector(ast->decs, i));
        switch (top.type) {

        case TOP_PROC: {
            bool doesReturn = false;
            for (size_t j = 0; j < top.fn->stmts->numItems; j++) {
                Stmt *stmt = *((Stmt **)indexVector(top.fn->stmts, j));
                switch (stmt->type) {
                case STMT_RETURN:
                    doesReturn = true;
                    if (stmt->returnExp == NULL) {

                        if (top.fn->retType->type != TYP_VOID) {
                            queueError("Function returns no value in a "
                                       "non-void function",
                                       stmt->start, stmt->end);
                        }
                    } else if (coerceAssignment(stmt->returnExp->typeExpr,
                                                top.fn->retType) == NULL) {
                        queueError("Wrong type returned from function",
                                   stmt->start, stmt->end);
                    }
                    break;
                default:
                    continue;
                }
            }
            if (doesReturn == false) {

                if (top.fn->retType->type == TYP_VOID) {
                    // Add implicit return
                    Stmt *implRet = calloc(1, sizeof(Stmt));
                    implRet->type = STMT_RETURN;

                    pushVector(top.fn->stmts, &implRet);
                } else {
                    queueError("Function never returns value", top.fn->start,
                               top.fn->end);
                }
            }
        } break;

        default:
            continue;
        }
    }
}
