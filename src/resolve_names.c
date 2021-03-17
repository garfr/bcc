#include <stdio.h>
#include <stdlib.h>

// clang-format off
#include "bcc/utils.h"
#include "bcc/ast.h"
#include "bcc/error.h"
#include "bcc/resolve_names.h"
// clang-format on 

void resolveExpr(Scope *scope, Expr *exp) {
    switch (exp->type) {
    case EXP_FUNCALL:
        if (exp->funcall.entry == NULL) {
            HashEntry *entry = findInScope(scope, exp->funcall.name);
            if (entry == NULL) {
                queueError(
                    msprintf("Could not find variable '%.*s' in scope.\n",
                             exp->funcall.name.len, exp->funcall.name.text),
                    exp->start, exp->end);
                printErrors();
            }

            exp->funcall.entry = entry;
            exp->typeExpr = ((TypedEntry *)entry->data)->type->fun.retType;
        }
        for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
            resolveExpr(scope,
                        *((Expr **)indexVector(exp->funcall.arguments, i)));
        }
        break;
    case EXP_BINOP:
        resolveExpr(scope, exp->binop.exp1);
        resolveExpr(scope, exp->binop.exp2);
        break;
    case EXP_INT:
    case EXP_BOOL:
    case EXP_VAR:
        return;
    case EXP_RECORDLIT: {
        for (size_t i = 0; i < exp->reclit.fields->entries; i++) {
            for (HashEntry *entry = exp->reclit.fields->buckets[i];
                 entry != NULL; entry = entry->next) {
                resolveExpr(scope, entry->data);
            }
        }
    }
    }
}

void resolveNames(AST *ast) {
    for (size_t i = 0; i < ast->decs->numItems; i++) {
        Toplevel toplevel = *((Toplevel *)indexVector(ast->decs, i));
        switch (toplevel.type) {
        case TOP_PROC: {
            Scope *scope = toplevel.fn->scope;
            for (size_t j = 0; j < toplevel.fn->stmts->numItems; j++) {
                Stmt *stmt = *((Stmt **)indexVector(toplevel.fn->stmts, j));
                switch (stmt->type) {
                case STMT_EXPR:
                    resolveExpr(scope, stmt->singleExpr);
                    break;
                case STMT_DEC:
                    break;
                case STMT_ASSIGN:
                    resolveExpr(scope, stmt->assign.value);
                    break;
                case STMT_RETURN:
                    if (stmt->returnExp != NULL) {
                        resolveExpr(scope, stmt->returnExp);
                    }
                    break;
                case STMT_DEC_ASSIGN:
                    resolveExpr(scope, stmt->dec_assign.value);
                    break;
                }
            }
        }
        case TOP_VAR:
        case TOP_EXTERN:
            break;
        }
    }
}
