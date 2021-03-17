#include <stdio.h>
#include <stdlib.h>

#include "bcc/codegen.h"

const char *generateType(Type *type) {

    switch (type->type) {

    case TYP_S8:
    case TYP_U8:
    case TYP_S16:
    case TYP_U16:
    case TYP_S32:
    case TYP_U32:
    case TYP_BOOL:
        return "l";
    case TYP_S64:
    case TYP_U64:
    case TYP_INTLIT:

        return "w";
    case TYP_VOID:
        return "";
    case TYP_BINDING:
        return generateType(type->typeEntry->data);
    default:
        printf("These types dont work yet.\n");
        exit(1);
    }
}

int getNewNum() {
    static int cnt = 0;
    return cnt++;
}

char *generateBinaryOp(int op) {

    switch (op) {

    case BINOP_ADD:
        return "add";
    case BINOP_SUB:
        return "sub";
    case BINOP_MULT:
        return "mul";
    case BINOP_DIV:
        return "div";
    default:
        printf("Invalid binary operation.\n");
        exit(1);
    }
}

static int generateExpr(Scope *scope, Expr *expr, FILE *file) {
    int location;
    switch (expr->type) {
    case EXP_INT:
        location = getNewNum();
        fprintf(file, "\t%%v%d =l copy %.*s\n", location, (int)expr->intlit.len,
                expr->intlit.text);
        return location;
    case EXP_VAR:
        location = getNewNum();
        fprintf(file, "\t%%v%d =%s copy %%%.*s\n", location,
                generateType(expr->typeExpr), (int)expr->var->id.len,
                expr->var->id.text);
        return location;
    case EXP_BOOL:
        location = getNewNum();
        if (expr->boolean) {
            fprintf(file, "\t%%v%d =l copy 1\n", location);
        } else {
            fprintf(file, "\t%%v%d =l copy 0\n", location);
        }
        return location;
    case EXP_BINOP: {
        int loc1 = generateExpr(scope, expr->binop.exp1, file);
        int loc2 = generateExpr(scope, expr->binop.exp2, file);
        int location = getNewNum();
        fprintf(file, "\t%%v%d =%s %s %%v%d, %%v%d\n", location,
                generateType(expr->typeExpr), generateBinaryOp(expr->binop.op),
                loc1, loc2);
        return location;
    }

    case EXP_FUNCALL: {

        Vector *posLoc =
            newVector(sizeof(int), expr->funcall.arguments->numItems);
        if (expr->funcall.arguments->numItems > 0) {
            for (size_t i = 0; i < expr->funcall.arguments->numItems; i++) {
                Expr *exp = *((Expr **)indexVector(expr->funcall.arguments, i));
                int loc = generateExpr(scope, exp, file);
                pushVector(posLoc, &loc);
            }
        }
        location = getNewNum();
        if (expr->typeExpr->type != TYP_VOID) {
            fprintf(file, "\t%%v%d =%s call $%.*s(", location,
                    generateType(expr->typeExpr), (int)expr->funcall.name.len,
                    expr->funcall.name.text);
        } else {
            fprintf(file, "\tcall $%.*s(", (int)expr->funcall.name.len,
                    expr->funcall.name.text);
        }

        if (expr->funcall.arguments->numItems > 0) {
            for (size_t i = 0; i < expr->funcall.arguments->numItems - 1; i++) {
                Expr *exp = *((Expr **)indexVector(expr->funcall.arguments, i));
                int loc = *((int *)indexVector(posLoc, i));
                pushVector(posLoc, &loc);
                fprintf(file, "%s %%v%d, ", generateType(exp->typeExpr), loc);
            }
            Expr *exp =
                *((Expr **)indexVector(expr->funcall.arguments,
                                       expr->funcall.arguments->numItems - 1));
            int loc = *((int *)indexVector(
                posLoc, expr->funcall.arguments->numItems - 1));
            fprintf(file, "%s %%v%d", generateType(exp->typeExpr), loc);
        }
        fprintf(file, ")\n");
        return location;
    }
    case EXP_RECORDLIT:
        fprintf(file, "No funcalls or record lits yet.\n");
        exit(1);
    }
    return 0;
}

static void generateStatement(Scope *scope, Stmt *stmt, FILE *file) {

    switch (stmt->type) {
    case STMT_DEC:
        /* noop */
        break;
    case STMT_EXPR:
        generateExpr(scope, stmt->singleExpr, file);
        break;
    case STMT_ASSIGN: {
        int position = generateExpr(scope, stmt->assign.value, file);
        fprintf(file, "\t%%%.*s =%s copy %%v%d\n",
                (int)stmt->assign.var->id.len, stmt->assign.var->id.text,
                generateType(stmt->assign.value->typeExpr), position);
        break;
    }
    case STMT_RETURN: {
        if (stmt->returnExp == NULL) {

            fprintf(file, "\tret\n");
        } else {
            int position = generateExpr(scope, stmt->returnExp, file);
            fprintf(file, "\tret %%v%d\n", position);
        }
        break;
    }
    case STMT_DEC_ASSIGN: {
        int position = generateExpr(scope, stmt->dec_assign.value, file);
        fprintf(file, "\t%%%.*s =%s copy %%v%d\n",
                (int)stmt->dec_assign.var->id.len, stmt->assign.var->id.text,
                generateType(stmt->dec_assign.value->typeExpr), position);
        break;
    }
    }
}

static void generateFunction(Function *fn, FILE *file) {
    fprintf(file, "export function %s $%.*s(", generateType(fn->retType),
            (int)fn->name.len, fn->name.text);

    if (fn->params->numItems != 0) {
        for (size_t i = 0; i < fn->params->numItems - 1; i++) {
            Param param = *((Param *)indexVector(fn->params, i));
            fprintf(file, "%s %%%.*s, ", generateType(param.type),
                    (int)param.var->id.len, param.var->id.text);
        }

        Param param =
            *((Param *)indexVector(fn->params, fn->params->numItems - 1));
        fprintf(file, "%s %%%.*s", generateType(param.type),
                (int)param.var->id.len, param.var->id.text);
    }
    fprintf(file, ") {\n");
    fprintf(file, "@start\n");

    for (size_t i = 0; i < fn->stmts->numItems; i++) {
        Stmt *stmt = *((Stmt **)indexVector(fn->stmts, i));
        generateStatement(fn->scope, stmt, file);
    }

    fprintf(file, "}\n");
}

static void generateToplevel(Toplevel top, FILE *file) {

    switch (top.type) {

    case TOP_PROC:
        generateFunction(top.fn, file);
        break;
    case TOP_VAR:
        printf("Global variables not implemented yet.\n");
        exit(1);
    case TOP_EXTERN:
        break;
    }
}

void generateCode(AST *ast, FILE *file) {
    for (size_t i = 0; i < ast->decs->numItems; i++) {

        Toplevel top = *((Toplevel *)indexVector(ast->decs, i));
        generateToplevel(top, file);
    }
}
