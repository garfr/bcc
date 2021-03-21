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
    case TYP_INTLIT:
    case TYP_CHAR:
        return "l";
    case TYP_S64:
    case TYP_U64:
    case TYP_FUN:

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

char *generateBinaryOp(int op, Type *type) {

    switch (op) {

    case BINOP_ADD:
        return "add";
    case BINOP_SUB:
        return "sub";
    case BINOP_MULT:
        return "mul";
    case BINOP_DIV:
        return "div";
    case BINOP_EQUAL:
        switch (type->type) {
        case TYP_S8:
        case TYP_U8:
        case TYP_S16:
        case TYP_U16:
        case TYP_S32:
        case TYP_U32:
        case TYP_BOOL:
        case TYP_INTLIT:
        case TYP_CHAR:
        case TYP_VOID:
            return "ceql";
        case TYP_S64:
        case TYP_U64:
        case TYP_FUN:
        case TYP_RECORD:
            return "ceqw";
        case TYP_BINDING:
            return generateBinaryOp(op, type->typeEntry->data);
        }
        break;
    default:
        printf("Invalid binary operation.\n");
        exit(1);
    }
    return NULL;
}

bool needsOwnInstruction(Expr *exp) {

    switch (exp->type) {
    case EXP_INT:
    case EXP_VAR:
    case EXP_BOOL:
    case EXP_CHAR:
        return false;
    case EXP_BINOP:
    case EXP_FUNCALL:
    case EXP_RECORDLIT:
        return true;
    default:
        printf("ERROR: Unexpected exp enum: %d\n", exp->type);
        exit(1);
    }
}

int translateCharacter(Symbol sym) {

    // clang-format off
    if (sym.len >= 1){
        if (sym.text[0] == '\\') {

            if (sym.text[1] == 'n') {

                return 10;
            }
        }
        else {
            return (int) sym.text[0];
        }
    }
    printf("Symbol too short.\n");
    exit(1);
}

char *generateExpr(Scope *scope, Expr *expr, bool *needsCopy, FILE *file) {
    switch (expr->type) {
    case EXP_INT:
        *needsCopy = true;
        return msprintf("%.*s", (int)expr->intlit.len, expr->intlit.text);
    case EXP_VAR:
        *needsCopy = true;
        if (expr->typeExpr->type == TYP_FUN) {
            return msprintf("$%.*s", (int)expr->var->id.len, expr->var->id.text);
        }
        else {
            return msprintf("%%%.*s", (int)expr->var->id.len, expr->var->id.text);
        }
    case EXP_BOOL:
        *needsCopy = true;
        return msprintf("%d", expr->boolean ? 1 : 0);
    case EXP_CHAR:
        *needsCopy = true;
        return msprintf("%d", translateCharacter(expr->character));
    case EXP_BINOP: {
        int loc1 = getNewNum();
        int loc2 = getNewNum();

        char *tempExpr1 =
            generateExpr(scope, expr->binop.exp1, needsCopy, file);
        char *expr1;
        if (needsOwnInstruction(expr->binop.exp1)) {
            if (*needsCopy) {
                fprintf(file, "\t%%v%d =%s %s\n", loc1,
                        generateType(expr->typeExpr), tempExpr1);

            } else {
                fprintf(file, "\t%%v%d =%s copy %s\n", loc1,
                        generateType(expr->typeExpr), tempExpr1);
            }
            expr1 = msprintf("%%v%d", loc1);
        } else {
            expr1 = tempExpr1;
        }

        char *tempExpr2 =
            generateExpr(scope, expr->binop.exp2, needsCopy, file);
        char *expr2;

        if (needsOwnInstruction(expr->binop.exp2)) {
            if (*needsCopy) {
                fprintf(file, "\t%%v%d =%s %s\n", loc2,
                        generateType(expr->typeExpr), tempExpr2);
            } else {
                fprintf(file, "\t%%v%d =%s copy %s\n", loc2,
                        generateType(expr->typeExpr), tempExpr2);
            }
            expr2 = msprintf("%%v%d", loc1);
        } else {

            expr2 = tempExpr2;
        }
        *needsCopy = false;
        return msprintf("%s %s, %s\n", generateBinaryOp(expr->binop.op, expr->typeExpr), expr1,
                        expr2);
    }

    case EXP_FUNCALL: {

        Vector *exprVec =
            newVector(sizeof(char *), expr->funcall.arguments->numItems);
        if (expr->funcall.arguments->numItems > 0) {
            for (size_t i = 0; i < expr->funcall.arguments->numItems; i++) {
                Expr *exp = *((Expr **)indexVector(expr->funcall.arguments, i));
                char *tempExpr;
                if (needsOwnInstruction(exp)) {
                    int tempLoc = getNewNum();
                    char *tempTempExpr =
                        generateExpr(scope, exp, needsCopy, file);
                    if (*needsCopy) {
                        fprintf(file, "\t%%v%d =%s copy %s\n", tempLoc,
                                generateType(exp->typeExpr), tempTempExpr);
                        tempExpr = msprintf("%%v%d", tempLoc);
                    } else {
                        fprintf(file, "\t%%v%d =%s %s\n", tempLoc,
                                generateType(exp->typeExpr), tempTempExpr);
                        tempExpr = msprintf("%%v%d", tempLoc);
                    }
                } else {
                    tempExpr = generateExpr(scope, exp, needsCopy, file);
                }
                pushVector(exprVec, &tempExpr);
            }
        }

        int location = getNewNum();

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
                char *expr = *((char **)indexVector(exprVec, i));
                fprintf(file, "%s %s, ", generateType(exp->typeExpr), expr);
            }
            Expr *exp =
                *((Expr **)indexVector(expr->funcall.arguments,
                                       expr->funcall.arguments->numItems - 1));
            char *tempExpr = *((char **)indexVector(
                exprVec, expr->funcall.arguments->numItems - 1));
            fprintf(file, "%s %s", generateType(exp->typeExpr), tempExpr);
        }
        fprintf(file, ")\n");
        *needsCopy = false;
        return msprintf("%%%d", location);
    }
    case EXP_RECORDLIT:
        printf("No funcalls or record lits yet.\n");
        exit(1);
    }
    return 0;
}

static void generateStatement(Scope *scope, Stmt *stmt, FILE *file) {

    bool copy;
    switch (stmt->type) {
    case STMT_DEC:
        /* noop */
        break;
    case STMT_EXPR:
        generateExpr(scope, stmt->singleExpr, &copy, file);
        break;
    case STMT_ASSIGN: {
        char *expr = generateExpr(scope, stmt->assign.value, &copy, file);
        if (copy) {
            fprintf(file, "\t%%%.*s =%s %s\n", (int)stmt->assign.var->id.len,
                    stmt->assign.var->id.text,
                    generateType(stmt->assign.value->typeExpr), expr);
        } else {
            fprintf(file, "\t%%%.*s =%s copy %s\n",
                    (int)stmt->assign.var->id.len, stmt->assign.var->id.text,
                    generateType(stmt->assign.value->typeExpr), expr);
        }
        break;
    }
    case STMT_RETURN: {
        if (stmt->returnExp == NULL) {

            fprintf(file, "\tret\n");
        } else {
            char *expr = generateExpr(scope, stmt->returnExp, &copy, file);
            fprintf(file, "\tret %s\n", expr);
        }
        break;
    }
    case STMT_IF: {
        char* expr = generateExpr(scope, stmt->if_block.cond, &copy, file);
        int value1 = getNewNum();
        if (copy) {

            fprintf(file, "\t%%v%d =%s copy %s\n", value1, generateType(stmt->if_block.cond->typeExpr), expr);
        }
        else {
            fprintf(file, "\t%%v%d =%s %s\n", value1, generateType(stmt->if_block.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        fprintf(file, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1, loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_block.block, i));
            generateStatement(scope, tempStmt, file);
        }
        fprintf(file, "@loc%d\n", loc2);
        break;
    }
    case STMT_IF_ELSE: {
        char* expr = generateExpr(scope, stmt->if_else.cond, &copy, file);
        int value1 = getNewNum();
        if (copy) {
            fprintf(file, "\t%%v%d =%s copy %s\n", value1, generateType(stmt->if_else.cond->typeExpr), expr);
        }
        else {
            fprintf(file, "\t%%v%d =%s %s\n", value1, generateType(stmt->if_else.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        int loc3 = getNewNum();
        fprintf(file, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1, loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_else.block1, i));
            generateStatement(scope, tempStmt, file);
        }
        fprintf(file, "\tjmp @loc%d\n", loc3);
        fprintf(file, "@loc%d\n", loc2);
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_else.block2, i));
            generateStatement(scope, tempStmt, file);
        }
        fprintf(file, "@loc%d\n", loc3);
        break;
    }
    case STMT_DEC_ASSIGN: {
        char *expr = generateExpr(scope, stmt->dec_assign.value, &copy, file);
        if (copy) {
            fprintf(file, "\t%%%.*s =%s copy %s\n",
                    (int)stmt->dec_assign.var->id.len,
                    stmt->assign.var->id.text,
                    generateType(stmt->dec_assign.value->typeExpr), expr);
        } else {
            fprintf(file, "\t%%%.*s =%s %s\n",
                    (int)stmt->dec_assign.var->id.len,
                    stmt->assign.var->id.text,
                    generateType(stmt->dec_assign.value->typeExpr), expr);
        }
        break;
    }
default:
                          printf("Sorry, cant codegen flow control yet.\n");
                          exit(1);
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
