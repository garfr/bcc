#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// clang-format off
#include "bcc/codegen.h"
#include "bcc/pp.h"
#include "bcc/semantics.h"
#include "bcc/utils.h"
// clang-format on

struct {
    FILE *out;
} context;

/* Calculates the size of a type in bytes */
int64_t calculateSize(Type *type) {
    switch (type->type) {
        case TYP_S8:
        case TYP_U8:
            return 1;
        case TYP_S16:
        case TYP_U16:
            return 2;
        case TYP_S32:
        case TYP_BOOL:
        case TYP_U32:
        case TYP_CHAR:
            return 4;
        case TYP_S64:
        case TYP_U64:
        case TYP_PTR:
            return 8;
        case TYP_VOID:
            return 0;
        case TYP_FUN:
            return 8;
        case TYP_INTLIT:
            printf(
                "Internal compiler error: Cannot calculate size of integer "
                "literal,\n");
            exit(1);
        case TYP_RECORD: {
            int64_t size = 0;
            for (size_t i = 0; i < type->record.vec->numItems; i++) {
                HashEntry *entry =
                    *((HashEntry **)indexVector(type->record.vec, i));
                size += calculateSize((Type *)entry->data);
            }
            return size;
        }
        case TYP_BINDING:
            return calculateSize(type->typeEntry->data);
    }
    printf(
        "Internal compiler error: Reached end of calculateSize without "
        "returning.\n");
    exit(1);
}

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
            return "w";
        case TYP_S64:
        case TYP_U64:
        case TYP_PTR:
        case TYP_FUN:

            return "l";
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
        case BINOP_NOTEQUAL:
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
                    return "ceqw";
                case TYP_S64:
                case TYP_U64:
                case TYP_FUN:
                case TYP_RECORD:
                case TYP_PTR:
                    return "ceql";
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
        case EXP_ADDROF:
        case EXP_DEREF:
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


char *generateExpr(Scope *scope, Expr *expr, bool *needsCopy);

/* And will not evaluate the right hand side if the first value evaluates to false */
char *generateAnd(Scope *scope, Expr *expr, bool *needsCopy) { 

    int loc1 = getNewNum();

    char *tempExpr1 =
        generateExpr(scope, expr->binop.exp1, needsCopy);
    if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
                generateType(expr->typeExpr), tempExpr1);
    } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc1,
                generateType(expr->typeExpr), tempExpr1);
    }

    int jmp1 = getNewNum();
    int jmp2 = getNewNum();
    int jmp3 = getNewNum();
    int jmp4 = getNewNum();

    fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc1, jmp1, jmp3);
    fprintf(context.out, "@j%d\n", jmp1);

    int loc2 = getNewNum();

    char *tempExpr2 =
        generateExpr(scope, expr->binop.exp2, needsCopy);

    if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
                generateType(expr->typeExpr), tempExpr2);
    } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc2,
                generateType(expr->typeExpr), tempExpr2);
    }

    fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc2, jmp2, jmp3);

    fprintf(context.out, "@j%d\n",  jmp2);

    int loc3 = getNewNum();

    fprintf(context.out, "\t%%v%d =l copy 1\n", loc3);
    fprintf(context.out, "\tjmp @j%d\n", jmp4);

    fprintf(context.out, "@j%d\n", jmp3);
    fprintf(context.out, "\t%%v%d =l copy 0\n", loc3);
    fprintf(context.out, "@j%d\n", jmp4);

    *needsCopy = true;

    return msprintf("%%v%d", loc3);
}

/*Or will not evaluate the right hand side if the first value evaluates to true */
char *generateOr(Scope *scope, Expr *expr, bool *needsCopy) { 


    int loc1 = getNewNum();

    char *tempExpr1 =
        generateExpr(scope, expr->binop.exp1, needsCopy);
    if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
                generateType(expr->typeExpr), tempExpr1);
    } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc1,
                generateType(expr->typeExpr), tempExpr1);
    }

    int jmp1 = getNewNum();
    int jmp2 = getNewNum();
    int jmp3 = getNewNum();
    int jmp4 = getNewNum();

    fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc1, jmp2, jmp1);
    fprintf(context.out, "@j%d\n", jmp1);

    int loc2 = getNewNum();

    char *tempExpr2 =
        generateExpr(scope, expr->binop.exp2, needsCopy);

    if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
                generateType(expr->typeExpr), tempExpr2);
    } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc2,
                generateType(expr->typeExpr), tempExpr2);
    }

    fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc2, jmp2, jmp3);

    fprintf(context.out, "@j%d\n",  jmp2);

    int loc3 = getNewNum();

    fprintf(context.out, "\t%%v%d =l copy 1\n", loc3);
    fprintf(context.out, "\tjmp @j%d\n", jmp4);

    fprintf(context.out, "@j%d\n", jmp3);
    fprintf(context.out, "\t%%v%d =l copy 0\n", loc3);
    fprintf(context.out, "@j%d\n", jmp4);

    *needsCopy = true;

    return msprintf("%%v%d", loc3);
}


char* pickLoadInst(Type* type) {
    switch (type->type) {
        case TYP_S8:
            return "loadsb";
        case TYP_U8:
            return "loadub";
        case TYP_S16:
            return "loadsh";
        case TYP_U16:
            return "loaduh";
        case TYP_S32:
            return "loadsw";
        case TYP_U32:
        case TYP_BOOL:
        case TYP_CHAR:
            return "loaduw";
        case TYP_S64:
        case TYP_U64:
        case TYP_PTR:
        case TYP_FUN:
            return "loadl";
        case TYP_BINDING:
            return pickLoadInst(type->typeEntry->data);
        case TYP_VOID:
        case TYP_INTLIT:
        case TYP_RECORD:
            assert(false);
            return NULL;
    }
    return NULL;
}

char* pickStoreInst(Type* type) {
    switch (type->type) {
        case TYP_S8:
        case TYP_U8:
            return "storeb";
        case TYP_S16:
        case TYP_U16:
            return "storeh";
        case TYP_S32:
        case TYP_U32:
            return "storew";
        case TYP_S64:
        case TYP_U64:
            return "storel";
        case TYP_BOOL:
            return "storew";
        case TYP_CHAR:
            return "storew";
        case TYP_FUN:
            return "storel";
        case TYP_BINDING:
            return pickStoreInst(type->typeEntry->data);
        default:
            printf("These types dont work yet.\n");
            exit(1);
    }
}

char *generateExpr(Scope *scope, Expr *expr, bool *needsCopy) {
    switch (expr->type) {
    case EXP_INT:
        *needsCopy = true;
        return msprintf("%.*s", (int)expr->intlit.len, expr->intlit.text);
    case EXP_VAR: {
        TypedEntry * entry = expr->var->data;

        if (entry->onStack) {

            int loc = getNewNum();
            /* Fetch the location from the stack */
            fprintf(context.out, "\t%%v%d =%s %s %%%.*s\n", loc, generateType(entry->type), pickLoadInst(expr->typeExpr), (int) expr->var->id.len, expr->var->id.text);
            *needsCopy = true;
            return msprintf("%%v%d", loc);
        }

        *needsCopy = true;
        if (expr->typeExpr->type == TYP_FUN) {
            return msprintf("$%.*s", (int)expr->var->id.len, expr->var->id.text);
        }
        else {
            return msprintf("%%%.*s", (int)expr->var->id.len, expr->var->id.text);
        }
    }
    case EXP_BOOL:
        *needsCopy = true;
        return msprintf("%d", expr->boolean ? 1 : 0);
    case EXP_CHAR:
        *needsCopy = true;
        return msprintf("%d", translateCharacter(expr->character));
    case EXP_ADDROF:  {
        if (expr->addrOf->type == EXP_VAR) {
            *needsCopy = true;
            return msprintf("%%%.*s", (int) expr->addrOf->var->id.len, expr->addrOf->var->id.text);
        }
        assert(false); // This will be signalled as an erro in gen_stack.c
        return NULL;
    }
    case EXP_DEREF: {
        char* exp = generateExpr(scope, expr->deref, needsCopy);
        int loc1 = getNewNum();
        if (*needsCopy) {
            fprintf(context.out, "\t%%v%d =l copy %s\n", loc1, exp);
        }
        else {
            fprintf(context.out, "\t%%v%d =l %s\n", loc1, exp);
        }
        int loc2 = getNewNum();

        fprintf(context.out, "\t%%v%d =%s %s %%v%d\n", loc2, generateType(expr->deref->typeExpr), pickLoadInst(expr->deref->typeExpr), loc1);
        *needsCopy = true;
        return msprintf("%%v%d", loc2);
    }
    case EXP_BINOP: {
        if (expr->binop.op == BINOP_AND) {
            return generateAnd(scope, expr, needsCopy);
        }
        if (expr->binop.op == BINOP_OR) {
            return generateOr(scope, expr, needsCopy);
        }

        int loc1 = getNewNum();
        int loc2 = getNewNum();

        char *tempexpr1 =
            generateExpr(scope, expr->binop.exp1, needsCopy);

        char *expr1;
        if (needsOwnInstruction(expr->binop.exp1)) {
            if (*needsCopy) {
                fprintf(context.out, "\t%%v%d =%s %s\n", loc1,
                        generateType(expr->typeExpr), tempexpr1);

            } else {
                fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
                        generateType(expr->typeExpr), tempexpr1);
            }
            expr1 = msprintf("%%v%d", loc1);
        } else {
            expr1 = tempexpr1;
        }

        char *tempExpr2 =
            generateExpr(scope, expr->binop.exp2, needsCopy);

        char *expr2;

        if (needsOwnInstruction(expr->binop.exp2)) {
            if (*needsCopy) {
                fprintf(context.out, "\t%%v%d =%s %s\n", loc2,
                        generateType(expr->typeExpr), tempExpr2);
            } else {
                fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
                        generateType(expr->typeExpr), tempExpr2);
            }
            expr2 = msprintf("%%v%d", loc1);
        } else {

            expr2 = tempExpr2;
        }
        *needsCopy = false;
        return msprintf("%s %s, %s", generateBinaryOp(expr->binop.op, expr->typeExpr), expr1,
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
                        generateExpr(scope, exp, needsCopy);
                    if (*needsCopy) {
                        fprintf(context.out, "\t%%v%d =%s copy %s\n", tempLoc,
                                generateType(exp->typeExpr), tempTempExpr);
                        tempExpr = msprintf("%%v%d", tempLoc);
                    } else {
                        fprintf(context.out, "\t%%v%d =%s %s\n", tempLoc,
                                generateType(exp->typeExpr), tempTempExpr);
                        tempExpr = msprintf("%%v%d", tempLoc);
                    }
                } else {
                    tempExpr = generateExpr(scope, exp, needsCopy);
                }
                pushVector(exprVec, &tempExpr);
            }
        }

        int location = getNewNum();

        if (expr->typeExpr->type != TYP_VOID) {
            fprintf(context.out, "\t%%v%d =%s call $%.*s(", location,
                    generateType(expr->typeExpr), (int)expr->funcall.name.len,
                    expr->funcall.name.text);
        } else {
            fprintf(context.out, "\tcall $%.*s(", (int)expr->funcall.name.len,
                    expr->funcall.name.text);
        }

        if (expr->funcall.arguments->numItems > 0) {
            for (size_t i = 0; i < expr->funcall.arguments->numItems - 1; i++) {
                Expr *exp = *((Expr **)indexVector(expr->funcall.arguments, i));
                char *expr = *((char **)indexVector(exprVec, i));
                fprintf(context.out, "%s %s, ", generateType(exp->typeExpr), expr);
            }
            Expr *exp =
                *((Expr **)indexVector(expr->funcall.arguments,
                                       expr->funcall.arguments->numItems - 1));
            char *tempExpr = *((char **)indexVector(
                exprVec, expr->funcall.arguments->numItems - 1));
            fprintf(context.out, "%s %s", generateType(exp->typeExpr), tempExpr);
        }
        fprintf(context.out, ")\n");
        *needsCopy = true;
        return msprintf("%%v%d", location);
    }
    case EXP_RECORDLIT:
        printf("No funcalls or record lits yet.\n");
        exit(1);
    }
    return 0;
}

static void generateStatement(Scope *scope, Stmt *stmt) {

    bool copy;
    switch (stmt->type) {
    case STMT_DEC: {
        TypedEntry* entry = stmt->dec.var->data;
        if (entry->onStack) {
            fprintf(context.out, "\t%%%.*s =l alloc4 %ld\n", 
                    (int) stmt->dec.var->id.len, 
                    stmt->dec.var->id.text, 
                    calculateSize(stmt->dec.type));
            printType(stmt->dec.type);
        }
        break;
    }

    case STMT_EXPR:
        generateExpr(scope, stmt->singleExpr, &copy);
        break;
    case STMT_ASSIGN: {
        TypedEntry* entry = stmt->assign.var->data;
        char *expr = generateExpr(scope, stmt->assign.value, &copy);
        int loc = getNewNum();
        if (entry->onStack) {
            if (copy) {
                fprintf(context.out, "\t%%v%d =%s copy %s\n", loc, generateType(stmt->assign.value->typeExpr), expr);
            }
            else {
                fprintf(context.out, "\t%%v%d =%s %s\n", loc, generateType(stmt->assign.value->typeExpr), expr);
            }
            fprintf(context.out, "\t%s %%v%d, %%%.*s\n", pickStoreInst(stmt->assign.value->typeExpr), loc, 
                    (int) stmt->assign.var->id.len, stmt->assign.var->id.text);
        }
        else {
            if (copy) {
                fprintf(context.out, "\t%%%.*s =%s copy %s\n", (int)stmt->assign.var->id.len,
                        stmt->assign.var->id.text,
                        generateType(stmt->assign.value->typeExpr), expr);
            } else {
                fprintf(context.out, "\t%%%.*s =%s %s\n",
                        (int)stmt->assign.var->id.len, stmt->assign.var->id.text,
                        generateType(stmt->assign.value->typeExpr), expr);
            }
        }
        break;
    }
    case STMT_RETURN: {
        if (stmt->returnExp == NULL) {

            fprintf(context.out, "\tret\n");
        } else {
            char *expr = generateExpr(scope, stmt->returnExp, &copy);
            fprintf(context.out, "\tret %s\n", expr);
        }
        break;
    }
    case STMT_IF: {
        char* expr = generateExpr(scope, stmt->if_block.cond, &copy);
        int value1 = getNewNum();
        if (copy) {

            fprintf(context.out, "\t%%v%d =%s copy %s\n", value1, generateType(stmt->if_block.cond->typeExpr), expr);
        }
        else {
            fprintf(context.out, "\t%%v%d =%s %s\n", value1, generateType(stmt->if_block.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        fprintf(context.out, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1, loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_block.block, i));
            generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "@loc%d\n", loc2);
        break;
    }
    case STMT_IF_ELSE: {
        char* expr = generateExpr(scope, stmt->if_else.cond, &copy);
        int value1 = getNewNum();
        if (copy) {
            fprintf(context.out, "\t%%v%d =%s copy %s\n", value1, generateType(stmt->if_else.cond->typeExpr), expr);
        }
        else {
            fprintf(context.out, "\t%%v%d =%s %s\n", value1, generateType(stmt->if_else.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        int loc3 = getNewNum();
        fprintf(context.out, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1, loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_else.block1, i));
            generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "\tjmp @loc%d\n", loc3);
        fprintf(context.out, "@loc%d\n", loc2);
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
            Stmt* tempStmt = *((Stmt**)indexVector(stmt->if_else.block2, i));
            generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "@loc%d\n", loc3);
        break;
    }
    case STMT_DEC_ASSIGN: {
        TypedEntry* entry = stmt->dec_assign.var->data;
        char *expr = generateExpr(scope, stmt->dec_assign.value, &copy);

        if (entry->onStack) {
            fprintf(context.out, "\t%%%.*s =l alloc4 %ld\n", 
                    (int) stmt->dec_assign.var->id.len, 
                    stmt->dec_assign.var->id.text, 
                    calculateSize(stmt->dec_assign.type));
            int loc = getNewNum();

            if (copy) {
                fprintf(context.out, "\t%%v%d =%s copy %s\n", loc, generateType(stmt->dec_assign.type), expr);
            }
            else {
                fprintf(context.out, "\t%%v%d =%s %s\n", loc, generateType(stmt->dec_assign.type), expr);

            }

            fprintf(context.out, "\t%s %%v%d, %%%.*s\n", pickStoreInst(stmt->dec_assign.type), loc, (int) stmt->dec_assign.var->id.len, stmt->dec_assign.var->id.text);

        }

        else {
            if (copy) {
                fprintf(context.out, "\t%%%.*s =%s copy %s\n",
                        (int)stmt->dec_assign.var->id.len,
                        stmt->assign.var->id.text,
                        generateType(stmt->dec_assign.value->typeExpr), expr);
            } else {
                fprintf(context.out, "\t%%%.*s =%s %s\n",
                        (int)stmt->dec_assign.var->id.len,
                        stmt->assign.var->id.text,
                        generateType(stmt->dec_assign.value->typeExpr), expr);
            }
        }
        break;
    }
default:
                          printf("Sorry, cant codegen flow control yet.\n");
                          exit(1);
    }
}

static void generateFunction(Function *fn) {
    fprintf(context.out, "export function %s $%.*s(", generateType(fn->retType),
            (int)fn->name.len, fn->name.text);

    if (fn->params->numItems != 0) {
        for (size_t i = 0; i < fn->params->numItems - 1; i++) {
            Param param = *((Param *)indexVector(fn->params, i));
            fprintf(context.out, "%s %%%.*s, ", generateType(param.type),
                    (int)param.var->id.len, param.var->id.text);
        }

        Param param =
            *((Param *)indexVector(fn->params, fn->params->numItems - 1));
        fprintf(context.out, "%s %%%.*s", generateType(param.type),
                (int)param.var->id.len, param.var->id.text);
    }
    fprintf(context.out, ") {\n");
    fprintf(context.out, "@start\n");

    for (size_t i = 0; i < fn->stmts->numItems; i++) {
        Stmt *stmt = *((Stmt **)indexVector(fn->stmts, i));
        generateStatement(fn->scope, stmt);
    }

    fprintf(context.out, "}\n");
}

static void generateToplevel(Toplevel top) {

    switch (top.type) {

    case TOP_PROC:
        generateFunction(top.fn);
        break;
    case TOP_VAR:
        printf("Global variables not implemented yet.\n");
        exit(1);
    case TOP_EXTERN:
        break;
    }
}

void generateCode(AST *ast, FILE *file) {
    allocateToStack(ast);
    context.out = file;

    for (size_t i = 0; i < ast->decs->numItems; i++) {

        Toplevel top = *((Toplevel *)indexVector(ast->decs, i));
        generateToplevel(top);
    }
}
