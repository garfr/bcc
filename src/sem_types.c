#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// clang-format off
#include "bcc/ast.h"
#include "bcc/error.h"
#include "bcc/utils.h"
#include "bcc/sem_types.h"
// clang-format off

/* Don't allocate the same type every time when using integer literals, just
 * point to this */
static Type *IntegerLit = &(Type){.type = TYP_INTLIT, {}};
static Type *BooleanLit = &(Type){.type = TYP_BOOL, {}};

// clang-format off

// clang-format on

/* This is only needed because the current error handling system does not
 * allow you to just pass a type and have the error printer call printType
 * This means an actual string must be allocated When the error system is
 * improved this can be deleted */
char *stringOfType(Type *type) {
    switch (type->type) {
    case TYP_S8:
        return "s8";
    case TYP_S16:
        return "s16";
    case TYP_S32:
        return "s32";
    case TYP_S64:
        return "s64";
    case TYP_U8:
        return "u8";
    case TYP_U16:
        return "u16";
    case TYP_U32:
        return "u32";
    case TYP_U64:
        return "u64";
    case TYP_VOID:
        return "void";
    case TYP_BOOL:
        return "bool";
    case TYP_BINDING:
        return msprintf("%.*s", (int)type->typeEntry->id.len,
                        type->typeEntry->id.text);

    case TYP_INTLIT:
        return "integer literal";
    case TYP_FUN:
        return "function pointer";
    case TYP_RECORD:
        return "record";
    }
    return NULL;
}

Type *coerceBinop(int op, Type *type1, Type *type2);

/* Coerces a two types to a binary type
 * For assignment, use coerceAssignment */
/* TODO: NOTE: THIS SUCKS ASS AND SHOULD BE BETTER BUT IDK MAN SOME TIMES IT BE
 * LIKE THAT */
Type *coerceBinop(int op, Type *type1, Type *type2) {
    assert(type1 != NULL);
    assert(type2 != NULL);

    if (type1->type == TYP_BINDING && type1->type == TYP_BINDING) {
        return coerceBinop(op, type1->typeEntry->data, type2->typeEntry->data);
    }
    if (type1->type == TYP_BINDING) {
        return coerceBinop(op, type1->typeEntry->data, type2);
    }
    if (type2->type == TYP_BINDING) {
        return coerceBinop(op, type1, type2->typeEntry->data);
    }
    if (type1->type == TYP_FUN || type1->type == TYP_RECORD) {
        return NULL;
    }
    switch (op) {
    case BINOP_ADD:
    case BINOP_SUB:
    case BINOP_MULT:
    case BINOP_DIV:
        if (type1->type == TYP_INTLIT) {
            switch (type2->type) {
            case TYP_S8:
            case TYP_S16:
            case TYP_S32:
            case TYP_S64:
            case TYP_U8:
            case TYP_U16:
            case TYP_U32:
            case TYP_U64:
            case TYP_INTLIT:
                return type2;
            default:
                return NULL;
            }
        }
        if (type2->type == TYP_INTLIT) {
            switch (type1->type) {
            case TYP_S8:
            case TYP_S16:
            case TYP_S32:
            case TYP_S64:
            case TYP_U8:
            case TYP_U16:
            case TYP_U32:
            case TYP_U64:
            case TYP_INTLIT:
                return type1;
            default:
                return NULL;
            }
        }
        if (type1->type == type2->type) {
            return type1;
        }
        return NULL;
    case BINOP_EQUAL:
        if (type1->type == TYP_INTLIT) {
            switch (type2->type) {
            case TYP_S8:
            case TYP_S16:
            case TYP_S32:
            case TYP_S64:
            case TYP_U8:
            case TYP_U16:
            case TYP_U32:
            case TYP_U64:
            case TYP_INTLIT:
                return BooleanLit;
            default:
                return NULL;
            }
        }
        if (type2->type == TYP_INTLIT) {
            switch (type1->type) {
            case TYP_S8:
            case TYP_S16:
            case TYP_S32:
            case TYP_S64:
            case TYP_U8:
            case TYP_U16:
            case TYP_U32:
            case TYP_U64:
            case TYP_INTLIT:
                return BooleanLit;
            default:
                return NULL;
            }
        }
        if (type1->type == type2->type) {
            return BooleanLit;
        }
        return NULL;

    case BINOP_AND:
    case BINOP_OR:
        if (type1->type != TYP_BOOL || type2->type != TYP_BOOL) {
            return NULL;
        }
        return BooleanLit;
    }
    /* This is unreachable */
    return NULL;
}

Type *coerceAssignment(Type *type1, Type *type2) {
    if (type1->type == TYP_FUN || type1->type == TYP_RECORD) {
        return NULL;
    }
    if (type1->type == TYP_BINDING && type1->type == TYP_BINDING) {
        return coerceAssignment(type1->typeEntry->data, type2->typeEntry->data);
    }
    if (type1->type == TYP_BINDING) {
        return coerceAssignment(type1->typeEntry->data, type2);
    }
    if (type2->type == TYP_BINDING) {
        return coerceAssignment(type1, type2->typeEntry->data);
    }

    if (type1->type == TYP_INTLIT) {
        switch (type2->type) {
        case TYP_S8:
        case TYP_S16:
        case TYP_S32:
        case TYP_S64:
        case TYP_U8:
        case TYP_U16:
        case TYP_U32:
        case TYP_U64:
        case TYP_INTLIT:
            return type2;
        default:
            return NULL;
        }
    }
    if (type2->type == TYP_INTLIT) {
        switch (type1->type) {
        case TYP_S8:
        case TYP_S16:
        case TYP_S32:
        case TYP_S64:
        case TYP_U8:
        case TYP_U16:
        case TYP_U32:
        case TYP_U64:
        case TYP_INTLIT:
            return type1;
        default:
            return NULL;
        }
    }
    if (type1->type == type2->type) {
        return type1;
    }
    return NULL;
}

void typeExpression(Scope *scope, Expr *exp) {
    assert(scope != NULL);
    assert(exp != NULL);

    switch (exp->type) {
    case EXP_FUNCALL: {
        Type *fun = ((TypedEntry *)exp->funcall.entry->data)->type;
        if (fun->fun.args->numItems != exp->funcall.arguments->numItems) {
            queueError(msprintf("Function call has %zd arguments, but %zd "
                                "were expected",
                                exp->funcall.arguments->numItems,
                                fun->fun.args->numItems),
                       exp->start, exp->end);
        }

        else {
            for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
                Expr *thisExp =
                    *((Expr **)indexVector(exp->funcall.arguments, i));
                typeExpression(scope, thisExp);
                Type *wantedType = *((Type **)indexVector(fun->fun.args, i));
                if (coerceAssignment(wantedType, thisExp->typeExpr) == NULL) {
                    queueError(msprintf("Function expected expression "
                                        "of type %s, not "
                                        "but got %s",
                                        stringOfType(wantedType),
                                        thisExp->typeExpr),
                               thisExp->start, thisExp->end);
                }
            }
        }
    } break;
    case EXP_INT:
        exp->typeExpr = IntegerLit;
        break;
    case EXP_BOOL:
        exp->typeExpr = BooleanLit;
        break;
    case EXP_VAR: {
        TypedEntry *entry = (TypedEntry *)exp->var->data;
        exp->typeExpr = entry->type;
        /* Check if the variable has been typed yet */
        assert(entry->type != NULL);
    } break;
    case EXP_BINOP: {
        typeExpression(scope, exp->binop.exp1);
        typeExpression(scope, exp->binop.exp2);
        Type *newType = coerceBinop(exp->binop.op, exp->binop.exp1->typeExpr,
                                    exp->binop.exp2->typeExpr);

        if (newType == NULL) {
            /* No continue mechanism for type errors yet, just print the
             * error and quit */
            queueError(msprintf("Cannot coerce type %s to %s",
                                stringOfType(exp->binop.exp1->typeExpr),
                                stringOfType(exp->binop.exp2->typeExpr)),
                       exp->binop.exp1->start, exp->binop.exp2->end);
            printErrors();
        }

        exp->typeExpr = newType;
    } break;
    case EXP_RECORDLIT: {
        Type *type = (Type *)exp->reclit.type->data;
        if (exp->reclit.fields->entries != type->record.recordFields->entries) {
            queueError("Supplies too many or two few fields for the record",
                       exp->start, exp->end);
            printErrors();
        }
        for (size_t i = 0; i < exp->reclit.fields->numBuckets; i++) {
            for (HashEntry *entry = exp->reclit.fields->buckets[i];
                 entry != NULL; entry = entry->next) {
                typeExpression(scope, entry->data);

                Expr *tempExpr = (Expr *)entry->data;

                HashEntry *otherEntry =
                    findHashtbl(type->record.recordFields, entry->id);

                if (otherEntry == NULL) {
                    queueError(msprintf("No field '%.*s' in record type '%.*s'",
                                        (int)entry->id.len, entry->id.text,
                                        (int)exp->reclit.type->id.len,
                                        exp->reclit.type->id.text),
                               exp->start, exp->end);
                    printErrors();
                }

                if (coerceAssignment(otherEntry->data, tempExpr->typeExpr) ==
                    NULL) {
                    queueError(
                        msprintf("Cannot coerce type '%s' to '%s', which is "
                                 "the type of record field '%.*s'",
                                 stringOfType(tempExpr->typeExpr),
                                 stringOfType(otherEntry->data),
                                 (int)entry->id.len, entry->id.text),
                        tempExpr->start, tempExpr->end);
                    printErrors();
                }
            }
        }
        exp->typeExpr = malloc(sizeof(Type));
        exp->typeExpr->type = TYP_BINDING;
        exp->typeExpr->typeEntry = exp->reclit.type;
    }
    }
}

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
    case TYP_U32:
        return 4;
    case TYP_S64:
    case TYP_U64:
        return 8;
    case TYP_VOID:
        return 0;
    case TYP_FUN:
        return 8;
    case TYP_INTLIT:
        printf("Internal compiler error: Cannot calculate size of integer "
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
    case TYP_BOOL:
        return 1;
    }
    printf("Internal compiler error: Reached end of calculateSize without "
           "returning.\n");
    exit(1);
}

/* Adds type information to a statment, including inserting any needed
 * information into the symbol table
 * Returns whether the stmt was a properly typed return statment */
bool typeStmt(Scope *scope, Stmt *stmt, Type *returnType, int64_t *stackSpace) {
    assert(scope != NULL);
    assert(stmt != NULL);

    switch (stmt->type) {
    /* This should have gotten typed before hand */
    case STMT_DEC: {
        TypedEntry *entry = stmt->dec.var->data;
        entry->stackOffset = *stackSpace;
        *stackSpace += calculateSize(entry->type);
    } break;

    case STMT_EXPR:
        typeExpression(scope, stmt->singleExpr);

        if (stmt->singleExpr->typeExpr->type != TYP_VOID) {
            queueError("Cannot discard returned value from expression "
                       "statement",
                       stmt->start, stmt->end);
        }
        break;
    case STMT_DEC_ASSIGN: {
        typeExpression(scope, stmt->dec_assign.value);

        Type *type;

        // The type was left to be inferred
        if (stmt->dec_assign.type == NULL) {
            if (stmt->dec_assign.value->typeExpr->type == TYP_INTLIT) {
                queueError(
                    msprintf("Cannot infer the type of a declaration from "
                             "only a integer literal"),
                    stmt->dec_assign.value->start, stmt->dec_assign.value->end);
                printErrors();
            }
            type = stmt->dec_assign.value->typeExpr;
            stmt->dec_assign.type = type;
        }

        else {
            type = coerceAssignment(stmt->dec_assign.type,
                                    stmt->dec_assign.value->typeExpr);
            if (type == NULL) {
                queueError(
                    msprintf("Cannot coerce type %s to %s",
                             stringOfType(stmt->dec_assign.value->typeExpr),
                             stringOfType(stmt->dec_assign.type)),
                    stmt->dec_assign.value->start, stmt->dec_assign.value->end);
                printErrors();
            }
        }

        TypedEntry *entry = stmt->dec_assign.var->data;
        entry->type = type;
        entry->stackOffset = *stackSpace;
        *stackSpace += calculateSize(entry->type);
    } break;
    case STMT_RETURN: {
        if (stmt->returnExp != NULL) {
            typeExpression(scope, stmt->returnExp);
            /* If the user wants to return a function call that has type
             * void */
            if (stmt->returnExp->typeExpr->type == TYP_VOID &&
                returnType->type == TYP_VOID) {
                return true;
            }
            if (coerceAssignment(returnType, stmt->returnExp->typeExpr) ==
                NULL) {
                queueError(msprintf("Cannot return type %s in a function "
                                    "that returns %s",
                                    stringOfType(stmt->returnExp->typeExpr),
                                    stringOfType(returnType)),
                           stmt->start, stmt->end);
                return false;
            }
            return true;
        } else {
            if (returnType->type == TYP_VOID) {
                return true;
            } else {
                queueError("Cannot return actual value in a function that "
                           "returns "
                           "void",
                           stmt->start, stmt->end);
                return false;
            }
        }
    } break;
    case STMT_ASSIGN: {
        typeExpression(scope, stmt->assign.value);

        assert(stmt->assign.var->data != NULL);

        TypedEntry *entry = (TypedEntry *)stmt->assign.var->data;

        if (!entry->isMut) {
            queueError(msprintf("Cannot assign to immutable variable '%.*s'",
                                stmt->assign.var->id.len,
                                stmt->assign.var->id.text),
                       stmt->start, stmt->end);
            return false;
        }

        Type *type =
            coerceAssignment(entry->type, stmt->assign.value->typeExpr);

        if (type == NULL) {
            queueError(msprintf("Cannot coerce type %s to %s",
                                stringOfType(entry->type),
                                stringOfType(stmt->assign.value->typeExpr)),
                       stmt->assign.value->start, stmt->assign.value->end);
            printErrors();
        }
        break;
    }
    }
    return false;
}

void typeToplevel(Toplevel *top) {
    switch (top->type) {
    case TOP_VAR:
        printf("Internal compiler error: No global variable support "
               "yet.\n");
        exit(1);
    case TOP_EXTERN:
        break;
    case TOP_PROC: {
        bool returnsCorrectly = false;
        int64_t stackSize = 0;
        for (size_t i = 0; i < top->fn->stmts->numItems; i++) {
            returnsCorrectly = typeStmt(
                top->fn->scope, *((Stmt **)indexVector(top->fn->stmts, i)),
                top->fn->retType, &stackSize);
        }

        if (!returnsCorrectly && top->fn->retType->type != TYP_VOID) {
            queueError("Function never returns a correct type", top->fn->start,
                       top->fn->start);
        }
    }
    }
}

void annotateAST(AST *ast) {
    for (size_t i = 0; i < ast->decs->numItems; i++) {
        typeToplevel(indexVector(ast->decs, i));
    }
}
