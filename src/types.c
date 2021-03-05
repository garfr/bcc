//===---------- types.c - Defines semantics and annotates the AST --------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// This file defines the semantics and rules of both builtin and user defined
// types, and provides the functionality for traversing and annotating the AST.
//
//===------------------------------ Todo ---------------------------------===//
//
// * Find a better way to handle errors here
//
//===---------------------------------------------------------------------===//

#include <assert.h>
#include <ast.h>
#include <error.h>
#include <stdio.h>
#include <stdlib.h>
#include <types.h>
#include <utils.h>

/* Don't malloc the same type every time when using integer literals, just point
 * to this */
static Type* IntegerLit = &(Type){.type = TYP_INTLIT, {}};

/* This is only needed because the current error handling system does not allow
 * you to just pass a type and have the error printer call printType This means
 * an actual string must be allocated
 * When the error system is improved this can be deleted */
char* stringOfType(Type* type) {
    switch (type->type) {
        case TYP_SINT:
            return msprintf("s%ld", type->intsize * 8);
        case TYP_VOID:
            return "void";
        case TYP_UINT:
            return msprintf("u%ld", type->intsize * 8);
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

Type* coerceBinop(int op, Type* type1, Type* type2);

bool compareRecords(Hashtbl* fields1, Hashtbl* fields2) {
    if (fields1->numBuckets != fields2->numBuckets) {
        return false;
    }
    // Iterate through the hashtable, this is what C++ is for lol
    for (size_t i = 0; i < fields1->numBuckets; i++) {
        for (HashEntry* entry = fields1->buckets[i]; entry != NULL;
             entry = entry->next) {
            HashEntry* otherEntry = findHashtbl(fields2, entry->id);
            if (otherEntry == NULL) {
                return false;
            }
            if (coerceBinop(BINOP_ADD, entry->data, otherEntry->data) == NULL) {
                return false;
            }
        }
    }
    return true;
}

/* Coerces a two types to a binary type
 * For assignment, use coerceAssignment */
/* TODO: NOTE: THIS SUCKS ASS AND SHOULD BE BETTER BUT IDK MAN SOME TIMES IT BE
 * LIKE THAT */
Type* coerceBinop(int op, Type* type1, Type* type2) {
    assert(type1 != NULL);
    assert(type2 != NULL);
    assert(op == BINOP_ADD || op == BINOP_SUB || op == BINOP_MULT ||
           op == BINOP_DIV);

    switch (op) {
        case BINOP_ADD:
        case BINOP_SUB:
        case BINOP_MULT:
        case BINOP_DIV:
            switch (type1->type) {
                case TYP_SINT:
                    switch (type2->type) {
                        case TYP_SINT:
                            if (type1->intsize == type2->intsize) {
                                return type1;
                            }
                            return NULL;
                        case TYP_BINDING:
                            return coerceBinop(op, type1,
                                               type2->typeEntry->data);
                        case TYP_UINT:
                        case TYP_VOID:
                        case TYP_FUN:
                        case TYP_RECORD:
                            return NULL;
                        case TYP_INTLIT:
                            return type1;
                    }
                    break;
                case TYP_UINT:
                    switch (type2->type) {
                        case TYP_UINT:
                            if (type1->intsize == type2->intsize) {
                                return type1;
                            }
                            return NULL;
                        case TYP_SINT:
                        case TYP_RECORD:
                        case TYP_FUN:
                        case TYP_VOID:
                            return NULL;
                        case TYP_BINDING:
                            return coerceBinop(op, type1,
                                               type2->typeEntry->data);
                        case TYP_INTLIT:
                            return type1;
                    }
                    break;
                case TYP_INTLIT:
                    switch (type2->type) {
                        case TYP_SINT:
                        case TYP_UINT:
                        case TYP_INTLIT:
                            return type1;
                        case TYP_VOID:
                        case TYP_RECORD:
                        case TYP_FUN:
                            return NULL;
                        case TYP_BINDING:
                            return coerceBinop(op, type1,
                                               type2->typeEntry->data);
                    }
                    break;
                case TYP_VOID:
                    return NULL;
                case TYP_FUN:
                    switch (type2->type) {
                        case TYP_SINT:
                        case TYP_UINT:
                        case TYP_INTLIT:
                        case TYP_VOID:
                        case TYP_FUN:
                        case TYP_RECORD:
                            return NULL;
                        case TYP_BINDING:
                            return coerceBinop(op, type1,
                                               type2->typeEntry->data);
                    }
                    break;
                case TYP_BINDING:
                    switch (type2->type) {
                        case TYP_SINT:
                        case TYP_UINT:
                        case TYP_RECORD:
                        case TYP_INTLIT:
                        case TYP_VOID:
                        case TYP_FUN:
                            return NULL;
                        case TYP_BINDING:
                            if (compareSymbol(type1->typeEntry->id,
                                              type2->typeEntry->id)) {
                                return type1;
                            }
                            return coerceBinop(op, type1->typeEntry->data,
                                               type2->typeEntry->data);
                    }
                    break;
                case TYP_RECORD:
                    switch (type2->type) {
                        case TYP_SINT:
                        case TYP_UINT:
                        case TYP_INTLIT:
                        case TYP_VOID:
                        case TYP_FUN:
                            return NULL;
                        case TYP_BINDING:
                            return coerceBinop(op, type1->typeEntry->data,
                                               type2->typeEntry->data);
                        case TYP_RECORD:
                            if (type2->type == TYP_RECORD) {
                                if (compareRecords(type1->recordFields,
                                                   type2->recordFields)) {
                                    return type1;
                                }
                            }
                            return NULL;
                    }
            }
            break;
    }
    /* This is unreachable */
    return NULL;
}

Type* coerceAssignment(Type* type1, Type* type2) {
    switch (type1->type) {
        case TYP_SINT:
            switch (type2->type) {
                case TYP_SINT:
                    if (type1->intsize == type2->intsize) {
                        return type1;
                    }
                    return NULL;
                case TYP_UINT:
                case TYP_VOID:
                case TYP_FUN:
                case TYP_RECORD:
                    return NULL;
                case TYP_INTLIT:
                    return type1;
                case TYP_BINDING:
                    return coerceAssignment(type1, type2->typeEntry->data);
            }
            break;
        case TYP_UINT:
            switch (type2->type) {
                case TYP_UINT:
                    if (type1->intsize == type2->intsize) {
                        return type1;
                    }
                    return NULL;
                case TYP_RECORD:
                case TYP_SINT:
                case TYP_FUN:
                case TYP_VOID:
                    return NULL;
                case TYP_INTLIT:
                    return type1;
                case TYP_BINDING:
                    return coerceAssignment(type1, type2->typeEntry->data);
            }
            break;
        case TYP_BINDING:
            switch (type2->type) {
                case TYP_UINT:
                case TYP_SINT:
                case TYP_FUN:
                case TYP_VOID:
                case TYP_RECORD:
                case TYP_INTLIT:
                    return coerceAssignment(type1->typeEntry->data, type2);
                case TYP_BINDING:
                    return coerceAssignment(type1->typeEntry->data,
                                            type2->typeEntry->data);
            }
            break;
        case TYP_VOID:
            if (type2->type == TYP_BINDING) {
                return coerceAssignment(type1, type2->typeEntry->data);
            }
            /* An integer literal should not be on the left side of an
             * assignment */
            return NULL;
        case TYP_RECORD:
            if (type2->type == TYP_RECORD) {
                if (compareRecords(type1->recordFields, type2->recordFields)) {
                    return type1;
                }
            }
            return NULL;
        case TYP_INTLIT:
        case TYP_FUN:
            return NULL;
    }
    return NULL;
}

void typeExpression(Scope* scope, Expr* exp) {
    assert(scope != NULL);
    assert(exp != NULL);

    switch (exp->type) {
        case EXP_FUNCALL: {
            Type* fun = ((TypedEntry*)exp->funcall.name->data)->type;
            if (fun->fun.args->numItems != exp->funcall.arguments->numItems) {
                queueError(msprintf("Function call has %zd arguments, but %zd "
                                    "were expected",
                                    exp->funcall.arguments->numItems,
                                    fun->fun.args->numItems),
                           exp->start, exp->end);
            }

            else {
                for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
                    Expr* thisExp =
                        *((Expr**)indexVector(exp->funcall.arguments, i));
                    typeExpression(scope, thisExp);
                    Type* wantedType = *((Type**)indexVector(fun->fun.args, i));
                    if (coerceAssignment(wantedType, thisExp->typeExpr) ==
                        NULL) {
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
            exp->typeExpr->type = TYP_INTLIT;
            break;
        case EXP_VAR: {
            TypedEntry* entry = (TypedEntry*)exp->var->data;
            exp->typeExpr = entry->type;
            /* Check if the variable has been typed yet */
            assert(entry->type != NULL);
        } break;
        case EXP_BINOP: {
            typeExpression(scope, exp->binop.exp1);
            typeExpression(scope, exp->binop.exp2);
            Type* newType =
                coerceBinop(exp->binop.op, exp->binop.exp1->typeExpr,
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
            Type* type = (Type*)exp->reclit.type->data;
            if (exp->reclit.fields->numBuckets !=
                type->recordFields->numBuckets) {
                queueError("Supplies too many or two few fields for the record",
                           exp->start, exp->end);
                printErrors();
            }
            for (size_t i = 0; i < exp->reclit.fields->numBuckets; i++) {
                for (HashEntry* entry = exp->reclit.fields->buckets[i];
                     entry != NULL; entry = entry->next) {
                    typeExpression(scope, entry->data);
                    HashEntry* otherEntry =
                        findHashtbl(type->recordFields, entry->id);
                    Expr* expr = (Expr*)entry->data;
                    if (coerceAssignment(otherEntry->data, expr->typeExpr) ==
                        NULL) {
                        queueError(
                            msprintf("Cannot coerce type %s to %s, which is "
                                     "the type of record field %.*s",
                                     stringOfType(expr->typeExpr),
                                     stringOfType(otherEntry->data),
                                     (int)entry->id.len, entry->id.text),
                            expr->start, expr->end);
                        printErrors();
                    }
                }
            }
            exp->typeExpr = exp->reclit.type->data;
        }
    }
}

/* Adds type information to a statment, including inserting any needed
 * information into the symbol table
 * Returns whether the stmt was a properly typed return statment */
bool typeStmt(Scope* scope, Stmt* stmt, Type* returnType) {
    assert(scope != NULL);
    assert(stmt != NULL);

    switch (stmt->type) {
        /* This should have gotten typed before hand */
        case STMT_DEC:
            break;
        case STMT_EXPR:
            typeExpression(scope, stmt->singleExpr);

            if (stmt->singleExpr->typeExpr->type != TYP_VOID) {
                queueError(
                    "Cannot discard returned value from expression "
                    "statement",
                    stmt->start, stmt->end);
            }
            break;
        case STMT_DEC_ASSIGN: {
            typeExpression(scope, stmt->dec_assign.value);

            Type* type;

            // The type was left to be inferred
            if (stmt->dec_assign.type == NULL) {
                if (stmt->dec_assign.value->typeExpr->type == TYP_INTLIT) {
                    queueError(
                        msprintf("Cannot infer the type of a declaration from "
                                 "only a integer literal"),
                        stmt->dec_assign.value->start,
                        stmt->dec_assign.value->end);
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
                        stmt->dec_assign.value->start,
                        stmt->dec_assign.value->end);
                    printErrors();
                }
            }

            TypedEntry* entry = stmt->dec_assign.var->data;
            entry->type = type;
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
                    queueError(
                        "Cannot return actual value in a function that "
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

            TypedEntry* entry = (TypedEntry*)stmt->assign.var->data;

            if (!entry->isMut) {
                queueError(
                    msprintf("Cannot assign to immutable variable '%.*s'",
                             stmt->assign.var->id.len,
                             stmt->assign.var->id.text),
                    stmt->start, stmt->end);
                return false;
            }

            Type* type =
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

void typeToplevel(Toplevel* top) {
    switch (top->type) {
        case TOP_VAR:
            printf(
                "Internal compiler error: No global variable support "
                "yet.\n");
            exit(1);
        case TOP_PROC: {
            bool returnsCorrectly = false;
            for (size_t i = 0; i < top->fn->stmts->numItems; i++) {
                returnsCorrectly = typeStmt(
                    top->fn->scope, *((Stmt**)indexVector(top->fn->stmts, i)),
                    top->fn->retType);
            }

            if (!returnsCorrectly && top->fn->retType->type != TYP_VOID) {
                queueError("Function never returns a correct type",
                           top->fn->end, top->fn->end);
            }
        }
    }
}

void annotateAST(AST* ast) {
    for (size_t i = 0; i < ast->decs->numItems; i++) {
        typeToplevel(indexVector(ast->decs, i));
    }
}
