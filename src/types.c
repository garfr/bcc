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
            return msprintf("TYP_SINT: 's%ld'", type->intsize * 8);
        case TYP_INTLIT:
            return msprintf("TYP_INTLIT");
    }
    return NULL;
}

/* Coerces a two types to a binary type
 * For assignment, use coerceAssignment */
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
                        case TYP_INTLIT:
                            return type1;
                    }
                    break;
                case TYP_INTLIT:
                    switch (type2->type) {
                        case TYP_SINT:
                        case TYP_INTLIT:
                            return type1;
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
                case TYP_INTLIT:
                    return type1;
            }
            break;
        case TYP_INTLIT:
            /* An integer literal should not be on the left side of an
             * assignment */
            return NULL;
    }
    return NULL;
}

void typeExpression(Scope* scope, Expr* exp) {
    assert(scope != NULL);
    assert(exp != NULL);

    switch (exp->type) {
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
    }
}

/* Adds type information to a statment, including inserting any needed
 * information into the symbol table */
void typeStmt(Scope* scope, Stmt* stmt) {
    assert(scope != NULL);
    assert(stmt != NULL);

    switch (stmt->type) {
        /* This should have gotten typed before hand */
        case STMT_DEC:
            break;
        case STMT_DEC_ASSIGN: {
            typeExpression(scope, stmt->dec_assign.value);

            Type* type;
            // The type is inferred
            if (stmt->dec_assign.type == NULL) {
                if (stmt->dec_assign.value->typeExpr->type) {
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
                    queueError(msprintf("Cannot coerce type %s to %s",
                                        stringOfType(stmt->dec_assign.type),
                                        stringOfType(
                                            stmt->dec_assign.value->typeExpr)),
                               stmt->dec_assign.value->start,
                               stmt->dec_assign.value->end);
                    printErrors();
                }
            }

            TypedEntry* entry = malloc(sizeof(TypedEntry));
            entry->type = type;
            stmt->dec_assign.var->data = entry;
        } break;

        case STMT_ASSIGN: {
            typeExpression(scope, stmt->assign.value);

            assert(stmt->assign.var->data != NULL);

            TypedEntry* entry = (TypedEntry*)stmt->assign.var->data;

            Type* type =
                coerceAssignment(entry->type, stmt->assign.value->typeExpr);

            if (type == NULL) {
                queueError(
                    msprintf("Cannot coerce type %s to %s",
                             stringOfType(stmt->dec_assign.type),
                             stringOfType(stmt->dec_assign.value->typeExpr)),
                    stmt->dec_assign.value->start, stmt->dec_assign.value->end);
                printErrors();
            }
            break;
        }
    }
}

void annotateAST(AST* ast) {
    for (size_t i = 0; i < ast->stmts->numItems; i++) {
        typeStmt(ast->globalScope, *(Stmt**)indexVector(ast->stmts, i));
    }
}
