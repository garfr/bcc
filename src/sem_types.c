#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "bcc/ast.h"
#include "bcc/pp.h"
#include "bcc/error.h"
#include "bcc/utils.h"
#include "bcc/semantics.h"

/* Don't allocate the same type every time when using integer literals, just
 * point to this */
static Type *IntegerLit = &(Type){.type = TYP_INTLIT, {}};
static Type *BooleanLit = &(Type){.type = TYP_BOOL, {}};
static Type *CharLit = &(Type){.type = TYP_CHAR, {}};
static Type *U64Lit = &(Type){.type = TYP_U64, {}};

/* This is only needed because the current error handling system does not
 * allow you to just pass a type and have the error printer call printType
 * This means an actual string must be allocated When the error system is
 * improved this can be deleted */
static char *
stringOfType(Type *type) {
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
    case TYP_CHAR:
      return "char";
    case TYP_U64:
      return "u64";
    case TYP_VOID:
      return "void";
    case TYP_BOOL:
      return "bool";
    case TYP_ARRAY:
      return msprintf("[%ld]%s", type->array.size,
                      stringOfType(type->array.type));
    case TYP_PTR:
      if (type->ptr.mut) {
        return msprintf("&mut %s", stringOfType(type->ptr.type));
      }
      return msprintf("&%s", stringOfType(type->ptr.type));
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
  assert(false);
  return NULL;
}

/* Coerces a two types to a binary type
 * For assignment, use coerceAssignment */
/* TODO: NOTE: THIS SUCKS ASS AND SHOULD BE BETTER BUT IDK MAN SOME TIMES IT BE
 * LIKE THAT */
Type *
coerceBinop(int op, Type *type1, Type *type2) {
  assert(type1 != NULL);
  assert(type2 != NULL);

  if (type1->type == TYP_ARRAY || type2->type == TYP_ARRAY) {
    return NULL; // There are no binary ops with arrays
  }
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
  if (type1->type == TYP_PTR && type2->type == TYP_PTR) {
    if (type1->ptr.mut != type2->ptr.mut) {
      return NULL;
    }
    /*printType(type1->ptrType);*/
    /*printType(type2->ptrType);*/
    return coerceBinop(op, type1->ptr.type, type2->ptr.type);
  }
  switch (op) {
    case BINOP_ADD:
    case BINOP_SUB:
    case BINOP_MUL:
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
    case BINOP_NOTEQUAL:
    case BINOP_LESS:
    case BINOP_LESS_EQ:
    case BINOP_GREAT:
    case BINOP_GREAT_EQ:
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

Type *
coerceAssignment(Type *type1, Type *type2) {
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
  if (type1->type == TYP_PTR && type2->type == TYP_PTR) {
    if (type1->ptr.mut != type2->ptr.mut) {
      return NULL;
    }

    return coerceAssignment(type1->ptr.type, type2->ptr.type);
  }
  if (type1->type == TYP_ARRAY && type2->type == TYP_ARRAY) {
    if (type1->array.size != type2->array.size) {
      if (type1->array.size == -1) {
        type1->array.size = type2->array.size; // Set the inferred type to what
      } else {
        return NULL;
      }
    }
    if (coerceAssignment(type1->array.type, type2->array.type) == NULL) {
      return NULL;
    }
  }

  if (type1->type == type2->type) {
    return type1;
  }
  return NULL;
}

static void
typeExpression(Scope *scope, Expr *exp) {
  assert(scope != NULL);
  assert(exp != NULL);

  switch (exp->type) {
    case EXP_FUNCALL:
      {
        exp->typeExpr =
            ((TypedEntry *)exp->funcall.entry->data)->type->fun.retType;
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
            Expr *thisExp = *((Expr **)indexVector(exp->funcall.arguments, i));
            typeExpression(scope, thisExp);
            Type *wantedType = *((Type **)indexVector(fun->fun.args, i));
            if (coerceAssignment(wantedType, thisExp->typeExpr) == NULL) {
              queueError(msprintf("Function expected expression "
                                  "of type '%s', "
                                  "but got '%s'",
                                  stringOfType(wantedType),
                                  stringOfType(thisExp->typeExpr)),
                         thisExp->start, thisExp->end);
            }
          }
        }
      }
      break;
    case EXP_INDEX:
      {
        typeExpression(scope, exp->index.lval);
        typeExpression(scope, exp->index.indexVal);

        if (coerceAssignment(U64Lit, exp->index.indexVal->typeExpr) == NULL) {
          queueError("Only 'u64' can be used to index arrays and slices",
                     exp->index.indexVal->start, exp->index.indexVal->end);
        }

        int type = exp->index.lval->typeExpr->type;
        if (type != TYP_ARRAY) {
          queueError("You can only index arrays", exp->index.lval->start,
                     exp->index.lval->end);
          printErrors();
        }

        exp->typeExpr = exp->index.lval->typeExpr->array.type;
        break;
      }
    case EXP_ARRAY:
      {
        if (exp->array.items->numItems > (size_t)exp->array.type->array.size) {
          queueError("Array literal is larger than the given array size",
                     exp->start, exp->end);
          printErrors();
        } else if (exp->array.items->numItems <
                   (size_t)exp->array.type->array.size) {
          queueError("Not every item in array is initialized", exp->start,
                     exp->end);
          printErrors();
        }

        for (size_t i = 0; i < exp->array.items->numItems; i++) {
          Expr *tempExpr = *((Expr **)indexVector(exp->array.items, i));
          typeExpression(scope, tempExpr);
          if (coerceAssignment(exp->array.type->array.type,
                               tempExpr->typeExpr) == NULL) {
            queueError(msprintf("Cannot coerce type '%s' to type '%s'",
                                stringOfType(exp->array.type),
                                stringOfType(tempExpr->typeExpr)),
                       tempExpr->start, tempExpr->end);
          }
        }
        if (errorsExist()) {
          printErrors();
        }
        exp->typeExpr = exp->array.type;
        break;
      }
    case EXP_ADDROF:
      {
        if (exp->addr.expr->type != EXP_VAR) {
          queueError("Can only take the address of a value that is guaranteed "
                     "to be on the stack",
                     exp->start, exp->end);
          printErrors();
        }

        if (!((TypedEntry *)exp->addr.expr->var->data)->isMut &&
            exp->addr.mut) {
          queueError("Cannot take a mutable reference to an immutable variable",
                     exp->start, exp->end);
          printErrors();
        }
        typeExpression(scope, exp->addr.expr);
        Type *newType = calloc(1, sizeof(Type));
        newType->type = TYP_PTR;
        newType->ptr.type = exp->addr.expr->typeExpr;
        newType->ptr.mut = exp->addr.mut;
        exp->typeExpr = newType;
        break;
      }
    case EXP_DEREF:
      {
        typeExpression(scope, exp->deref);
        if (exp->deref->typeExpr->type != TYP_PTR) {
          queueError(msprintf("Cannot dereference type '%s'",
                              stringOfType(exp->deref->typeExpr)),
                     exp->start, exp->end);
          printErrors();
        }
        exp->typeExpr = exp->deref->typeExpr->ptr.type;
        break;
      }
    case EXP_INT:
      exp->typeExpr = IntegerLit;
      break;
    case EXP_CHAR:
      exp->typeExpr = CharLit;
      break;
    case EXP_BOOL:
      exp->typeExpr = BooleanLit;
      break;
    case EXP_VAR:
      {
        TypedEntry *entry = (TypedEntry *)exp->var->data;
        exp->typeExpr = entry->type;
        /* Check if the variable has been typed yet */
        assert(entry->type != NULL);
      }
      break;
    case EXP_BINOP:
      {
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
      }
      break;
    case EXP_RECORDLIT:
      {
        Type *type = (Type *)exp->reclit.type->data;
        if (exp->reclit.fields->entries != type->record.recordFields->entries) {
          queueError("Supplies too many or two few fields for the record",
                     exp->start, exp->end);
          printErrors();
        }
        for (size_t i = 0; i < exp->reclit.fields->numBuckets; i++) {
          for (HashEntry *entry = exp->reclit.fields->buckets[i]; entry != NULL;
               entry = entry->next) {
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
              queueError(msprintf("Cannot coerce type '%s' to '%s', which is "
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

/* Returns the place in the symbol table */
static TypedEntry *
getLValEntry(LVal *lval) {
  switch (lval->type) {
    case LVAL_VAR:
      return lval->var.entry->data;
    case LVAL_DEREF:
      return lval->deref.entry->data;
  }
  assert(false);
  exit(1);
}

/* Returns the place in the symbol table */
static Symbol
getLValSym(LVal *lval) {
  switch (lval->type) {
    case LVAL_VAR:
      return lval->var.sym;
    case LVAL_DEREF:
      return lval->deref.sym;
  }
  assert(false);
  exit(1);
}

/* Adds type information to a statment, including inserting any needed
 * information into the symbol table */
static void
typeStmt(Scope *scope, Stmt *stmt) {
  assert(scope != NULL);
  assert(stmt != NULL);

  switch (stmt->type) {
    /* This should have gotten typed before hand */
    case STMT_DEC:
      if (stmt->dec.type->type == TYP_ARRAY) {
        if (stmt->dec.type->array.size == -1) {
          queueError("Cannot infer size of an array without assigning to it",
                     stmt->start, stmt->end);
          printErrors();
        }
      }
      break;

    case STMT_EXPR:
      typeExpression(scope, stmt->singleExpr);

      if (stmt->singleExpr->typeExpr->type != TYP_VOID) {
        queueError("Cannot discard returned value from expression "
                   "statement",
                   stmt->start, stmt->end);
      }
      break;

    case STMT_IF:
      {
        typeExpression(scope, stmt->if_block.cond);

        if (stmt->if_block.cond->typeExpr->type != TYP_BOOL) {
          queueError("Expression in 'if' statements must be a boolean",
                     stmt->if_block.cond->start, stmt->if_block.cond->end);
        }

        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_block.block, i));
          typeStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_WHILE:
      {
        typeExpression(scope, stmt->while_block.cond);

        if (stmt->while_block.cond->typeExpr->type != TYP_BOOL) {
          queueError("Expression in 'while' statements must be a boolean",
                     stmt->while_block.cond->start,
                     stmt->while_block.cond->end);
        }

        for (size_t i = 0; i < stmt->while_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->while_block.block, i));
          typeStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_IF_ELSE:
      {
        typeExpression(scope, stmt->if_else.cond);

        if (stmt->if_else.cond->typeExpr->type != TYP_BOOL) {
          queueError("Expression in 'if' statements must be a boolean",
                     stmt->if_else.cond->start, stmt->if_else.cond->end);
          printErrors();
        }

        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block1, i));
          typeStmt(scope, tempStmt);
        }
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block2, i));
          typeStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_DEC_ASSIGN:
      {
        typeExpression(scope, stmt->dec_assign.value);

        Type *type;

        // The type was left to be inferred
        if (stmt->dec_assign.type == NULL) {
          if (stmt->dec_assign.value->typeExpr->type == TYP_INTLIT) {
            queueError(msprintf("Cannot infer the type of a declaration from "
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
                                stringOfType(stmt->dec_assign.value->typeExpr),
                                stringOfType(stmt->dec_assign.type)),
                       stmt->dec_assign.value->start,
                       stmt->dec_assign.value->end);
            printErrors();
          }
        }

        TypedEntry *entry = stmt->dec_assign.var->data;
        entry->type = type;
      }
      if (stmt->dec_assign.type->type == TYP_ARRAY) {
        if (stmt->dec_assign.value->type != EXP_ARRAY) {
          queueError("Cannot assign a non-array literal to an array. Maybe you "
                     "want a slice?",
                     stmt->start, stmt->end);
          printErrors();
        }
      }
      break;
    case STMT_RETURN:
      {
        if (stmt->returnExp != NULL) {
          typeExpression(scope, stmt->returnExp);
        }
      }
      break;
    case STMT_ASSIGN:
      {
        typeExpression(scope, stmt->assign.value);

        TypedEntry *entry = getLValEntry(stmt->assign.lval);

        Type *type;
        if (stmt->assign.value->type == EXP_ARRAY) {
          queueError("Cannot assign a stack allocated array to an already "
                     "initialized variable",
                     stmt->start, stmt->end);
        }

        if (stmt->assign.lval->type == LVAL_VAR) {
          type = coerceAssignment(entry->type, stmt->assign.value->typeExpr);
          if (!entry->isMut) {
            queueError(msprintf("Cannot assign to immutable variable '%.*s'",
                                (int)getLValSym(stmt->assign.lval).len,
                                getLValSym(stmt->assign.lval).text),
                       stmt->start, stmt->end);
            return;
          }
        } else if (stmt->assign.lval->type == LVAL_DEREF) {
          if (entry->type->type != TYP_PTR) {
            queueError("Cannot assign to address of non-pointer variable",
                       stmt->start, stmt->end);
            printErrors();
          }
          if (!entry->type->ptr.mut) {
            queueError("Cannot assign to immutable pointer to variable",
                       stmt->start, stmt->end);
          }
          type = coerceAssignment(entry->type->ptr.type,
                                  stmt->assign.value->typeExpr);
        } else {
          assert(false);
          exit(1);
        }

        if (type == NULL) {
          queueError(msprintf("Cannot coerce type %s to %s",
                              stringOfType(entry->type),
                              stringOfType(stmt->assign.value->typeExpr)),
                     stmt->assign.value->start, stmt->assign.value->end);
          printErrors();
        }
        break;
      }
    case STMT_COMPOUND_ASSIGN:
      {
        typeExpression(scope, stmt->compound_assign.value);

        TypedEntry *entry = getLValEntry(stmt->compound_assign.lval);

        Type *type;

        if (stmt->compound_assign.lval->type == LVAL_VAR) {
          type = coerceAssignment(entry->type,
                                  stmt->compound_assign.value->typeExpr);
          if (!entry->isMut) {
            queueError(msprintf("Cannot assign to immutable variable '%.*s'",
                                (int)getLValSym(stmt->compound_assign.lval).len,
                                getLValSym(stmt->compound_assign.lval).text),
                       stmt->start, stmt->end);
            return;
          }
        } else if (stmt->compound_assign.lval->type == LVAL_DEREF) {
          if (entry->type->type != TYP_PTR) {
            queueError("Cannot assign to address of non-pointer variable",
                       stmt->start, stmt->end);
            printErrors();
          }
          if (!entry->type->ptr.mut) {
            queueError("Cannot assign to immutable pointer to variable",
                       stmt->start, stmt->end);
          }
          type = coerceBinop(stmt->compound_assign.op, entry->type->ptr.type,
                             stmt->compound_assign.value->typeExpr);
        } else {
          assert(false);
          exit(1);
        }

        if (type == NULL) {
          queueError(
              msprintf("Cannot coerce type %s to %s", stringOfType(entry->type),
                       stringOfType(stmt->compound_assign.value->typeExpr)),
              stmt->compound_assign.value->start,
              stmt->compound_assign.value->end);
          printErrors();
        }
        break;
      }
  }
}

static void
typeToplevel(Toplevel *top) {
  switch (top->type) {
    case TOP_VAR:
      printf("Internal compiler error: No global variable support "
             "yet.\n");
      exit(1);
    case TOP_EXTERN:
      break;
    case TOP_PROC:
      {
        for (size_t i = 0; i < top->fn->stmts->numItems; i++) {
          typeStmt(top->fn->scope, *((Stmt **)indexVector(top->fn->stmts, i)));
        }
      }
  }
}

void
annotateAST(AST *ast) {
  for (size_t i = 0; i < ast->decs->numItems; i++) {
    typeToplevel(indexVector(ast->decs, i));
  }
}
