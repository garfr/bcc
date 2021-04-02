#include "bcc/ast.h"
#include "bcc/codegen.h"
#include "bcc/error.h"

/* First a pass is made through the the AST to determine what variables need to
 * be on the stack Then a pass is made to calculate the position on the stack of
 * all of these variables
 */
static void
checkStackExpr(Scope *scope, Expr *exp) {
  switch (exp->type) {
    case EXP_ADDROF:
      {
        if (exp->addr.expr->type == EXP_VAR) {
          ((TypedEntry *)exp->addr.expr->var->data)->onStack = true;
        } else {
          queueError("Can only take the address of variables, indexes, and "
                     "struct fields",
                     exp->start, exp->end);
          checkStackExpr(scope, exp->addr.expr);
        }
        break;
      }
    case EXP_DEREF:
      checkStackExpr(scope, exp->deref);
      break;
    case EXP_ARRAY:
      {
        for (size_t i = 0; i < exp->array.items->numItems; i++) {
          Expr *tempExpr = *((Expr **)indexVector(exp->array.items, i));
          checkStackExpr(scope, tempExpr);
        }
        break;
      }
    case EXP_FUNCALL:
      {
        for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
          Expr *tempExp = *((Expr **)indexVector(exp->funcall.arguments, i));
          checkStackExpr(scope, tempExp);
        }
        break;
      }
    case EXP_BINOP:
      checkStackExpr(scope, exp->binop.exp1);
      checkStackExpr(scope, exp->binop.exp2);
      break;
    case EXP_INT:
    case EXP_BOOL:
    case EXP_VAR:
    case EXP_CHAR:
      break;
    case EXP_RECORDLIT:
      {
        for (size_t i = 0; i < exp->reclit.fields->numBuckets; i++) {
          for (HashEntry *entry = exp->reclit.fields->buckets[i]; entry != NULL;
               entry = entry->next) {
            checkStackExpr(scope, entry->data);
          }
        }
        break;
      }
  }
}

static void
checkStackStmt(Scope *scope, Stmt *stmt) {
  switch (stmt->type) {
    case STMT_EXPR:
      checkStackExpr(scope, stmt->singleExpr);
      break;
    case STMT_DEC:
      if (stmt->dec.type->type == TYP_ARRAY) {
        ((TypedEntry *)stmt->dec.var->data)->onStack = true;
      }
      break;
    case STMT_ASSIGN:
      checkStackExpr(scope, stmt->assign.value);
      break;
    case STMT_COMPOUND_ASSIGN:
      checkStackExpr(scope, stmt->compound_assign.value);
      break;
    case STMT_IF:
      {
        checkStackExpr(scope, stmt->if_block.cond);
        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_block.block, i));
          checkStackStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_WHILE:
      {
        checkStackExpr(scope, stmt->while_block.cond);
        for (size_t i = 0; i < stmt->while_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->while_block.block, i));
          checkStackStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_IF_ELSE:
      {
        checkStackExpr(scope, stmt->if_else.cond);
        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block1, i));
          checkStackStmt(scope, tempStmt);
        }
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block2, i));
          checkStackStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_RETURN:
      if (stmt->returnExp != NULL) {
        checkStackExpr(scope, stmt->returnExp);
      }
      break;
    case STMT_DEC_ASSIGN:
      checkStackExpr(scope, stmt->dec_assign.value);
      break;
  }
}

void
allocateToStack(AST *ast) {
  for (size_t i = 0; i < ast->decs->numItems; i++) {
    Toplevel toplevel = *((Toplevel *)indexVector(ast->decs, i));
    switch (toplevel.type) {
      case TOP_PROC:
        {
          Scope *scope = toplevel.fn->scope;
          for (size_t j = 0; j < toplevel.fn->stmts->numItems; j++) {
            Stmt *stmt = *((Stmt **)indexVector(toplevel.fn->stmts, j));
            checkStackStmt(scope, stmt);
          }
        }
      case TOP_VAR:
      case TOP_EXTERN:
        break;
    }
  }
}
