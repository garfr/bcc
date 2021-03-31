#include <stdio.h>
#include <stdlib.h>

#include "bcc/utils.h"
#include "bcc/ast.h"
#include "bcc/error.h"
#include "bcc/semantics.h"

static void
resolveExpr(Scope *scope, Expr *exp) {
  switch (exp->type) {
    case EXP_FUNCALL:
      if (exp->funcall.entry == NULL) {
        HashEntry *entry = findInScope(scope, exp->funcall.name);
        if (entry == NULL) {
          queueError(msprintf("Could not find variable '%.*s' in scope.\n",
                              exp->funcall.name.len, exp->funcall.name.text),
                     exp->start, exp->end);
          printErrors();
        }

        exp->typeExpr = ((TypedEntry *)entry->data)->type->fun.retType;
        exp->funcall.entry = entry;
      }
      for (size_t i = 0; i < exp->funcall.arguments->numItems; i++) {
        resolveExpr(scope, *((Expr **)indexVector(exp->funcall.arguments, i)));
      }
      break;
    case EXP_BINOP:
      resolveExpr(scope, exp->binop.exp1);
      resolveExpr(scope, exp->binop.exp2);
      break;
    case EXP_ADDROF:
      resolveExpr(scope, exp->addr.expr);
      break;
    case EXP_DEREF:
      resolveExpr(scope, exp->deref);
      break;
    case EXP_INT:
    case EXP_BOOL:
    case EXP_VAR:
    case EXP_CHAR:
      break;
    case EXP_RECORDLIT:
      {
        for (size_t i = 0; i < exp->reclit.fields->entries; i++) {
          for (HashEntry *entry = exp->reclit.fields->buckets[i]; entry != NULL;
               entry = entry->next) {
            resolveExpr(scope, entry->data);
          }
        }
      }
  }
}

static void
resolveStmt(Scope *scope, Stmt *stmt) {
  switch (stmt->type) {
    case STMT_EXPR:
      resolveExpr(scope, stmt->singleExpr);
      break;
    case STMT_DEC:
      break;
    case STMT_ASSIGN:
      resolveExpr(scope, stmt->assign.value);
      break;
    case STMT_COMPOUND_ASSIGN:
      resolveExpr(scope, stmt->compound_assign.value);
      break;
    case STMT_IF:
      {

        resolveExpr(scope, stmt->if_block.cond);
        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_block.block, i));
          resolveStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_WHILE:
      {
        resolveExpr(scope, stmt->while_block.cond);
        for (size_t i = 0; i < stmt->while_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->while_block.block, i));
          resolveStmt(scope, tempStmt);
        }
        break;
      }
    case STMT_IF_ELSE:
      {

        resolveExpr(scope, stmt->if_else.cond);
        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block1, i));
          resolveStmt(scope, tempStmt);
        }
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block2, i));
          resolveStmt(scope, tempStmt);
        }
        break;
      }
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

void
resolveNames(AST *ast) {
  for (size_t i = 0; i < ast->decs->numItems; i++) {
    Toplevel toplevel = *((Toplevel *)indexVector(ast->decs, i));
    switch (toplevel.type) {
      case TOP_PROC:
        {
          Scope *scope = toplevel.fn->scope;
          for (size_t j = 0; j < toplevel.fn->stmts->numItems; j++) {
            Stmt *stmt = *((Stmt **)indexVector(toplevel.fn->stmts, j));
            resolveStmt(scope, stmt);
          }
        }
      case TOP_VAR:
      case TOP_EXTERN:
        break;
    }
  }
}
