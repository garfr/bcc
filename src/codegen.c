#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "bcc/codegen.h"
#include "bcc/pp.h"
#include "bcc/semantics.h"
#include "bcc/utils.h"

/* Notes: Throughout this file, a pointer to a bool called "needsCopy" is passed
 * around.  This is because certain returned expressions require a copy to be
 * assigned to IR temporaries, while others can be assigned without one */

struct {
  FILE *out;
} context;

/* Calculates the size of a type in bytes */
static int64_t
calculateSize(Type *type) {
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
      printf("Internal compiler error: Cannot calculate size of integer "
             "literal,\n");
      exit(1);
    case TYP_RECORD:
      {
        int64_t size = 0;
        for (size_t i = 0; i < type->record.vec->numItems; i++) {
          HashEntry *entry = *((HashEntry **)indexVector(type->record.vec, i));
          size += calculateSize((Type *)entry->data);
        }
        return size;
      }
    case TYP_BINDING:
      return calculateSize(type->typeEntry->data);
  }
  printf("Internal compiler error: Reached end of calculateSize without "
         "returning.\n");
  exit(1);
}

/* Generates either 'w' or 'l' for a given type, which are the only types that
 * QBE allows temporaries to have */
static const char *
generateType(Type *type) {
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
    case TYP_RECORD:
      printf("These types dont work yet.\n");
      exit(1);
  }
  assert(false);
  return NULL;
}

/* Generates a random new number */
static int
getNewNum() {
  static int cnt = 0;
  return cnt++;
}

#define SMALL_TYPES                                                            \
  TYP_S8:                                                                      \
  case TYP_U8:                                                                 \
  case TYP_S16:                                                                \
  case TYP_U16:                                                                \
  case TYP_S32:                                                                \
  case TYP_U32:                                                                \
  case TYP_BOOL:                                                               \
  case TYP_INTLIT:                                                             \
  case TYP_CHAR:                                                               \
  case TYP_VOID

#define BIG_TYPES                                                              \
  TYP_S64:                                                                     \
  case TYP_U64:                                                                \
  case TYP_FUN:                                                                \
  case TYP_RECORD:                                                             \
  case TYP_PTR

#define SIGNED_SMALL_TYPES                                                     \
  TYP_S8:                                                                      \
  case TYP_S16:                                                                \
  case TYP_S32:                                                                \
  case TYP_INTLIT

#define UNSIGNED_SMALL_TYPES                                                   \
  TYP_U8:                                                                      \
  case TYP_U16:                                                                \
  case TYP_U32:                                                                \
  case TYP_BOOL:                                                               \
  case TYP_CHAR:                                                               \
  case TYP_VOID

#define SIGNED_BIG_TYPES TYP_S64

#define UNSIGNED_BIG_TYPES                                                     \
  TYP_U64:                                                                     \
  case TYP_FUN:                                                                \
  case TYP_RECORD:                                                             \
  case TYP_PTR

/* Picks the correct binary op for a type */
static char *
generateBinaryOp(int op, Type *type) {
  switch (op) {
    case BINOP_ADD:
      return "add";
    case BINOP_SUB:
      return "sub";
    case BINOP_MUL:
      return "mul";
    case BINOP_DIV:
      switch (type->type) {
        case TYP_S8:
        case TYP_S16:
        case TYP_S32:
        case TYP_S64:
          return "div";
        case TYP_U8:
        case TYP_U16:
        case TYP_U32:
        case TYP_U64:
          return "udiv";
        default:
          assert(false);
          return NULL;
      }
    case BINOP_EQUAL:
      switch (type->type) {
        case SMALL_TYPES:
          return "ceqw";
        case BIG_TYPES:
          return "ceql";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    case BINOP_NOTEQUAL:
      switch (type->type) {
        case SMALL_TYPES:
          return "cnew";
        case BIG_TYPES:
          return "cnel";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    case BINOP_LESS:
      switch (type->type) {
        case SIGNED_SMALL_TYPES:
          return "csltw";
        case UNSIGNED_SMALL_TYPES:
          return "csltw";
        case SIGNED_BIG_TYPES:
          return "csltl";
        case UNSIGNED_BIG_TYPES:
          return "csltl";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    case BINOP_GREAT:
      switch (type->type) {
        case SIGNED_SMALL_TYPES:
          return "csgtw";
        case UNSIGNED_SMALL_TYPES:
          return "csgtw";
        case SIGNED_BIG_TYPES:
          return "csgtl";
        case UNSIGNED_BIG_TYPES:
          return "csgtl";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    case BINOP_LESS_EQ:
      switch (type->type) {
        case SIGNED_SMALL_TYPES:
          return "cslew";
        case UNSIGNED_SMALL_TYPES:
          return "cslew";
        case SIGNED_BIG_TYPES:
          return "cslel";
        case UNSIGNED_BIG_TYPES:
          return "cslel";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    case BINOP_GREAT_EQ:
      switch (type->type) {
        case SIGNED_SMALL_TYPES:
          return "csgew";
        case UNSIGNED_SMALL_TYPES:
          return "csgew";
        case SIGNED_BIG_TYPES:
          return "csgel";
        case UNSIGNED_BIG_TYPES:
          return "csgel";
        case TYP_BINDING:
          return generateBinaryOp(op, type->typeEntry->data);
      }
      break;
    default:
      assert(false);
      exit(1);
  }
  return NULL;
}

/* Returns true if the given expression needs its own instruction in the IR,
 * or false if the given instruction can be placed inline. The needsCopy
 * parameter should be used to determine if a copy instruction is needed */
static bool
needsOwnInstruction(Expr *exp) {
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

/* TODO: Move this to a different file for semantic analysis, as this should
 * not be in the codegeneration phase */
static int
translateCharacter(Symbol sym) {
  if (sym.len >= 1) {
    if (sym.text[0] == '\\') {

      if (sym.text[1] == 'n') {

        return 10;
      }
    } else {
      return (int)sym.text[0];
    }
  }
  printf("Symbol too short.\n");
  exit(1);
}

// Forward declaration for mutual recursion
static char *generateExpr(Scope *scope, Expr *expr, bool *needsCopy);

/* A boolean 'and' will not evaluate the right hand side if the first value
 * evaluates to false */
static char *
generateAnd(Scope *scope, Expr *expr, bool *needsCopy) {

  int loc1 = getNewNum();

  char *tempExpr1 = generateExpr(scope, expr->binop.exp1, needsCopy);
  if (*needsCopy) {
    fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
            generateType(expr->typeExpr), tempExpr1);
  } else {
    fprintf(context.out, "\t%%v%d =%s %s\n", loc1, generateType(expr->typeExpr),
            tempExpr1);
  }

  int jmp1 = getNewNum();
  int jmp2 = getNewNum();
  int jmp3 = getNewNum();
  int jmp4 = getNewNum();

  fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc1, jmp1, jmp3);
  fprintf(context.out, "@j%d\n", jmp1);

  int loc2 = getNewNum();

  char *tempExpr2 = generateExpr(scope, expr->binop.exp2, needsCopy);

  if (*needsCopy) {
    fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
            generateType(expr->typeExpr), tempExpr2);
  } else {
    fprintf(context.out, "\t%%v%d =%s %s\n", loc2, generateType(expr->typeExpr),
            tempExpr2);
  }

  fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc2, jmp2, jmp3);

  fprintf(context.out, "@j%d\n", jmp2);

  int loc3 = getNewNum();

  fprintf(context.out, "\t%%v%d =l copy 1\n", loc3);
  fprintf(context.out, "\tjmp @j%d\n", jmp4);

  fprintf(context.out, "@j%d\n", jmp3);
  fprintf(context.out, "\t%%v%d =l copy 0\n", loc3);
  fprintf(context.out, "@j%d\n", jmp4);

  *needsCopy = true;

  return msprintf("%%v%d", loc3);
}

/* A boolean 'or' will not evaluate the right hand side if the first value
 * evaluates to true
 */
char *
generateOr(Scope *scope, Expr *expr, bool *needsCopy) {

  int loc1 = getNewNum();

  char *tempExpr1 = generateExpr(scope, expr->binop.exp1, needsCopy);
  if (*needsCopy) {
    fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
            generateType(expr->typeExpr), tempExpr1);
  } else {
    fprintf(context.out, "\t%%v%d =%s %s\n", loc1, generateType(expr->typeExpr),
            tempExpr1);
  }

  int jmp1 = getNewNum();
  int jmp2 = getNewNum();
  int jmp3 = getNewNum();
  int jmp4 = getNewNum();

  fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc1, jmp2, jmp1);
  fprintf(context.out, "@j%d\n", jmp1);

  int loc2 = getNewNum();

  char *tempExpr2 = generateExpr(scope, expr->binop.exp2, needsCopy);

  if (*needsCopy) {
    fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
            generateType(expr->typeExpr), tempExpr2);
  } else {
    fprintf(context.out, "\t%%v%d =%s %s\n", loc2, generateType(expr->typeExpr),
            tempExpr2);
  }

  fprintf(context.out, "\tjnz %%v%d, @j%d, @j%d\n", loc2, jmp2, jmp3);

  fprintf(context.out, "@j%d\n", jmp2);

  int loc3 = getNewNum();

  fprintf(context.out, "\t%%v%d =l copy 1\n", loc3);
  fprintf(context.out, "\tjmp @j%d\n", jmp4);

  fprintf(context.out, "@j%d\n", jmp3);
  fprintf(context.out, "\t%%v%d =l copy 0\n", loc3);
  fprintf(context.out, "@j%d\n", jmp4);

  *needsCopy = true;

  return msprintf("%%v%d", loc3);
}

/* Picks the correct load instruction for a given type. This is needed because
 * if a item smaller than the size of the temporary is loaded, a bitshift is
 * needed, and this varies depending on if the type is signed */
char *
pickLoadInst(Type *type) {
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

char *
pickStoreInst(Type *type) {
  switch (type->type) {
    case TYP_S8:
    case TYP_U8:
      return "storeb";
    case TYP_S16:
    case TYP_U16:
      return "storeh";
    case TYP_S32:
    case TYP_U32:
    case TYP_CHAR:
    case TYP_BOOL:
      return "storew";
    case TYP_S64:
    case TYP_U64:
    case TYP_PTR:
    case TYP_FUN:
      return "storel";
    case TYP_BINDING:
      return pickStoreInst(type->typeEntry->data);
      printf("These types dont work yet.\n");
      exit(1);
    case TYP_VOID:
    case TYP_INTLIT:
    case TYP_RECORD:

      assert(false);
      return NULL;
  }
  assert(false);
  return NULL;
}

static char *
generateVarExpr(Expr *expr, bool *needsCopy) {
  TypedEntry *entry = expr->var->data;

  if (entry->onStack) {

    int loc = getNewNum();
    /* Fetch the location from the stack */
    fprintf(context.out, "\t%%v%d =%s %s %%%.*s\n", loc,
            generateType(entry->type), pickLoadInst(expr->typeExpr),
            (int)expr->var->id.len, expr->var->id.text);
    *needsCopy = true;
    return msprintf("%%v%d", loc);
  }

  *needsCopy = true;
  if (expr->typeExpr->type == TYP_FUN) {
    return msprintf("$%.*s", (int)expr->var->id.len, expr->var->id.text);
  } else {
    return msprintf("%%%.*s", (int)expr->var->id.len, expr->var->id.text);
  }
}

static char *
generateBinOpExpr(Scope *scope, Expr *expr, bool *needsCopy) {

  if (expr->binop.op == BINOP_AND) {
    return generateAnd(scope, expr, needsCopy);
  }
  if (expr->binop.op == BINOP_OR) {
    return generateOr(scope, expr, needsCopy);
  }

  int loc1 = getNewNum();
  int loc2 = getNewNum();

  char *tempexpr1 = generateExpr(scope, expr->binop.exp1, needsCopy);

  char *expr1;
  if (needsOwnInstruction(expr->binop.exp1)) {
    if (*needsCopy) {
      fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1,
              generateType(expr->typeExpr), tempexpr1);

    } else {
      fprintf(context.out, "\t%%v%d =%s %s\n", loc1,
              generateType(expr->typeExpr), tempexpr1);
    }
    expr1 = msprintf("%%v%d", loc1);
  } else {
    expr1 = tempexpr1;
  }

  char *tempExpr2 = generateExpr(scope, expr->binop.exp2, needsCopy);

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
  return msprintf("%s %s, %s", generateBinaryOp(expr->binop.op, expr->typeExpr),
                  expr1, expr2);
}

static char *
generateFuncallExpr(Scope *scope, Expr *expr, bool *needsCopy) {
  Vector *exprVec =
      newVector(sizeof(char *), expr->funcall.arguments->numItems);
  if (expr->funcall.arguments->numItems > 0) {
    for (size_t i = 0; i < expr->funcall.arguments->numItems; i++) {
      Expr *exp = *((Expr **)indexVector(expr->funcall.arguments, i));
      char *tempExpr;
      if (needsOwnInstruction(exp)) {
        int tempLoc = getNewNum();
        char *tempTempExpr = generateExpr(scope, exp, needsCopy);
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
    Expr *exp = *((Expr **)indexVector(expr->funcall.arguments,
                                       expr->funcall.arguments->numItems - 1));
    char *tempExpr =
        *((char **)indexVector(exprVec, expr->funcall.arguments->numItems - 1));
    fprintf(context.out, "%s %s", generateType(exp->typeExpr), tempExpr);
  }
  fprintf(context.out, ")\n");
  *needsCopy = true;
  return msprintf("%%v%d", location);
}

static char *
generateExpr(Scope *scope, Expr *expr, bool *needsCopy) {
  switch (expr->type) {
    case EXP_INT:
      *needsCopy = true;
      return msprintf("%.*s", (int)expr->intlit.len, expr->intlit.text);
    case EXP_VAR:
      return generateVarExpr(expr, needsCopy);
    case EXP_BOOL:
      *needsCopy = true;
      return msprintf("%d", expr->boolean ? 1 : 0);
    case EXP_CHAR:
      *needsCopy = true;
      return msprintf("%d", translateCharacter(expr->character));
    case EXP_ADDROF:
      {
        if (expr->addr.expr->type == EXP_VAR) {
          *needsCopy = true;
          return msprintf("%%%.*s", (int)expr->addr.expr->var->id.len,
                          expr->addr.expr->var->id.text);
        }
        assert(false); // This will be signalled as an erro in gen_stack.c
        return NULL;
      }
    case EXP_DEREF:
      {
        char *exp = generateExpr(scope, expr->deref, needsCopy);
        int loc1 = getNewNum();
        if (*needsCopy) {
          fprintf(context.out, "\t%%v%d =l copy %s\n", loc1, exp);
        } else {
          fprintf(context.out, "\t%%v%d =l %s\n", loc1, exp);
        }
        int loc2 = getNewNum();

        fprintf(context.out, "\t%%v%d =%s %s %%v%d\n", loc2,
                generateType(expr->deref->typeExpr),
                pickLoadInst(expr->deref->typeExpr), loc1);
        *needsCopy = true;
        return msprintf("%%v%d", loc2);
      }
    case EXP_BINOP:
      return generateBinOpExpr(scope, expr, needsCopy);
    case EXP_FUNCALL:
      return generateFuncallExpr(scope, expr, needsCopy);
    case EXP_RECORDLIT:
      printf("No funcalls or record lits yet.\n");
      exit(1);
  }
  return 0;
}

static void
generateCompoundAssign(Scope *scope, Stmt* stmt, bool *needsCopy) {
  if (stmt->compound_assign.lval->type == LVAL_VAR) {

    TypedEntry *entry = stmt->compound_assign.lval->var.entry->data;
    char *expr = generateExpr(scope, stmt->compound_assign.value, needsCopy);
    if (entry->onStack) {
      int loc1 = getNewNum(); // Stores the old value from the stack
      int loc2 = getNewNum(); // Stores the expression 
      int loc3 = getNewNum(); // Stores the new value, that will be stored back into the stack

      // Fetch the value from the stack
      fprintf(context.out, "\t%%v%d =%s %s %%%.*s\n", 
          loc1, 
          generateType(entry->type), pickLoadInst(entry->type), 
          (int) stmt->compound_assign.lval->var.sym.len, 
          stmt->compound_assign.lval->var.sym.text);

      // Calculate the right hand side of the statement
      if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2, 
            generateType(stmt->compound_assign.value->typeExpr), expr);
      }
      else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc2, 
            generateType(stmt->compound_assign.value->typeExpr), expr);
      }

      fprintf(context.out, "\t%%v%d =%s %s %%v%d, %%v%d\n", loc3, 
          generateType(entry->type), 
          generateBinaryOp(stmt->compound_assign.op, entry->type), loc1, loc2);

      fprintf(context.out, "\t%s %%v%d, %%%.*s\n", 
          pickStoreInst(entry->type), loc3, 
          (int) stmt->compound_assign.lval->var.sym.len, 
          stmt->compound_assign.lval->var.sym.text);
    }
    else {
      int loc1 = getNewNum(); // This stores the right hand expression 

      // Calculate the right hand side of the statement
      if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc1, generateType(stmt->compound_assign.value->typeExpr), expr);
      }
      else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc1, generateType(stmt->compound_assign.value->typeExpr), expr);
      }

      fprintf(context.out, "\t%%%.*s =%s %s %%%.*s, %%v%d\n", (int) stmt->compound_assign.lval->var.sym.len, 
          stmt->compound_assign.lval->var.sym.text, generateType(entry->type),
          generateBinaryOp(stmt->compound_assign.op, entry->type),
          (int) stmt->compound_assign.lval->var.sym.len, 
          stmt->compound_assign.lval->var.sym.text,
          loc1);
    }
  }
  else {
    assert(false);
    exit(1);
  }
}
static void
generateAssign(Scope *scope, Stmt *stmt, bool *needsCopy) {
  if (stmt->assign.lval->type == LVAL_VAR) {
    TypedEntry *entry = stmt->assign.lval->var.entry->data;
    char *expr = generateExpr(scope, stmt->assign.value, needsCopy);
    int loc = getNewNum();
    if (entry->onStack) {
      if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc,
                generateType(stmt->assign.value->typeExpr), expr);
      } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc,
                generateType(stmt->assign.value->typeExpr), expr);
      }
      fprintf(context.out, "\t%s %%v%d, %%%.*s\n",
              pickStoreInst(stmt->assign.value->typeExpr), loc,
              (int)stmt->assign.lval->var.entry->id.len,
              stmt->assign.lval->var.entry->id.text);
    } else {
      if (*needsCopy) {
        fprintf(context.out, "\t%%%.*s =%s copy %s\n",
                (int)stmt->assign.lval->var.entry->id.len,
                stmt->assign.lval->var.entry->id.text,
                generateType(stmt->assign.value->typeExpr), expr);
      } else {
        fprintf(context.out, "\t%%%.*s =%s %s\n",
                (int)stmt->assign.lval->var.entry->id.len,
                stmt->assign.lval->var.entry->id.text,
                generateType(stmt->assign.value->typeExpr), expr);
      }
    }
  }
  if (stmt->assign.lval->type == LVAL_DEREF) {
    TypedEntry *entry = stmt->assign.lval->deref.entry->data;
    char *expr = generateExpr(scope, stmt->assign.value, needsCopy);
    if (entry->onStack) {
      int loc = getNewNum(); // Stores the actual address of the variable, not
                             // the address of the address on the stack
      fprintf(context.out, "\t%%v%d =l loadl %%%.*s\n", loc,
              (int)stmt->assign.lval->deref.sym.len,
              stmt->assign.lval->deref.sym.text);

      int loc2 = getNewNum();
      if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc2,
                generateType(stmt->assign.value->typeExpr), expr);
      } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc2,
                generateType(stmt->assign.value->typeExpr), expr);
      }
      fprintf(context.out, "\t%s %%v%d, %%v%d\n",
              pickStoreInst(stmt->assign.value->typeExpr), loc2, loc);
    } else {

      int loc = getNewNum();
      if (*needsCopy) {
        fprintf(context.out, "\t%%v%d =%s copy %s\n", loc,
                generateType(stmt->assign.value->typeExpr), expr);
      } else {
        fprintf(context.out, "\t%%v%d =%s %s\n", loc,
                generateType(stmt->assign.value->typeExpr), expr);
      }

      fprintf(context.out, "\t%s %%v%d, %%%.*s\n",
              pickStoreInst(stmt->assign.value->typeExpr), loc,
              (int)stmt->assign.lval->deref.sym.len,
              stmt->assign.lval->deref.sym.text);
    }
  }
}

static void
generateDecAssign(Scope *scope, Stmt *stmt, bool *needsCopy) {
  TypedEntry *entry = stmt->dec_assign.var->data;
  char *expr = generateExpr(scope, stmt->dec_assign.value, needsCopy);

  if (entry->onStack) {
    fprintf(context.out, "\t%%%.*s =l alloc4 %ld\n",
            (int)stmt->dec_assign.var->id.len, stmt->dec_assign.var->id.text,
            calculateSize(stmt->dec_assign.type));
    int loc = getNewNum();

    if (*needsCopy) {
      fprintf(context.out, "\t%%v%d =%s copy %s\n", loc,
              generateType(stmt->dec_assign.type), expr);
    } else {
      fprintf(context.out, "\t%%v%d =%s %s\n", loc,
              generateType(stmt->dec_assign.type), expr);
    }

    fprintf(context.out, "\t%s %%v%d, %%%.*s\n",
            pickStoreInst(stmt->dec_assign.type), loc,
            (int)stmt->dec_assign.var->id.len, stmt->dec_assign.var->id.text);

  }

  else {
    if (*needsCopy) {
      fprintf(context.out, "\t%%%.*s =%s copy %s\n",
              (int)stmt->dec_assign.var->id.len, stmt->dec_assign.var->id.text,
              generateType(stmt->dec_assign.value->typeExpr), expr);
    } else {
      fprintf(context.out, "\t%%%.*s =%s %s\n",
              (int)stmt->dec_assign.var->id.len, stmt->dec_assign.var->id.text,
              generateType(stmt->dec_assign.value->typeExpr), expr);
    }
  }
}

static void
generateStatement(Scope *scope, Stmt *stmt) {

  bool needsCopy;
  switch (stmt->type) {
    case STMT_DEC:
      {
        TypedEntry *entry = stmt->dec.var->data;
        if (entry->onStack) {
          fprintf(context.out, "\t%%%.*s =l alloc4 %ld\n",
                  (int)stmt->dec.var->id.len, stmt->dec.var->id.text,
                  calculateSize(stmt->dec.type));
          printType(stmt->dec.type);
        }
        break;
      }

    case STMT_EXPR:
      generateExpr(scope, stmt->singleExpr, &needsCopy);
      break;
    case STMT_ASSIGN:
      generateAssign(scope, stmt, &needsCopy);
      return;
    case STMT_COMPOUND_ASSIGN:
      generateCompoundAssign(scope, stmt, &needsCopy);
      return;
    case STMT_RETURN:
      {
        if (stmt->returnExp == NULL) {

          fprintf(context.out, "\tret\n");
        } else {
          char *expr = generateExpr(scope, stmt->returnExp, &needsCopy);
          fprintf(context.out, "\tret %s\n", expr);
        }
        break;
      }
    case STMT_IF:
      {
        char *expr = generateExpr(scope, stmt->if_block.cond, &needsCopy);
        int value1 = getNewNum();
        if (needsCopy) {

          fprintf(context.out, "\t%%v%d =%s copy %s\n", value1,
                  generateType(stmt->if_block.cond->typeExpr), expr);
        } else {
          fprintf(context.out, "\t%%v%d =%s %s\n", value1,
                  generateType(stmt->if_block.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        fprintf(context.out, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1,
                loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_block.block, i));
          generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "@loc%d\n", loc2);
        break;
      }
    case STMT_WHILE:
      {
        int loc1, loc2, loc3;

        loc1 = getNewNum();
        loc2 = getNewNum();
        loc3 = getNewNum();
        fprintf(context.out, "@loc%d\n", loc1);

        char *expr = generateExpr(scope, stmt->while_block.cond, &needsCopy);
        int value1 = getNewNum();



        if (needsCopy) {
          fprintf(context.out, "\t%%v%d =%s copy %s\n", value1,
                  generateType(stmt->while_block.cond->typeExpr), expr);
        } else {
          fprintf(context.out, "\t%%v%d =%s %s\n", value1,
                  generateType(stmt->while_block.cond->typeExpr), expr);
        }

        fprintf(context.out, "\tjnz %%v%d, @loc%d, @loc%d\n", value1, loc2,
                loc3);

        fprintf(context.out, "@loc%d\n", loc2);

        for (size_t i = 0; i < stmt->while_block.block->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->while_block.block, i));
          generateStatement(scope, tempStmt);
        }

        fprintf(context.out, "\tjmp @loc%d\n", loc1);

        fprintf(context.out, "@loc%d\n", loc3);
        break;
      }
    case STMT_IF_ELSE:
      {
        char *expr = generateExpr(scope, stmt->if_else.cond, &needsCopy);
        int value1 = getNewNum();
        if (needsCopy) {
          fprintf(context.out, "\t%%v%d =%s copy %s\n", value1,
                  generateType(stmt->if_else.cond->typeExpr), expr);
        } else {
          fprintf(context.out, "\t%%v%d =%s %s\n", value1,
                  generateType(stmt->if_else.cond->typeExpr), expr);
        }
        int loc1 = getNewNum();
        int loc2 = getNewNum();
        int loc3 = getNewNum();
        fprintf(context.out, "\tjnz %%v%d, @loc%d, @loc%d\n@loc%d\n", value1,
                loc1, loc2, loc1);

        for (size_t i = 0; i < stmt->if_else.block1->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block1, i));
          generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "\tjmp @loc%d\n", loc3);
        fprintf(context.out, "@loc%d\n", loc2);
        for (size_t i = 0; i < stmt->if_else.block2->numItems; i++) {
          Stmt *tempStmt = *((Stmt **)indexVector(stmt->if_else.block2, i));
          generateStatement(scope, tempStmt);
        }
        fprintf(context.out, "@loc%d\n", loc3);
        break;
      }
    case STMT_DEC_ASSIGN:
      generateDecAssign(scope, stmt, &needsCopy);
      return;
    default:
      assert(false);
      exit(1);
  }
}

static void
generateFunction(Function *fn) {
  fprintf(context.out, "export function %s $%.*s(", generateType(fn->retType),
          (int)fn->name.len, fn->name.text);

  if (fn->params->numItems != 0) {
    for (size_t i = 0; i < fn->params->numItems - 1; i++) {
      Param param = *((Param *)indexVector(fn->params, i));
      fprintf(context.out, "%s %%%.*s, ", generateType(param.type),
              (int)param.var->id.len, param.var->id.text);
    }

    Param param = *((Param *)indexVector(fn->params, fn->params->numItems - 1));
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

static void
generateToplevel(Toplevel top) {

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

void
generateCode(AST *ast, FILE *file) {
  allocateToStack(ast);
  context.out = file;

  for (size_t i = 0; i < ast->decs->numItems; i++) {

    Toplevel top = *((Toplevel *)indexVector(ast->decs, i));
    generateToplevel(top);
  }
}
