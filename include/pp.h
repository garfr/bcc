//===----------- pp.h - Pretty printing header -------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// The header file for pp.c containing function prototypes.
//
//===---------------------------------------------------------------------===//

#include <ast.h>
#include <lexer.h>
#include <tac.h>
#include <unresolved_ast.h>

/* lexer.h */
void printToken(Token tok);

/* ast.h */
void printType(Type *type);
void printExpr(Expr *exp);
void printStmt(Stmt *stmt);
void printAST(AST *ast);

/* unresolved_ast.h */
void printURType(UR_Type *type);
void printURExpr(UR_Expr *exp);
void printURStmt(UR_Stmt *stmt);
void printURAST(UR_AST *ast);
/* tac.h */
void printAddr(TACAddr addr);
void printOp(TACOp op);
void printInst(TACInst *inst);
void printTAC(TAC *tac);
