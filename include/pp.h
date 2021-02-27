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

#include <lexer.h>
#include <parser.h>
#include <tac.h>

/* lexer.h */
void printToken(Token tok);

/* parser.h */
void printType(Type *type);
void printExpr(Expr *exp);
void printStmt(Stmt *stmt);

/* tac.h */
void printAddr(TACAddr addr);
void printOp(TACOp op);
void printInst(TACInst *inst);
void printTAC(TAC *tac);
