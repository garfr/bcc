#pragma once

#include "bcc/lexer.h"
#include "bcc/parser.h"

/* lexer.h */
void printToken(Token tok);

/* parser.h */
void printType(Type *type);
void printExpr(Expr *exp);
void printStmt(Stmt *stmt);
void printLVal(LVal *lval);
void printAST(AST *ast);
