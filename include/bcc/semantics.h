#pragma once
#include "bcc/ast.h"

/* Traverses the AST and gives types to all values */
void resolveNames(AST *ast);
void annotateAST(AST *ast);
void checkReturns(AST *ast);
