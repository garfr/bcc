#pragma once
#include "bcc/ast.h"

/* AST traversers */
void resolveNames(AST *ast);
void annotateAST(AST *ast);
void checkReturns(AST *ast);

Type *coerceBinop(int op, Type *type1, Type *type2);
Type *coerceAssignment(Type *type1, Type *type2);
