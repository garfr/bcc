#pragma once

#include "bcc/ast.h"

/* Traverses the AST and gives types to all values */
void annotateAST(AST *ast);
