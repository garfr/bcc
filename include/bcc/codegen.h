#pragma once

#include <stdio.h>

#include "bcc/ast.h"

void allocateToStack(AST *ast);

void generateCode(AST *ast, FILE *file);

