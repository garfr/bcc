#pragma once

#include <stdio.h>

#include "bcc/ast.h"

void calculateStackPositions(AST *ast);

void generateCode(AST *ast, FILE *file);

