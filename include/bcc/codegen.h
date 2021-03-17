#pragma once

#include <stdio.h>

#include "bcc/ast.h"

void generateCode(AST *ast, FILE *file);
