//===------------------ types.h - Types header file --------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides type definitions for the AST.
//
//===---------------------------------------------------------------------===//

#pragma once 

#include <ast.h>

/* Traverses the AST and gives types to all values */
void annotateAST(AST* ast);
