//===------------ resolve_names.h - Resolve scopes and names -------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides functions prototypes for resolve_names.c
//
//===------------------------------ Todo ---------------------------------===//
//
//===---------------------------------------------------------------------===//

#pragma once

#include <ast.h>

void resolveNames(AST *oldAst);
