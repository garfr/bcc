//===------------------ types.c - Defines semantics and annotates the AST --------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// This file defines the semantics and rules of both builtin and user defined types, and
// provides the functionality for traversing and annotating the AST.
//
//===---------------------------------------------------------------------===//

#include <types.h>
#include <ast.h>
