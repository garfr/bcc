//===------------------ parser.h - Parser header file --------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides functions for parsing a file into an untyped syntax tree.
//
//===---------------------------------------------------------------------===//

#pragma once

#include <lexer.h>
#include <stddef.h>
#include <stdint.h>
#include <utils.h>
#include <ast.h>

AST *parseSource(Lexer *lex);
