//===------------- codegen.h - Code generation header ---------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Header for codegen.h
//
//===---------------------------------------------------------------------===//
#pragma once

#include <stdio.h>
#include <tac.h>

void generateCode(TAC* tac, Hashtbl* symTable, FILE* output);
