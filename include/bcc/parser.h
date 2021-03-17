#pragma once

#include <stddef.h>
#include <stdint.h>

#include "bcc/ast.h"
#include "bcc/lexer.h"
#include "bcc/utils.h"

AST *parseSource(Lexer *lex);
