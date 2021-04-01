#pragma once

#include <stddef.h>

#include "bcc/utils.h"

enum TokenType {
  /* Keywords */
  TOK_LET,
  TOK_PROC,
  TOK_MUT,
  TOK_END,
  TOK_RETURN,
  TOK_VOID,
  TOK_RECORD,
  TOK_BOOL,
  TOK_TYPE,
  TOK_FALSE,
  TOK_TRUE,
  TOK_AND,
  TOK_OR,
  TOK_EXTERN,
  TOK_IF,
  TOK_ELSE,
  TOK_WHILE,
  TOK_DO,

  /* Symbols, and other values */
  TOK_SYM,
  TOK_INT,
  TOK_CHAR,
  TOK_CHARLIT,

  /* Punctuation */
  TOK_COLON,
  TOK_AMP,
  TOK_AT,
  TOK_DOUBLECOLON,
  TOK_SEMICOLON,
  TOK_COMMA,
  TOK_ARROW,
  TOK_COLONEQUAL,
  TOK_EQUAL,
  TOK_DOUBLEEQUAL,
  TOK_NOTEQUAL,
  TOK_PERIOD,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_LANGLE,
  TOK_RANGLE,
  TOK_LANGLE_EQ,
  TOK_RANGLE_EQ,
  TOK_LCURLY,
  TOK_RCURLY,
  TOK_LBRACKET,
  TOK_RBRACKET,
  TOK_UNDERLINE,

  TOK_PLUS,
  TOK_MINUS,
  TOK_STAR,
  TOK_SLASH,

  TOK_PLUS_EQ,
  TOK_MINUS_EQ,
  TOK_STAR_EQ,
  TOK_SLASH_EQ,

  /* Other */
  TOK_EOF,
  TOK_NEWLINE
};

typedef struct {
  size_t start;
  size_t end;
  enum TokenType type;

  union {
    Symbol sym;
    /* Integers are kept symbols until they can be proven to fit into a
     * certain integer type */
    Symbol intnum;
    Symbol character;
  };

} Token;

enum LexerState {
  LEX_START,
  LEX_SYMBOL,
  LEX_INT,
  LEX_DASH,
  LEX_COLON,
  LEX_SINGLE_QUOTE,
  LEX_EQUAL,
  LEX_EXCLAMATION,
  LEX_LANGLE,
  LEX_RANGLE,
  LEX_PLUS,
  LEX_STAR,
  LEX_SLASH,
};

typedef struct {
  const unsigned char *buffer;
  size_t bufferLen;
  size_t startIdx;
  size_t endIdx;
  enum LexerState state;
  Token previousTok;
} Lexer;

Lexer newLexer(const unsigned char *buffer, size_t bufferLen);
Token nextToken(Lexer *lex);
Token lookaheadToken(Lexer *lex);
Token peekToken(Lexer *lex);
