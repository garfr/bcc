//===-------------------- lexer.h - Lexer header file --------------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Provides types and function prototypes for lexer.h
//
//===---------------------------------------------------------------------===//

#pragma once

#include <stddef.h>
#include <utils.h>

enum TokenType {
    /* Keywords */
    TOK_LET,
    TOK_PROC,
    TOK_MUT,

    /* Symbols, and other values */
    TOK_SYM,
    TOK_INT,

    /* Punctuation */
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_EQUAL,
    TOK_PLUS,
    TOK_MINUS,
    TOK_STAR,
    TOK_SLASH,

    /* Other */
    TOK_EOF
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
    };

} Token;

enum LexerState { LEX_START, LEX_SYMBOL, LEX_INT };

typedef struct {
    const unsigned char *buffer;
    size_t bufferLen;
    size_t startIdx;
    size_t endIdx;
    enum LexerState state;
} Lexer;

Lexer newLexer(const unsigned char *buffer, size_t bufferLen);
Token nextToken(Lexer *lex);
Token peekToken(Lexer *lex);
