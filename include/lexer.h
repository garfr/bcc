#pragma once

#include <stddef.h>

#include <utils.h>

/* Public Interface */

enum TokenType {
    /* Keywords */
    TOK_VAR,
    TOK_PROC,
    TOK_MUT,

    /* Symbols, and other values */
    TOK_SYM,
    TOK_INT,

    /* Punctuation */
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_EQUAL,

    /* Other */
    TOK_EOF
};

typedef struct {
    size_t start;
    size_t end;
    enum TokenType type;

    union {
        Symbol sym;
        Symbol intnum;
    };

} Token;

void printToken(Token tok);

typedef struct Lexer Lexer;

Lexer *newLexer(const unsigned char *buffer, size_t len);
Token nextToken(Lexer *lex);
Token peekToken(Lexer *lex);
