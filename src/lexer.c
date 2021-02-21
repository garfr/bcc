#include <ctype.h>
#include <error.h>
#include <lexer.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SYM_TABLE_INIT_SIZE 8
#define MIN(a, b) a > b ? b : a

enum LexerState { LEX_START, LEX_SYMBOL, LEX_INT };

struct Lexer {
    const unsigned char *buffer;
    size_t bufferLen;
    size_t startIdx;
    size_t endIdx;
    enum LexerState state;
};

Lexer *newLexer(const unsigned char *buffer, size_t bufferLen) {
    Lexer *ret = malloc(sizeof(Lexer));
    ret->buffer = buffer;
    ret->bufferLen = bufferLen;
    ret->startIdx = ret->endIdx = 0;
    ret->state = LEX_START;
    return ret;
}

void printToken(Token tok) {

    switch (tok.type) {

    case TOK_VAR:
        printf("TOK_VAR");
        break;
    case TOK_PROC:
        printf("TOK_PROC");
        break;
    case TOK_MUT:
        printf("TOK_MUT");
        break;
    case TOK_SYM:
        printf("TOK_SYM: '");
        printSymbol(tok.sym);
        printf("'");
        break;
    case TOK_INT:
        printf("TOK_INT: '");
        printSymbol(tok.intnum);
        printf("'");
        break;
    case TOK_COLON:
        printf("TOK_COLON");
        break;
    case TOK_SEMICOLON:
        printf("TOK_SEMICOLON");
        break;
    case TOK_EQUAL:
        printf("TOK_EQUAL");
        break;
    case TOK_EOF:
        printf("TOK_EOF");
        break;
    default:
        printf("Compiler internal error: Invalid token type to print %d.\n",
               tok.type);
        exit(1);
    }

    printf(" %zd-%zd\n", tok.start, tok.end);
}

static Token makeTokenInplace(Lexer *lex, enum TokenType type) {
    Token tok =
        (Token){.start = lex->startIdx, .end = lex->endIdx, .type = type, {}};
    lex->endIdx++;
    lex->startIdx = lex->endIdx;
    lex->state = LEX_START;
    return tok;
}

static int compareSymbolStr(Symbol sym, const char *str) {
    if (sym.len != strlen(str)) {
        return -1;
    }
    size_t length = MIN(sym.len, strlen(str));
    for (size_t i = 0; i < length && str[i] != '\0'; i++) {
        if (sym.text[i] != str[i]) {
            return -1;
        }
    }
    return 0;
}
static Token makeSymbolBehind(Lexer *lex) {
    Symbol sym = (Symbol){.text = &lex->buffer[lex->startIdx],
                          .len = lex->endIdx - lex->startIdx};

    Token tok;
    tok.start = lex->startIdx;
    tok.end = lex->endIdx - 1;
    if (compareSymbolStr(sym, "var") == 0) {
        tok.type = TOK_VAR;
    } else if (compareSymbolStr(sym, "proc") == 0) {
        tok.type = TOK_PROC;
    } else if (compareSymbolStr(sym, "mut") == 0) {
        tok.type = TOK_MUT;
    } else {
        tok.sym = sym;
        tok.type = TOK_SYM;
    }

    if (lex->startIdx == lex->endIdx) {
        tok.end = lex->endIdx;
    }
    lex->startIdx = lex->endIdx;
    lex->state = LEX_START;
    return tok;
}

static Token makeIntBehind(Lexer *lex) {
    Symbol sym = (Symbol){.text = &lex->buffer[lex->startIdx],
                          .len = lex->endIdx - lex->startIdx};

    Token tok = (Token){.start = lex->startIdx,
                        .end = lex->endIdx - 1,
                        .type = TOK_INT,
                        .intnum = sym};

    if (lex->startIdx == lex->endIdx) {
        tok.end = lex->endIdx;
    }
    lex->startIdx = lex->endIdx;
    lex->state = LEX_START;
    return tok;
}

Token nextToken(Lexer *lex) {
    for (;;) {
        switch (lex->state) {
        case LEX_START: {
            if (lex->endIdx >= lex->bufferLen) {
                return makeTokenInplace(lex, TOK_EOF);
            }
            unsigned char c = lex->buffer[lex->endIdx];
            if (isalpha(c)) {
                lex->state = LEX_SYMBOL;
                lex->endIdx++;
                continue;
            }
            if (isdigit(c)) {
                lex->state = LEX_INT;
                lex->endIdx++;
                continue;
            }
            if (isspace(c)) {
                lex->endIdx++;
                lex->startIdx = lex->endIdx;
                continue;
            }
            switch (c) {
            case ';':
                return makeTokenInplace(lex, TOK_SEMICOLON);
            case ':':
                return makeTokenInplace(lex, TOK_COLON);
            case '=':
                return makeTokenInplace(lex, TOK_EQUAL);
            default: {
                char *buffer = dynamicSprintf("Unexpected character: '%c'", c);
                queueError(buffer, lex->startIdx, lex->endIdx);
                lex->endIdx++;
                lex->startIdx = lex->endIdx;
                continue;
            }
            }
        }
        case LEX_SYMBOL: {
            if (lex->endIdx >= lex->bufferLen) {
                return makeSymbolBehind(lex);
            }
            unsigned char c = lex->buffer[lex->endIdx];
            if (isalpha(c) || isdigit(c) || c == '_') {
                lex->endIdx++;
                continue;
            } else {
                return makeSymbolBehind(lex);
            }
        }

        case LEX_INT: {
            if (lex->endIdx >= lex->bufferLen) {
                return makeIntBehind(lex);
            }
            unsigned char c = lex->buffer[lex->endIdx];
            if (isdigit(c)) {
                lex->endIdx++;
                continue;
            } else {
                return makeIntBehind(lex);
            }
        }
        }
    }
}

Token peekToken(Lexer *lex) {
    Lexer old = *lex;
    Token ret = nextToken(lex);
    *lex = old;
    return ret;
}
