//===------------- lexer.c - Tokenizes a source file lazily --------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Breaks a file into tokens lazily.
//
//===------------------------------ Todo ---------------------------------===//
//
// * Support more complex integer syntaxes (eg. negative signs, hexadecimal,
//   binary, underscores)
// * Support floats
// * Possibly impose naming conventions on tokens
//
//===---------------------------------------------------------------------===//

#include <ctype.h>
#include <error.h>
#include <lexer.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Lexer newLexer(const unsigned char *buffer, size_t bufferLen) {
    Lexer ret;
    ret.buffer = buffer;
    ret.bufferLen = bufferLen;
    ret.startIdx = ret.endIdx = 0;
    ret.state = LEX_START;
    return ret;
}

/* Generates a token in the location the lexer is in currently, moves the lexer
 * forward, and resets the lexer to begin tokenizing then next token */
static Token makeTokenInplace(Lexer *lex, enum TokenType type) {
    Token tok =
        (Token){.start = lex->startIdx, .end = lex->endIdx, .type = type, {}};
    lex->endIdx++;
    lex->startIdx = lex->endIdx;
    lex->state = LEX_START;
    return tok;
}

static Token makeTokenBehind(Lexer *lex, enum TokenType type) {
    Token tok = (Token){
        .start = lex->startIdx, .end = lex->endIdx - 1, .type = type, {}};
    lex->endIdx++;
    lex->startIdx = lex->endIdx;
    lex->state = LEX_START;
    return tok;
}
/* Generates a symbol token one character behind where the lexer is currently,
 * and resets the lexer to begin tokenizing then next token */
static Token makeSymbolBehind(Lexer *lex) {
    Symbol sym = (Symbol){.text = &lex->buffer[lex->startIdx],
                          .len = lex->endIdx - lex->startIdx};

    Token tok;
    tok.start = lex->startIdx;
    tok.end = lex->endIdx - 1;
    /* Check if it matches any keywords, this can be replaced with a table in
     * the future */
    if (compareSymbolStr(sym, "let")) {
        tok.type = TOK_LET;
    } else if (compareSymbolStr(sym, "proc")) {
        tok.type = TOK_PROC;
    } else if (compareSymbolStr(sym, "mut")) {
        tok.type = TOK_MUT;
    } else if (compareSymbolStr(sym, "end")) {
        tok.type = TOK_END;
    } else if (compareSymbolStr(sym, "void")) {
        tok.type = TOK_VOID;
    } else if (compareSymbolStr(sym, "return")) {
        tok.type = TOK_RETURN;
    } else if (compareSymbolStr(sym, "type")) {
        tok.type = TOK_TYPE;
    } else if (compareSymbolStr(sym, "record")) {
        tok.type = TOK_RECORD;
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

/* Works the same as makeSymbolBehind but with integers */
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

bool newlineNeeded(Lexer *lex) {
    switch (lex->previousTok.type) {
        case TOK_INT:
        case TOK_SYM:
        case TOK_RPAREN:
        case TOK_RBRACKET:
            return true;
        default:
            return false;
    }
}
/* Big and messy but works well and is actually reasonably readable */
Token getToken(Lexer *lex) {
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
                if (c == '\n') {
                    if (newlineNeeded(lex)) {
                        return makeTokenInplace(lex, TOK_NEWLINE);
                    }
                    lex->endIdx++;
                    lex->startIdx = lex->endIdx;
                    continue;
                }
                if (isspace(c)) {
                    lex->endIdx++;
                    lex->startIdx = lex->endIdx;
                    continue;
                }
                switch (c) {
                    case '-':
                        lex->endIdx++;
                        lex->state = LEX_DASH;
                        continue;
                    case ':':
                        lex->endIdx++;
                        lex->state = LEX_COLON;
                        continue;
                    case ';':
                        return makeTokenInplace(lex, TOK_SEMICOLON);
                    case '=':
                        return makeTokenInplace(lex, TOK_EQUAL);
                    case '+':
                        return makeTokenInplace(lex, TOK_PLUS);
                    case '*':
                        return makeTokenInplace(lex, TOK_STAR);
                    case ')':
                        return makeTokenInplace(lex, TOK_RPAREN);
                    case '(':
                        return makeTokenInplace(lex, TOK_LPAREN);
                    case ',':
                        return makeTokenInplace(lex, TOK_COMMA);
                    case '.':
                        return makeTokenInplace(lex, TOK_PERIOD);
                    case '{':
                        return makeTokenInplace(lex, TOK_LBRACKET);
                    case '}':
                        return makeTokenInplace(lex, TOK_RBRACKET);
                    case '/':
                        return makeTokenInplace(lex, TOK_SLASH);
                    default: {
                        char *buffer =
                            msprintf("Unexpected character: '%c'", c);
                        queueError(buffer, lex->startIdx, lex->endIdx);
                        lex->endIdx++;
                        lex->startIdx = lex->endIdx;
                        continue;
                    }
                }
            }
            case LEX_DASH: {
                if (lex->endIdx >= lex->bufferLen) {
                    return makeTokenBehind(lex, TOK_MINUS);
                }
                unsigned char c = lex->buffer[lex->endIdx];
                if (c == '>') {
                    return makeTokenInplace(lex, TOK_ARROW);
                }
                return makeTokenBehind(lex, TOK_MINUS);
            }
            case LEX_COLON: {
                if (lex->endIdx >= lex->bufferLen) {
                    return makeTokenBehind(lex, TOK_COLON);
                }
                unsigned char c = lex->buffer[lex->endIdx];
                if (c == ':') {
                    return makeTokenInplace(lex, TOK_DOUBLECOLON);
                } else if (c == '=') {
                    return makeTokenInplace(lex, TOK_COLONEQUAL);
                }
                return makeTokenBehind(lex, TOK_COLON);
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

Token nextToken(Lexer *lex) { return lex->previousTok = getToken(lex); }

Token peekToken(Lexer *lex) {
    Lexer old = *lex;
    Token ret = getToken(lex);
    *lex = old;
    return ret;
}

Token lookaheadToken(Lexer *lex) {
    Lexer old = *lex;
    nextToken(lex);
    Token ret = getToken(lex);
    *lex = old;
    return ret;
}
