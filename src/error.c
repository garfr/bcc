//===-------------- error.c - Error formatting and printing ---------------===/
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------------------------------------------------===/
//
// This file provides functions to add to a global error list, which will then
// be printed when the compiler can no longer continue
//
//===----------------------------------------------------------------------===/
//
// TODO: Better multiline errors,
// Give a specific variant in a variant enum to an error,
// rather than just having the code raising the error dynamically allocate a
// string every time.
//
//===----------------------------------------------------------------------===/

#include <error.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>

#define BOLDRED "\033[1m\033[31m"
#define BOLDWHITE "\033[1m\033[37m"
#define RESET "\033[0m"
#define RED "\033[31m"

/*
 * Queuing an error simply appends it to a vector of arrays
 * Handling the error messages is simply iterating over the list and
 * printing each one
 */

struct {
    Vector *errors;
    const char *bufferName;
    const unsigned char *buffer;
    size_t bufferSize;
} errorCtx;

void initErrors(const unsigned char *newBuffer, size_t newBufferSize,
                const char *newBufferName) {
    errorCtx.buffer = newBuffer;
    errorCtx.bufferSize = newBufferSize;
    errorCtx.bufferName = newBufferName;
    errorCtx.errors = newVector(sizeof(Error), 0);
}

void queueError(char *message, size_t start, size_t end) {
    Error err;
    err.message = message;
    err.start = start;
    err.end = end;

    pushVector(errorCtx.errors, &err);
}

typedef struct {
    size_t line_idx;
    size_t line_idx_end;
    size_t line_start;
    size_t line_end;
    size_t column_start;
    size_t column_end;
} Location;

/*
 * This is bad and incomplete
 * It hosts who knows how many bugs
 * I will fix this.
 */
Location calculateLocation(size_t start, size_t end) {
    size_t line = 1;
    size_t column = 1;
    size_t i;

    for (i = 0; i < start; i++) {
        column++;
        if (errorCtx.buffer[i] == '\n') {
            line++;
            column = 1;
        }
    }

    Location ret;
    size_t line_start;
    for (line_start = start; line_start > 0; line_start--) {
        if (errorCtx.buffer[line_start] == '\n') {
            break;
        }
    }
    ret.line_start = line;
    ret.line_idx = line_start;
    size_t line_end;
    for (line_end = start; line_end < errorCtx.bufferSize; line_end++) {
        if (errorCtx.buffer[line_end] == '\n') {
            break;
        }
    }
    ret.line_end = line;
    ret.line_idx_end = line_end;
    ret.column_start = column;
    ret.column_end = column;
    for (size_t i = start; i < end; i++) {
        ret.column_end++;
        if (errorCtx.buffer[i] == '\n') {
            ret.line_end++;
            ret.column_end = 1;
        }
    }
    return ret;
}

/* This is also weird and bad */
void printError(Error *err) {
    Location loc = calculateLocation(err->start, err->end);

    printf(BOLDWHITE);
    printf("%s:%zd:%zd: ", errorCtx.bufferName, loc.line_start,
           loc.column_start);
    printf(BOLDRED);
    printf("error: ");
    printf(BOLDWHITE);
    printf("%s.\n", err->message);
    printf(RESET);
    printf("   %zd | ", loc.line_start);
    for (size_t i = loc.line_idx + 1; i < loc.line_idx_end; i++) {
        putchar(errorCtx.buffer[i]);
    }
    printf("\n");
    printf("     | ");
    for (size_t i = 0; i < loc.column_start - 1; i++) {
        printf(" ");
    }
    printf(BOLDRED);
    for (size_t i = loc.column_start; i < loc.column_end + 1; i++) {
        printf("~");
    }
    printf(RESET);
    printf("\n");
}

void printErrors() {
    for (size_t i = 0; i < errorCtx.errors->numItems; i++) {
        printError(indexVector(errorCtx.errors, i));
    }
    exit(1);
}

bool errorsExist() { return errorCtx.errors->numItems != 0; }

