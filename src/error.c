#include <error.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BOLDRED "\033[1m\033[31m"
#define BOLDWHITE "\033[1m\033[37m"
#define RESET "\033[0m"
#define RED "\033[31m"

/* Fuck concurrency its global variable time */

Error *errorQueue = NULL;
const char *bufferName;

const unsigned char *buffer;
size_t bufferSize;

void initErrors(const unsigned char *newBuffer, size_t newBufferSize,
                const char *newBufferName) {
    buffer = newBuffer;
    bufferSize = newBufferSize;
    bufferName = newBufferName;
}

void queueError(char *message, size_t start, size_t end) {
    Error *err = malloc(sizeof(Error));
    err->message = message;
    err->start = start;
    err->end = end;

    Error *oldErr = errorQueue;
    errorQueue = err;
    err->next = oldErr;
}

typedef struct {
    size_t line_idx;
    size_t line_idx_end;
    size_t line_start;
    size_t line_end;
    size_t column_start;
    size_t column_end;
} Location;

Location calculateLocation(size_t start, size_t end) {
    size_t line = 1;
    size_t column = 1;
    size_t i;

    for (i = 0; i < start; i++) {
        column++;
        if (buffer[i] == '\n') {
            line++;
            column = 1;
        }
    }

    Location ret;
    size_t line_start;
    for (line_start = start; line_start > 0; line_start--) {
        if (buffer[line_start] == '\n') {
            break;
        }
    }
    ret.line_start = line;
    ret.line_idx = line_start;
    size_t line_end;
    for (line_end = start; line_end < bufferSize; line_end++) {
        if (buffer[line_end] == '\n') {
            break;
        }
    }
    ret.line_end = line;
    ret.line_idx_end = line_end;
    ret.column_start = column;
    ret.column_end = column;
    for (size_t i = start; i < end; i++) {
        ret.column_end++;
        if (buffer[i] == '\n') {
            ret.line_end++;
            ret.column_end = 1;
        }
    }
    return ret;
}

/* TODO: Improve error printing. */
void printError(Error *err) {
    Location loc = calculateLocation(err->start, err->end);

    printf(BOLDWHITE);
    printf("%s:%zd:%zd: ", bufferName, loc.line_start, loc.column_start);
    printf(BOLDRED);
    printf("error: ");
    printf(BOLDWHITE);
    printf("%s.\n", err->message);
    printf(RESET);
    printf("   %zd | ", loc.line_start);
    for (size_t i = loc.line_idx + 1; i < loc.line_idx_end; i++) {
        putchar(buffer[i]);
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

void printErrorHelper(Error *err) {
    if (err->next != NULL) {
        printErrorHelper(err->next);
    }
    printError(err);
}

void printErrors() {
    printErrorHelper(errorQueue);
    exit(1);
}

char *dynamicSprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    va_list args2;
    va_copy(args2, args);

    size_t memNeeded = vsnprintf(NULL, 0, format, args) + 1;
    char *buffer = malloc(memNeeded * sizeof(char));
    if (buffer == NULL) {
        printf("Compiler internal error: Out of memory.\n");
        exit(1);
    }
    vsprintf(buffer, format, args2);
    return buffer;
}

bool errorsExist() { return errorQueue != NULL; }
