//===----------- error.h - Error formatting and printing header -----------===/
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ----------------------------------===/
//
// Defines the types and provides prototypes for functions in error.c
//
//===----------------------------------------------------------------------===/

#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef struct Error {
    char *message;
    size_t start;
    size_t end;
} Error;

void queueError(char *message, size_t start, size_t end);
void printErrors();
bool errorsExist();
void initErrors(const unsigned char *newBuffer, size_t bufferSize,
                const char *newBufferName);
