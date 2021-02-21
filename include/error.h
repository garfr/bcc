#pragma once

#include <stddef.h>

typedef struct Error {
    char *message;
    size_t start;
    size_t end;
    struct Error *next;
} Error;

void queueError(char *message, size_t start, size_t end);
void printErrors();
char *dynamicSprintf(const char *format, ...);
