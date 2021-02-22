#pragma once

#include <stdbool.h>
#include <stddef.h>

/* This is kind of bad, and should be improved for better error messages
 * However, currently is supports multiple error messages and soon will format
 * them much better
 */

typedef struct Error {
    char *message;
    size_t start;
    size_t end;
    struct Error *next;
} Error;

void queueError(char *message, size_t start, size_t end);
void printErrors();
char *dynamicSprintf(const char *format, ...);
bool errorsExist();
void initErrors(const unsigned char *newBuffer);
