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
