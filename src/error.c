#include <error.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Error *errorQueue = NULL;

void queueError(char *message, size_t start, size_t end) {
    Error *err = malloc(sizeof(Error));
    err->message = message;
    err->start = start;
    err->end = end;

    Error *oldErr = errorQueue;
    errorQueue = err;
    err->next = oldErr;
}

/* TODO: Improve error printing. */
void printError(Error *err) {
    printf("ERROR: %s.\n%zd - %zd\n\n", err->message, err->start, err->end);
}

void printErrors(void) __attribute__((noreturn));
void printErrors(void) {
    while (errorQueue != NULL) {
        printError(errorQueue);
        errorQueue = errorQueue->next;
    }
    exit(1);
}

char *dynamicSprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    va_list args2;
    va_copy(args2, args);

    size_t memNeeded = vsnprintf(NULL, 0, format, args);
    char *buffer = malloc(memNeeded * sizeof(char));
    if (buffer == NULL) {
        printf("Compiler internal error: Out of memory.\n");
        exit(1);
    }
    vsprintf(buffer, format, args2);
    return buffer;
}
