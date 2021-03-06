//===---------- utils.c - Utility functions and data structures -----------===/
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ----------------------------------===/
//
// This file provides several helper data structures and functions, including a
// generic hashtbl, vector, and a general purpose Symbol type
//
//===------------------------------ Todo ----------------------------------===/
//
// * Make the hashtable completely generic and provide type safe wrappers
// * Provide type safe wrappers for the vector implementation
//
//===----------------------------------------------------------------------===/

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>

/* ------------------------------ Helpers ---------------------------------- */

/* Rounds up the value to the nearest power of 2 */
static size_t roundup(size_t bottomValue) {
    size_t ret = 1;
    while (ret <= bottomValue) {
        ret *= 2;
    }
    return ret;
}

/* Sprintf but it dynamically allocates memory */
char *msprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    va_list args2;
    va_copy(args2, args);

    size_t memNeeded = vsnprintf(NULL, 0, format, args) + 1;
    char *buffer = calloc(1, memNeeded * sizeof(char));
    if (buffer == NULL) {
        printf("Compiler internal error: Out of memory.\n");
        exit(1);
    }
    vsprintf(buffer, format, args2);
    return buffer;
}

/* ------------------------------ Symbol ----------------------------------- */

bool compareSymbol(Symbol sym1, Symbol sym2) {
    if (sym1.len != sym2.len) {
        return false;
    }
    const char *castSym1 = (const char *)sym1.text;
    const char *castSym2 = (const char *)sym2.text;
    size_t length = sym1.len < sym2.len ? sym1.len : sym2.len;
    return strncmp(castSym1, castSym2, length) == 0;
}

/* Compares a Symbol to a null terminated string */
bool compareSymbolStr(Symbol sym, const char *str) {
    if (sym.len != strlen(str)) {
        return false;
    }
    size_t stringLength = strlen(str);
    /* Smallest size of the two, to make sure a buffer overrun doesn't occur */
    size_t length = sym.len < stringLength ? sym.len : stringLength;
    for (size_t i = 0; i < length && str[i] != '\0'; i++) {
        if (sym.text[i] != str[i]) {
            return false;
        }
    }
    return true;
}

/* ------------------------------ Hashtbl ---------------------------------- */

Hashtbl *newHashtbl(size_t initBuckets) {
    Hashtbl *ret = calloc(1, sizeof(Hashtbl));
    ret->numBuckets = roundup(initBuckets);
    ret->usedBuckets = ret->entries = 0;
    ret->buckets = calloc(ret->numBuckets, sizeof(HashEntry *));
    return ret;
}

size_t spow(size_t base, size_t exp) {
    size_t result = 1;
    for (;;) {
        if (exp & 1) result *= base;
        exp >>= 1;
        if (!exp) break;
        base *= base;
    }

    return result;
}

static size_t hashSymbol(Symbol sym) {
    size_t ret = 0;
    for (size_t i = 0; i < sym.len; i++) {
        ret += spow(10, i) * sym.text[i];
    }
    return ret;
}

/*
 * This is implemented manually instead of just calling the other two hashtbl
 * functions, because I dont need to hash the symbol twice, and only have to
 * insert once
 */
HashEntry *findOrInsertHashtbl(Hashtbl *tbl, Symbol sym, void *data) {
    size_t hash = hashSymbol(sym) % tbl->numBuckets;

    /* Look for entry that already exists */
    for (HashEntry *entry = tbl->buckets[hash]; entry != NULL;
         entry = entry->next) {
        if (compareSymbol(sym, entry->id)) {
            return entry;
        }
    }

    /* No entries exist, so just allocate a new one */
    HashEntry *oldEntry = tbl->buckets[hash];
    HashEntry *newEntry = calloc(1, sizeof(HashEntry));
    newEntry->id = sym;
    newEntry->data = data;
    newEntry->next = oldEntry;
    tbl->buckets[hash] = newEntry;
    return newEntry;
}

HashEntry *insertHashtbl(Hashtbl *tbl, Symbol sym, void *data) {
    size_t hash = hashSymbol(sym) % tbl->numBuckets;

    for (HashEntry *entry = tbl->buckets[hash]; entry != NULL;
         entry = entry->next) {
        if (compareSymbol(sym, entry->id)) {
            /* If it already exists, return NULL, signaling failure to insert
             */
            return NULL;
        }
    }

    /* No entries exist, so just allocate a new one */
    HashEntry *oldEntry = tbl->buckets[hash];
    HashEntry *newEntry = calloc(1, sizeof(HashEntry));
    newEntry->id = sym;
    newEntry->data = data;
    newEntry->next = oldEntry;
    tbl->buckets[hash] = newEntry;
    return newEntry;
}

HashEntry *findHashtbl(Hashtbl *tbl, Symbol sym) {
    size_t hash = hashSymbol(sym) % tbl->numBuckets;

    for (HashEntry *entry = tbl->buckets[hash]; entry != NULL;
         entry = entry->next) {
        if (compareSymbol(sym, entry->id)) {
            return entry;
        }
    }
    return NULL;
}

/* ------------------------------ Vector ----------------------------------- */

Vector *newVector(size_t itemSize, size_t initialSize) {
    Vector *ret = calloc(1, sizeof(Vector));
    ret->itemSize = itemSize;
    ret->itemCapacity = roundup(initialSize);
    ret->numItems = 0;

    if (initialSize == 0) {
        ret->data = NULL;
    } else {
        ret->data = calloc(itemSize, ret->itemCapacity);
        /* I am going to continue to believe that memory allocation failure
         * doesn't exist*/
        if (ret->data == NULL) {
            return NULL;
        }
    }

    return ret;
}

bool pushVector(Vector *vec, void *data) {
    if (vec->numItems + 1 >= vec->itemCapacity) {
        /* Time to reallocate */
        size_t newSize = vec->itemCapacity * 2;
        void *newBuffer = realloc(vec->data, newSize * vec->itemSize);
        if (newBuffer == NULL) {
            return false;
        }
        vec->data = newBuffer;
        vec->itemCapacity = newSize;
    }

    memcpy(&((char *)vec->data)[vec->numItems++ * vec->itemSize], data,
           vec->itemSize);
    return true;
}

/* Returns a pointer to the value indexed */
void *indexVector(Vector *vec, size_t index) {
    return &((char *)vec->data)[index * vec->itemSize];
}

/* ------------------------------ Scope ------------------------------------ */

/* Searches through a scope to find an entry for a symbol */
HashEntry *findInScope(Scope *scope, Symbol sym) {
    for (; scope != NULL; scope = scope->upScope) {
        HashEntry *entry = findHashtbl(scope->vars, sym);
        if (entry != NULL) {
            return entry;
        }
    }
    return NULL;
}
