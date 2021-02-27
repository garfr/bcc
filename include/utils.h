//===------- utils.h - Utility functions and data structures header -------===/
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------------------------------------------------===/
//
// Defines the types and provides prototypes for functions in utils.c
//
//===----------------------------------------------------------------------===/

#pragma once

#include <stdbool.h>
#include <stddef.h>

/* A fat pointer to a location in the file being compiled */ typedef struct {
    const unsigned char *text;
    size_t len;
} Symbol;

/* Works the same way strcmp does */
int compareSymbol(Symbol sym1, Symbol sym2);

/* Compares a Symbol to a null terminated string */
int compareSymbolStr(Symbol sym, const char *str);

/*
 * A hashtable that hashes from a Symbol to some unspecified value
 * This will later be made generic and given typesafe wrappers over it
 * This is currently implemented through chaining, but I am open to more
 * efficient methods
 */
typedef struct HashEntry HashEntry;

typedef struct {
    size_t numBuckets;
    size_t usedBuckets;
    size_t entries;
    HashEntry **buckets;
} Hashtbl;

typedef struct HashEntry {
    Symbol id;
    void *data;
    struct HashEntry *next;
} HashEntry;

Hashtbl *newHashtbl(size_t initBuckets);
/* This is the find function you want most of the time */
HashEntry *findOrInsertHashtbl(Hashtbl *tbl, Symbol sym, void *data);
HashEntry *findHashtbl(Hashtbl *tbl, Symbol sym);
/* Returns NULL if the entry is already found */
HashEntry *insertHashtbl(Hashtbl *tbl, Symbol sym, void *data);

/*
 * A completely generic resizeable array which actually holds the data passed to
 * it may give this some typesafe wrappers in the future
 */
typedef struct {
    void *data;
    size_t itemSize;
    /* Both of these values are in number of entries, not number of bytes */
    size_t itemCapacity;
    size_t numItems;
} Vector;

/* Item size is the number of bytes needed to store the type in the vector */
Vector *newVector(size_t itemSize, size_t initialSize);
bool pushVector(Vector *vec, void *data);
void *indexVector(Vector *vec, size_t index);

/* Sprintf but it mallocs memory */
char *msprintf(const char *format, ...);
