#pragma once

#include <stddef.h>

typedef struct {
    const unsigned char *text;
    size_t len;
} Symbol;

void printSymbol(Symbol sym);
int compareSymbol(Symbol sym1, Symbol sym2);

typedef struct HashEntry {
    Symbol id;
    void *data;
    struct HashEntry *next;
} HashEntry;

typedef struct {
    size_t numBuckets;
    size_t usedBuckets;
    size_t entries;
    HashEntry **buckets;
} Hashtbl;

Hashtbl *newHashtbl(size_t initBuckets);
HashEntry *findOrInsertHashtbl(Hashtbl *tbl, Symbol sym, void *data);

/* Returns NULL if the entry is already found */
HashEntry *insertHashtbl(Hashtbl *tbl, Symbol sym, void *data);
HashEntry *findHashtbl(Hashtbl *tbl, Symbol sym);
