#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>

#define MIN(a, b) a < b ? a : b

void printSymbol(Symbol sym) {
    for (size_t i = 0; i < sym.len; i++) {
        putchar(sym.text[i]);
    }
}

int compareSymbol(Symbol sym1, Symbol sym2) {
    if (sym1.len != sym2.len) {
        return -1;
    }
    return strncmp((const char *)sym1.text, (const char *)sym2.text,
                   MIN(sym1.len, sym2.len));
}

static size_t roundup(size_t bottomValue) {
    size_t ret = 1;
    while (ret <= bottomValue) {
        ret *= 2;
    }
    return ret;
}

Hashtbl *newHashtbl(size_t initBuckets) {
    Hashtbl *ret = malloc(sizeof(Hashtbl));
    ret->numBuckets = roundup(initBuckets);
    ret->usedBuckets = ret->entries = 0;
    ret->buckets = calloc(ret->numBuckets, sizeof(HashEntry *));
    return ret;
}

static size_t power(size_t base, size_t exp) {
    size_t ret = base;
    while (exp != 0) {
        ret *= base;
        exp--;
    }
    return ret;
}

static size_t hashSymbol(Symbol sym) {
    size_t ret = 0;
    for (size_t i = 0; i < sym.len; i++) {
        ret += power(10, i) * sym.text[i];
    }
    return ret;
}

HashEntry *findOrInsertHashtbl(Hashtbl *tbl, Symbol sym, void *data) {
    size_t hash = hashSymbol(sym) % tbl->numBuckets;

    for (HashEntry *entry = tbl->buckets[hash]; entry != NULL;
         entry = entry->next) {
        if (compareSymbol(sym, entry->id) == 0) {
            return entry;
        }
    }

    HashEntry *oldEntry = tbl->buckets[hash];
    HashEntry *newEntry = malloc(sizeof(HashEntry));
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
        if (compareSymbol(sym, entry->id) == 0) {
            return NULL;
        }
    }

    HashEntry *oldEntry = tbl->buckets[hash];
    HashEntry *newEntry = malloc(sizeof(HashEntry));
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
        if (compareSymbol(sym, entry->id) == 0) {
            return entry;
        }
    }
    return NULL;
}
