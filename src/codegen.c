#include <codegen.h>
#include <error.h>
#include <parser.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <utils.h>

/* This is an extremely basic code generator.
 * It performs no optimizations, and allocates and assigns registers
 * in a very crude way.
 * It generates code x86_64 in the NASM format, utilizing no predefined macros.
 * One day this will be replaced with a homegrown assembler builtin to the
 * compiler */

/* Utilize the constant strings instead of stack allocating a new for each
 * function */
const char* reg64[] = {"rax", "rdi", "rsi", "rdx", "rcx",
                       "r8",  "r9",  "r10", "r11"};
const char* reg32[] = {"eax", "edi", "esi",  "edx", "ecx",
                       "r8d", "r9d", "r10d", "r11d"};
const char* reg16[] = {"ax",  "di",  "si",   "dx",  "cx",
                       "r8w", "r9w", "r10w", "r11w"};
const char* reg8[] = {"al",  "dl",  "sil",  "dl",  "cl",
                      "r8b", "r9b", "r10b", "r11b"};

enum Register { RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11 };

/* Returns the size in bytes needed to store the given type */
size_t calculateSize(Type* type) {
    switch (type->type) {
        case TYP_SINT:
            return (type->intbits + (8 / 2)) / 8;
        case TYP_INTLIT:
            printf(
                "Internal compiler error: Cannot calculate the storage size of "
                "an integer literal.\n");
            exit(1);
    }
    return 0;
}

const char* getRegister(enum Register reg, size_t bytes) {
    switch (bytes) {
        case 1:
            return reg8[(size_t)reg];
        case 2:
            return reg16[(size_t)reg];
        case 4:
            return reg32[(size_t)reg];
        case 8:
            return reg64[(size_t)reg];
        default:
            printf(
                "Internal compiler error: Cannot use types of size 1, 2, 4 or "
                "8.\n");
            exit(1);
    }
}

typedef struct {
    bool regs[9];
    Hashtbl* newSyms;
    size_t currOffset;
} RegAlloc;

typedef struct {
    enum {
        LOC_REG,
        LOC_STACK,
    } type;

    union {
        enum Register reg;
        size_t stackOffset;
    };

} ValueLocation;

typedef struct {
    Type* type;
    ValueLocation loc;
} CodegenEntry;

/*
 * This register allocator simply gives variables and temporariesw in the TAC a
 * register as it goes through them, and when its out of registers starts
 * slapping them onto the stack This is very slow and limiting, and certainly
 * will be changed in later iterations of the code generator
 */
HashEntry* allocateRegister(HashEntry* entry, RegAlloc* alloc) {
    for (int i = 0; i < 9; i++) {
        if (alloc->regs[i]) {
            alloc->regs[i] = false;
            CodegenEntry* newData = malloc(sizeof(CodegenEntry));
            newData->type = ((TypedEntry*)entry->data)->type;
            newData->loc =
                (ValueLocation){.type = LOC_REG, .reg = (enum Register)i};
            return insertHashtbl(alloc->newSyms, entry->id, newData);
        }
    }
    CodegenEntry* newData = malloc(sizeof(CodegenEntry));
    newData->type = ((TypedEntry*)entry->data)->type;
    newData->loc =
        (ValueLocation){.type = LOC_STACK, .stackOffset = alloc->currOffset};

    /* Update the offset */
    alloc->currOffset += calculateSize(((TypedEntry*)entry->data)->type);
    return insertHashtbl(alloc->newSyms, entry->id, newData);
}

void generateTACInst(RegAlloc* alloc, TACInst* inst, FILE* output) {
    const char* args[3];
    size_t firstSize;
    for (int i = 0; i < 3; i++) {
        switch (inst->args[i].type) {
            case ADDR_VAR: {
                HashEntry* fullEntry =
                    findHashtbl(alloc->newSyms, inst->args[i].var->id);
                if (fullEntry == NULL) {
                    fullEntry = allocateRegister(inst->args[i].var, alloc);
                }

                CodegenEntry* entry = ((CodegenEntry*)fullEntry->data);

                if (i == 2) {
                    firstSize = calculateSize(entry->type);
                }

                switch (entry->loc.type) {
                    case LOC_REG:
                        args[i] = getRegister(entry->loc.reg,
                                              calculateSize(entry->type));
                        break;
                    case LOC_STACK:
                        args[i] =
                            msprintf("[rsp - %zd]", entry->loc.stackOffset);
                        break;
                }
            } break;
            case ADDR_TEMP:
                printf("Internal compiler error.\n");
                exit(1);
            case ADDR_INTLIT:
                args[i] = msprintf("%.*s", inst->args[i].intlit.len,
                                   inst->args[i].intlit.text);
                break;
            case ADDR_EMPTY:
                args[i] = NULL;
        }
    }
    switch (inst->op) {
        char* sizeSpec;
        case OP_COPY: {
            switch (firstSize) {
                case 1:
                    sizeSpec = "BYTE";
                    break;
                case 2:
                    sizeSpec = "WORD";
                    break;
                case 4:
                    sizeSpec = "DWORD";
                    break;
                case 8:
                    sizeSpec = "QWORD";
                    break;
                default:
                    printf("Internal compiler error %ld.\n", firstSize);
                    exit(1);
            }

            fprintf(output, "mov %s %s, %s\n", sizeSpec, args[2], args[0]);
        }
    }
}

RegAlloc* newRegisterAllocator(Hashtbl* symTable) {
    RegAlloc* ret = malloc(sizeof(RegAlloc));
    for (int i = 0; i < 9; i++) {
        ret->regs[i] = true;
    }
    ret->newSyms = newHashtbl(symTable->numBuckets);
    return ret;
}

void generateCode(TAC* tac, Hashtbl* symTable, FILE* output) {
    fprintf(output, "global main\n\nmain:\n\tpush rbp\n\tmov rbp, rsp\n");

    RegAlloc* registers = newRegisterAllocator(symTable);

    for (size_t i = 0; i < tac->codes->numItems; i++) {
        fprintf(output, "\t");
        TACInst* inst = *((TACInst**)indexVector(tac->codes, i));
        generateTACInst(registers, inst, output);
    }
    fprintf(output, "\tmov eax, 0\n\tpop rbp\n\tret\n");
}
