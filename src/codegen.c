//===------------- codegen.c - Generates code from the TAC ---------------===//
//
// Part of BCC, which is MIT licensed
// See https//opensource.org/licenses/MIT
//
//===----------------------------- About ---------------------------------===//
//
// Taking as input the TAC address code from tac.h, this produces an output in
// NASM format assembly. This code generation here is extremely primitive, with
// basic things like register allocation and instruction selection not
// implemented to any meaningful degree.
//
// For now it generates code for the NASM assembler, but a builtin assembler
// would probably be a good idea in the future.
//
//===------------------------------ Todo ---------------------------------===//
//
// * A proper register allocation process
// * A proper instruction selection process
// * More passes for more flexibility
//
//===---------------------------------------------------------------------===//

#include <codegen.h>
#include <error.h>
#include <parser.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utils.h>

/* The context for the primitive register allocator */
typedef struct {
    bool regs[9];
    Hashtbl* newSyms;
    size_t currOffset;
} RegAlloc;

/*
 * All the scratch registers allowed in the System V ABI, each index is the same
 * register across arrays
 */
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
            return type->intsize;
        case TYP_INTLIT:
            /* For now just use 64 bits, later some semantic stuff will happen
             */
            return 8;
    }
    return 0;
}

/* Given a register and a size in bytes, this returns the correct name of the
 * register */
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
    enum {
        LOC_REG,
        LOC_STACK,
    } type;

    union {
        enum Register reg;
        size_t stackOffset;
    };

} ValueLocation;

/* The values that symbols will be hashed too, including more information than
 * in the AST table */
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
HashEntry* allocateRegister(Type* valType, Symbol id, RegAlloc* alloc) {
    for (int i = 0; i < 9; i++) {
        /* Is this register available? */
        if (alloc->regs[i]) {
            alloc->regs[i] = false;

            /* Make new entry in the codegenerator symbol table */
            CodegenEntry* newData = malloc(sizeof(CodegenEntry));
            newData->type = valType;
            newData->loc =
                (ValueLocation){.type = LOC_REG, .reg = (enum Register)i};
            return insertHashtbl(alloc->newSyms, id, newData);
        }
    }

    /* No registers available, so stick it on the stack */
    CodegenEntry* newData = malloc(sizeof(CodegenEntry));
    newData->type = valType;
    newData->loc =
        (ValueLocation){.type = LOC_STACK, .stackOffset = alloc->currOffset};

    /* Update the offset */
    alloc->currOffset += calculateSize(valType);
    return insertHashtbl(alloc->newSyms, id, newData);
}

/* This is horrible and only works for something like copy or arithmetic */
void generateTACInst(RegAlloc* alloc, TACInst* inst, FILE* output) {
    const char* args[3];
    size_t firstSize;
    /* Converts all the addresses to a string */
    for (int i = 0; i < 3; i++) {
        switch (inst->args[i].type) {
            case ADDR_VAR: {
                HashEntry* oldEntry = inst->args[i].var;
                HashEntry* fullEntry =
                    findHashtbl(alloc->newSyms, oldEntry->id);
                if (fullEntry == NULL) {
                    fullEntry =
                        allocateRegister(((TypedEntry*)oldEntry->data)->type,
                                         oldEntry->id, alloc);
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
            case ADDR_TEMP: {
                /* Tempory values will be hashed into the symbol table with * as
                 * a prefix, this is hacky: sorry. */
                char* str = msprintf("*%d\n", inst->args[i].temp.num);
                Symbol sym =
                    (Symbol){.text = (unsigned char*)str, .len = strlen(str)};

                HashEntry* fullEntry = findHashtbl(alloc->newSyms, sym);
                if (fullEntry == NULL) {
                    fullEntry =
                        allocateRegister(inst->args[i].temp.type, sym, alloc);
                }

                CodegenEntry* entry = ((CodegenEntry*)fullEntry->data);

                if (i == 2) {
                    firstSize = calculateSize(inst->args[i].temp.type);
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
            case ADDR_INTLIT:
                args[i] = msprintf("%.*s", inst->args[i].intlit.len,
                                   inst->args[i].intlit.text);
                break;
            case ADDR_EMPTY:
                args[i] = NULL;
        }
    }
    char* sizeSpec;
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
    switch (inst->op) {
        case OP_COPY: {
            /* Must specify the size of the operation */

            fprintf(output, "mov %s %s, %s\n", sizeSpec, args[2], args[0]);
        } break;
        case OP_ADD: {
            /* The TAC has already been restructured during the fixArithmetic
             * pass, so just add val2 to the val1/dest */
            fprintf(output, "add %s %s, %s\n", sizeSpec, args[2], args[1]);
            break;
        }
        case OP_SUB: {
            fprintf(output, "sub %s %s, %s\n", sizeSpec, args[2], args[1]);
            break;
        }
        default:
            printf("Internal compiler error: cannot do mult or div.\n");
            exit(1);
    }
}

RegAlloc newRegisterAllocator(Hashtbl* symTable) {
    RegAlloc ret;
    for (int i = 0; i < 9; i++) {
        ret.regs[i] = true;
    }
    ret.newSyms = newHashtbl(symTable->numBuckets);
    ret.currOffset = 0;
    return ret;
}

void generateCode(TAC* tac, Hashtbl* symTable, FILE* output) {
    fprintf(output,
            "global main\n\nmain:\n\t; added by compiler\n\tpush rbp\n\tmov "
            "rbp, rsp\n\t; real code\n");

    RegAlloc registers = newRegisterAllocator(symTable);

    for (size_t i = 0; i < tac->codes->numItems; i++) {
        fprintf(output, "\t");
        TACInst* inst = *((TACInst**)indexVector(tac->codes, i));
        generateTACInst(&registers, inst, output);
    }

    fprintf(output, "\t; added by compiler\n\tmov eax, 0\n\tpop rbp\n\tret\n");
}
