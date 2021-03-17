#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

// clang-format off
#include "bcc/pp.h"
#include "bcc/lexer.h"
#include "bcc/error.h"
#include "bcc/parser.h"
#include "bcc/sem_names.h"
#include "bcc/sem_types.h"
#include "bcc/codegen.h"
// clang-format on 

int main(int argc, char *argv[]) {
    opterr = 0;

    int c;
    char *outputname = NULL;

    bool makeExecutable = true;

    if (argc < 2) {
        printf("ERROR: Correct format: bcc (filename) [options].\n");
        exit(1);
    }
    char *filename = argv[1];

    while ((c = getopt(argc - 1, &argv[1], "co:")) != -1) {
        switch (c) {
        case 'c':
            makeExecutable = false;
            break;
        case 'o':
            outputname = optarg;
            break;
        }
    }

    /* mmap the file open */
    int fd = open(filename, O_RDONLY);

    if (fd < 0) {
        printf("ERROR: Failed to open file at '%s', due to error: '%s'.\n",
               filename, strerror(errno));
        exit(1);
    }

    struct stat file_stats;
    int status = fstat(fd, &file_stats);

    if (status < 0) {
        printf("Failed to fetch stats on file: '%s' due to error: '%s'.\n",
               filename, strerror(errno));
        exit(1);
    }

    const unsigned char *mapped_file =
        mmap(NULL, file_stats.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

    if (file_stats.st_size <= 0) {
        printf("ERROR: Source files cannot be completely empty.\n");
        exit(1);
    }
    if (mapped_file == MAP_FAILED) {
        printf("ERROR: Failed to memory map file: '%s' due to error: '%s'.\n",
               filename, strerror(errno));
        exit(1);
    }

    initErrors(mapped_file, file_stats.st_size, filename);

    Lexer lex = newLexer(mapped_file, file_stats.st_size);

    AST *ast = parseSource(&lex);
    resolveNames(ast);
    annotateAST(ast);

    /*printAST(ast);*/

    if (errorsExist()) {
        printErrors();
    }

    if (outputname == NULL) {
        if (makeExecutable) {
            outputname = "a.out";
            FILE *file = fopen(outputname, "w");
            generateCode(ast, file);
            fclose(file);
            system(msprintf("mv %s %s.ssa", outputname, outputname));
            if (system(msprintf("./qbe %s.ssa -o %s.s", outputname,
                                outputname)) == 0) {
                system(msprintf("gcc %s.s -o %s", outputname, outputname));
                system(msprintf("rm %s.s %s.ssa", outputname, outputname));
            } else {
                system(msprintf("rm %s.ssa", outputname, outputname));
            }
        } else {
            FILE *file = fopen("temp.temp.temp.ssa", "w");
            generateCode(ast, file);
            fclose(file);
            if (system("./qbe temp.temp.temp.ssa") == 0) {
                system("rm temp.temp.temp.ssa");
            } else {
                system(
                    msprintf("rm temp.temp.temp.ssa", outputname, outputname));
            }
        }
    } else {
        if (makeExecutable) {
            FILE *file = fopen(outputname, "w");
            generateCode(ast, file);
            fclose(file);
            system(msprintf("mv %s %s.ssa", outputname, outputname));
            if (system(msprintf("./qbe %s.ssa -o %s.s", outputname,
                                outputname)) == 0) {
                system(msprintf("gcc %s.s -o %s", outputname, outputname));
                system(msprintf("rm %s.s %s.ssa", outputname, outputname));
            } else {
                system(msprintf("rm %s.ssa %s.s", outputname, outputname));
            }
        } else {
            FILE *file = fopen(outputname, "w");
            generateCode(ast, file);
            fclose(file);
            system(msprintf("mv %s %s.ssa", outputname, outputname));
            if (system(msprintf("./qbe %s.ssa -o %s.s", outputname,
                                outputname)) == 0) {
                system(msprintf("gcc %s.s -o %s -c", outputname, outputname));
                system(msprintf("rm %s.s %s.ssa", outputname, outputname));
            } else {
                system(msprintf("rm %s.ssa", outputname, outputname));
            }
        }
    }
}

