#include <errno.h>
#include <error.h>
#include <fcntl.h>
#include <lexer.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("ERROR: Usage \"bcc (filename) [flags]\".\n");
        exit(1);
    }
    const char *filename = argv[1];

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

    if (mapped_file == MAP_FAILED) {
        printf("ERROR: Failed to memory map file: '%s' due to error: '%s'.\n",
               filename, strerror(errno));
        exit(1);
    }

    initErrors(mapped_file, filename);

    Lexer *lex = newLexer(mapped_file, file_stats.st_size);

    Parser *pars = newParser(lex);

    for (;;) {
        if (peekToken(lex).type == TOK_EOF) {
            break;
        }
        Stmt *stmt = parseStmt(pars);
        if (errorsExist()) {
            printErrors();
            exit(1);
        }
        printStmt(stmt);
        printf("\n");
    }
}

