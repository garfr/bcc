#include <errno.h>
#include <error.h>
#include <fcntl.h>
#include <parser.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <lexer.h>

int main() {
    const char *filename = "test.bns";

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

    Lexer *lex = newLexer(mapped_file, file_stats.st_size);

    Parser *pars = newParser(lex);

    for (;;) {
        if (peekToken(lex).type == TOK_EOF) {
            break;
        }
        printStmt(parseStmt(pars));
    }

    printErrors();
}

