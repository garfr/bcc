
OBJ_DIR=objs
SRC_DIR=src
INCLUDE_DIR=include

CFLAGS=-O0 -g -Wall -Werror -Wextra -MMD -MP -I$(INCLUDE_DIR)
LDFLAGS= -g

SRC_FILES=$(wildcard $(SRC_DIR)/*.c)
OBJ_FILES=$(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))
DEP = $(addsuffix .d,$(OBJ))

bcc: $(OBJ_FILES)
	$(CC) $(LDFLAGS) -o $@ $^

-include $(DEP)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

test: bcc
	./bcc test.bns > test.asm
	nasm -g -felf64 test.asm
	gcc -g test.o
	./a.out
