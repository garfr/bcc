CC=gcc
OBJ_DIR=objs
SRC_DIR=src
INCLUDE_DIR=include

CFLAGS=-O0 -g -Wall -Wextra -I$(INCLUDE_DIR)
LDFLAGS= -g

SRC_FILES=$(wildcard $(SRC_DIR)/*.c)
OBJ_FILES=$(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))
DEP = $(addsuffix .d,$(OBJ))

bcc: $(OBJ_FILES) 
	$(CC) $(LDFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

test: bcc test.bns
	./bcc test.bns > test.ssa
	./qbe test.ssa -o test.s
	gcc test.s -o test
