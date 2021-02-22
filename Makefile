CC=gcc

OBJ_DIR=objs
SRC_DIR=src
INCLUDE_DIR=include

CFLAGS=-g -O0 -Wall -Werror -Wextra -I$(INCLUDE_DIR)
LDFLAGS=-g

SRC_FILES=$(wildcard $(SRC_DIR)/*.c)
OBJ_FILES=$(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))

bcc: $(OBJ_FILES)
	$(CC) $(LDFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

run: bcc
	./bcc
