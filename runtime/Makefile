BUILD_PATH=..

INCLUDE_DIR=./include
SRCS=$(shell find ./src -name '*.c')
TESTS=$(shell find ./tests -name '*.c')

OBJS=$(SRCS:.c=.o)

CFLAGS_BUILD_1=-c -Wall -Werror -fPIC -I$(INCLUDE_DIR)
CFLAGS_BUILD_2=-shared

CC=gcc

CFLAGS_TEST = -Wall -Wextra -fsanitize=address -g -I$(INCLUDE_DIR)
LDFLAGS_TEST = -lpthread -lm

.PHONY: all clean tests
all: libkag.so

libkag.so: $(OBJS) | $(BUILD_PATH)
	$(CC) $(CFLAGS_BUILD_2) -o $(BUILD_PATH)/$@ $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS_BUILD_1) -o $@ $<

tests:
	$(CC) $(CFLAGS_TEST) $(SRCS) $(TESTS) -o test_gc $(LDFLAGS_TEST)

clean:
	rm -f $(OBJS) $(BUILD_PATH)/libkag.so test_gc