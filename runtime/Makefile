UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
	BUILD_PATH := /tmp/kaglang
endif
ifeq ($(UNAME),Linux)
	BUILD_PATH := /tmp/kaglang
endif

INCLUDE_DIR=./include
SRCS=$(shell find ./csrc -name '*.c')
TESTS=$(shell find ./tests -name '*.c')

OBJS=$(SRCS:.c=.o)

CFLAGS_BUILD_1=-c -Wall -Werror -fPIC
CFLAGS_BUILD_2=-shared

CC=gcc

CFLAGS_TEST = -Wall -Wextra -fsanitize=address -g
LDFLAGS_TEST = -lpthread -lm

.PHONY: all clean tests
all: libkag.so

libkag.so: $(OBJS) | $(BUILD_PATH)
	$(CC) $(CFLAGS_BUILD_2) -o $(BUILD_PATH)/$@ $(OBJS)

%.o: %.c
	$(CC) $(CFLAGS_BUILD_1) -o $@ $<

tests:
	$(CC) $(CFLAGS_TEST) $(SRCS) $(TESTS) -o test_gc $(LDFLAGS_TEST)

$(BUILD_PATH):
	mkdir -p $(BUILD_PATH)

clean:
	rm -rf $(OBJS) $(BUILD_PATH)/libkag.so test_gc test_gc.dSYM