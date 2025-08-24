BUILD_PATH=..
SRCS=$(shell find . -name '*.c')
OBJS=$(SRCS:.c=.o)
OPTS_1=-c -Wall -Werror -fPIC
OPTS_2=-shared
CC=gcc

.PHONY: all
all: libkag.so

libkag.so: $(OBJS) | $(BUILD_PATH)
	$(CC) $(OPTS_2) -o $(BUILD_PATH)/$@ $(OBJS)

%.o: %.c
	$(CC) $(OPTS_1) -o $@ $<

.PHONY: clean
clean:
	rm -f *.o $(BUILD_PATH)/libkag.so