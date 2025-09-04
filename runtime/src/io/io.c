#include <stdio.h>

void print_int(int value) {
    printf("%d\n", value);
}

void println(char *str) {
    puts(str);
}

void eprintln(char *str) {
    fputs(str, stderr);
}