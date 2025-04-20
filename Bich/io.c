#include <stdio.h>

void println(char *str) {
    puts(str);
    puts("returning from here");
}

void eprintln(char *str) {
    fprintf(stderr, "%s\n", str);
    return;
}