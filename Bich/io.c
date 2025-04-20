#include <stdio.h>

void println(char *str) {
    fprintf(stdout, "%s\n", str);
}

void eprintln(char *str) {
    fprintf(stderr, "%s\n", str);
}