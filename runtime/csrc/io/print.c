#include "print.h"
#include <stdio.h>

void print(char* value) {
    fprintf(stdout, "%s", value);
    fflush(stdout);
}

void println(char* value) {
    fprintf(stdout, "%s\n", value);
    fflush(stdout);
}

void eprintln(char* value) {
    fprintf(stderr, "%s\n", value);
    fflush(stderr);
}

void print_int(long long x) {
    printf("%lld\n", x);
}