#include "print.h"
#include <stdio.h>

void print(Object *obj) {
    if (obj->ob_type == K_REC) {
        fputs("Record {}", stdout);
        return;
    }
    if (obj->ob_type == K_STR) {
        fprintf(stdout, "%s", (char*) obj->data);
    }
    else if (obj->ob_type == K_INT) {
        fprintf(stdout, "%llu\n", *((uint64_t*) obj->data));
    }
    fflush(stdout);
}

void print_int(long long x) {
    printf("%lld\n", x);
}