#include "io/print.h"
#include <unistd.h>
#include <stdio.h>

void print(Object *obj) {
    if (obj->ob_type == K_REC) {
        printf("%p", obj);
        return;
    }

    if (obj->ob_type != K_STR) return;
    write(STDOUT_FILENO, obj->data, obj->ob_size);
    fflush(stdout);
}

void print_int(long long x) {
    printf("%lld\n", x);
}