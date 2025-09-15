#include "io/print.h"
#include <unistd.h>
#include <stdio.h>

void print(K_Object *obj) {
    if (obj->ob_type == K_REC) {
        printf("%p", obj);
        return;
    }

    if (obj->ob_type != K_STR) return;
    write(STDOUT_FILENO, obj->data, obj->ob_size);
    fflush(stdout);
}