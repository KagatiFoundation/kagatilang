#include "gc/gc_str.h"

gc_object_t* kgc_str_new(const char* s) {
    size_t len = kgc_strlen(s) + 1;
    gc_object_t* obj = kgc_alloc(len);
    if (!obj) return NULL;

    k_memcpy(obj->data, s, len);
    return obj;
}

size_t kgc_strlen(const char* s) {
    size_t len = 0;
    while (*s++) len++;
    return len;
}

void k_memcpy(void* dst, const char* src, size_t n) {
    char *d = dst;
    for (size_t i = 0; i < n; i++) {
        d[i] = src[i];
    }
}