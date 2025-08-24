#include "gc_str.h"

gc_object_t* kgc_str_new(const char* s) {
    size_t len = kgc_strlen(s) + 1;
    gc_object_t* obj = kgc_alloc(len);
    if (!obj) return NULL;

    kgc_memcpy(obj->data, s, len);
    return obj;
}

size_t kgc_strlen(const char* s) {
    size_t len = 0;
    while (*s++) len++;
    return len;
}

void kgc_memcpy(char* dst, const char* src, size_t n) {
    for (size_t i = 0; i < n; i++) {
        dst[i] = src[i];
    }
}