#ifndef KAG_GC_STR_H
#define KAG_GC_STR_H

#include <stdlib.h>
#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

gc_object_t* kgc_str_new(const char* s);

size_t kgc_strlen(const char* s);

void k_memcpy(void* dst, const char* src, size_t n);

#ifdef __cplusplus
}
#endif

#endif // KAG_GC_STR_H