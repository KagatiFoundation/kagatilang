#ifndef KAG_GC_H
#define KAG_GC_H

#define INIT_HEAP_SIZE 100
#define MACH_PTR_SIZE sizeof(void*)
#define MAX_OBJECTS 1000

#include <stdlib.h>
#include <string.h>
#include "object.h"

#ifdef __cplusplus
extern "C" {
#endif

extern const uint64_t GC_OFFSET_DATA;
extern const uint64_t GC_OFFSET_CHILDREN;

typedef struct _RootStack {
    struct _Object ***roots;
    size_t count;
    size_t capacity;
} RootStack;

// initialize the GC
void init_gc();

void gc_mark(Object *root);

size_t count_live_objects();

Object* make_rt_int(uint64_t);

Object* make_rt_str(void*, uint64_t);

Object* make_rt_rec(uint64_t);

void dbg_print_heap();

void gc_collect();

void gc_push_root(Object **root);

void gc_pop_roots(size_t n);

#ifdef _cplusplus
}
#endif

#endif
