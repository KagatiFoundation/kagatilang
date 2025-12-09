#ifndef KAG_GC_H
#define KAG_GC_H

#define INIT_HEAP_SIZE 100
#define MACH_PTR_SIZE sizeof(void*)
#define MAX_OBJECTS 1000

#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum ObjectType {
    K_INT = 0,
    K_STR = 1,
    K_REC = 2
} ObjectType;

typedef struct _Object {
    uint64_t            marked;
    uint64_t            ob_size;
    uint64_t            num_children;
    struct _Object**    children;
    struct _Object*    next;
    enum ObjectType     ob_type;
    uint8_t*            data;
} Object;

Object* object_new(size_t size, ObjectType type, void* src);

typedef struct _RootStack {
    struct _Object ***roots;
    size_t count;
    size_t capacity;
} RootStack;

// initialize the GC
void init_gc();

void gc_mark(Object *root);

size_t count_live_objects();

void gc_collect();

#ifdef _cplusplus
}
#endif

#endif
