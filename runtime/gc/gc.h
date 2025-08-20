#ifndef KAG_GC_H
#define KAG_GC_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Garbage collector object
 */
typedef struct gc_object {
    int             ref_count;
    size_t          size;
    size_t          num_children;
    gc_object_t**   children;
} gc_object_t;

// allocate a GC object
gc_object_t* kgc_alloc(size_t size);

// increase the reference count
void kgc_retain(gc_object_t* obj);

// free the allocated record
void kgc_release(gc_object_t* obj);

void kgc_add_child(gc_object_t* obj, gc_object_t* child);

#ifdef __cplusplus
}
#endif

#endif // KAG_GC_H