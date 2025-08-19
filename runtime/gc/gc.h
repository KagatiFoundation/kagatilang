#ifndef KAG_GC_H
#define KAG_GC_H

#include <stdlib.h>

/**
 * Garbage collector object
 */
typedef struct gc_object {
    int     ref_count;
    size_t  size;
    void*   data;
} gc_object_t;

// allocate a GC object
gc_object_t* alloc_object(size_t size);

// increase the reference count
void retain(gc_object_t* obj);

// free the allocated record
void release(gc_object_t* obj);

#endif