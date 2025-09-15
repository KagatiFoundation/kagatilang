#ifndef KAG_GC_H
#define KAG_GC_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum _object_type {
    K_INT = 0,
    K_STR = 1,
    K_REC = 2
} K_Object_Type;

typedef struct _object {
    uint64_t            ref_count;
    uint64_t            ob_size;
    uint64_t            num_children;
    struct _object**    children;
    enum _object_type   ob_type;
    uint8_t*            data;
} K_Object;

K_Object* object_new(size_t size, K_Object_Type type);

void object_delete(K_Object *obj);

/**
 * Garbage collector object
 */
typedef struct gc_object {
    uint64_t            ref_count;
    uint64_t            size;
    uint64_t            num_children;
    struct gc_object**  children;
    uint8_t*            data;
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