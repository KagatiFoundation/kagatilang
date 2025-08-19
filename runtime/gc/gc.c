#include "gc.h"
#include <assert.h>

gc_object_t* alloc_object(size_t size) {
    assert(size != 0);

    gc_object_t* obj = malloc(sizeof(gc_object_t));
    obj->ref_count = 1;
    obj->size = size;
    obj->data = malloc(size);
    return obj;
}

void retain(gc_object_t* obj) {
    obj->ref_count += 1;
}

void release(gc_object_t* obj) {
    if (--obj->ref_count == 0) {
        free(obj->data);
        free(obj);
    }
}