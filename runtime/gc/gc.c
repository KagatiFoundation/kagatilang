#include "gc.h"
#include <assert.h>
#include <stdio.h>

gc_object_t* kgc_alloc(size_t size) {
    if (size == 0) {
        fprintf(stderr, "kgc_alloc: cannot allocate zero size\n");
        return NULL;
    }

    gc_object_t* obj = (gc_object_t*)malloc(sizeof(gc_object_t));
    if (!obj) return NULL;

    obj->ref_count = 1;
    obj->size = size;
    obj->num_children = 0;
    obj->children = NULL;
    return obj;
}

void kgc_retain(gc_object_t* obj) {
    if (obj) {
        obj->ref_count += 1;
    }
}

void kgc_release(gc_object_t* obj) {
    if (!obj) return;

    if (--obj->ref_count == 0) {
        // release all children
        for (size_t i = 0; i < obj->num_children; ++i) {
            kgc_release(obj->children[i]);
        }

        free(obj->children);
        free(obj);
    }
}

void kgc_add_child(gc_object_t* parent, gc_object_t* child) {
    if (!parent || !child) return;

    gc_object_t** children_array = (gc_object_t**) realloc(
        parent->children, 
        (parent->num_children + 1) * sizeof(gc_object_t*)
    );

    if (!children_array) {
        fprintf(stderr, "kgc_add_child: realloc failed\n");
        return;
    }

    parent->children = children_array;
    parent->children[parent->num_children] = child;
    parent->num_children += 1;

    kgc_retain(child);  // child ref count incremented
}