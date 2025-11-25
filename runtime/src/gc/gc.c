#include "gc/gc.h"
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

K_Object* object_new(size_t size, K_Object_Type type) {
   if (size == 0) {
        fprintf(stderr, "kgc_alloc: cannot allocate zero size\n");
        return NULL;
    }

    K_Object* obj = (K_Object*) malloc(sizeof(K_Object));
    if (!obj) return NULL;

    obj->ref_count = 1;
    obj->ob_size = size;
    obj->num_children = 0;
    obj->children = NULL;
    obj->ob_type = type;

    obj->data = malloc(size);
    if (!obj->data) {
        puts("invalid data pointer");
        free(obj);
        return NULL;
    }
    
    return obj; 
}

void object_copy(void* dest, void *src, size_t size) {
    memcpy(dest, src, size);
}

void obj_alloc_fail()   { 
    const char msg[] = "object_new failed\n";
    write(STDERR_FILENO, msg, sizeof(msg)-1);
    _exit(1);
}

void object_delete(K_Object *obj) {
    if (!obj) return;

    if (--obj->ref_count == 0) {
        // release all children
        for (size_t i = 0; i < obj->num_children; ++i) {
            object_delete(obj->children[i]);
        }

        free(obj->children);
        free(obj->data);
        free(obj);
    }
}

/*
void kgc_alloc_fail()   { 
    const char msg[] = "kgc_alloc failed\n";
    write(STDERR_FILENO, msg, sizeof(msg)-1);
    _exit(1);
}

void kgc_memcpy_fail()   { 
    const char msg[] = "kgc_memcpy failed\n";
    write(STDERR_FILENO, msg, sizeof(msg)-1);
    _exit(1);
}

void kgc_retain(gc_object_t* obj) {
    if (obj) {
        obj->ref_count += 1;
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
    */