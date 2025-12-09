#include "gc/gc.h"
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

// global object list (all allocated objects)
static Object *gc_objects = NULL;

static RootStack root_stack = {NULL, 0, 0};
static int gc_initialized = 0;
static size_t allocated_objects = 0;

void init_gc() {
    if (gc_initialized) return; // gc is already set; return

    root_stack.count = 0;
    root_stack.capacity = INIT_HEAP_SIZE;
    root_stack.roots = malloc(sizeof(RootStack**) * INIT_HEAP_SIZE);
    
    gc_objects = NULL;
    // gc is now initialized
    gc_initialized = 1;
}

static void ensure_root_capacity() {
    if (root_stack.count >= root_stack.capacity) {
        size_t newcap = root_stack.capacity == 0 ? 64 : root_stack.capacity * 2;
        root_stack.roots = realloc(root_stack.roots, newcap * sizeof(Object**));
        if (!root_stack.roots) { 
            perror("realloc roots"); 
            exit(1); 
        }
        root_stack.capacity = newcap;
    }
}

void gc_push_root(Object **root) {
    if (!gc_initialized) { 
        fprintf(stderr, "GC not initialized\n"); 
        abort(); 
    }
    ensure_root_capacity();
    root_stack.roots[root_stack.count++] = root;
}

void gc_pop_roots(size_t n) {
    assert(n <= root_stack.count);
    root_stack.count -= n;
}

static void register_object(Object *o) {
    o->next = gc_objects;
    gc_objects = o;
}

Object* object_new(size_t size, ObjectType type, void* src) {
    if (size == 0) {
        fprintf(stderr, "cannot allocate zero size\n");
        return NULL;
    }

    Object* obj = malloc(sizeof(Object));
    if (!obj) {
        perror("couldn't allocate object");
        exit(1);
    }
    
    if (!gc_initialized) {
        init_gc();
    }

    allocated_objects += 1; // new object
    if (allocated_objects >= MAX_OBJECTS) {
        gc_collect();
        allocated_objects = count_live_objects();
    }

    obj->ob_size = size;
    obj->num_children = 0;
    obj->children = NULL;
    obj->ob_type = type;

    if (type == K_REC) { // record type
        obj->num_children = size / MACH_PTR_SIZE;
        if (obj->num_children > 0) {
            obj->children = malloc(obj->num_children * MACH_PTR_SIZE);
            if (!obj->children) {
                free(obj);
                perror("couldn't allocate obj->children");
                exit(1);
            }
            memcpy(obj->children, src, size);
            obj->data = NULL; // no data for record type
        }
    }
    else {
        obj->children = NULL;
        obj->num_children = 0;
        obj->data = malloc(size);

        if (!obj->data) {
            free(obj);
            perror("couldn't allocate obj->data");
            exit(1);
        }
        memcpy(obj->data, src, size);
    }
    register_object(obj);
    return obj; 
}

void gc_mark(Object *root) {
    if (!root) return;

    size_t cap = INIT_HEAP_SIZE;
    Object **stack = malloc(cap * sizeof(Object*));
    if (!stack) return;

    size_t sp = 0; // stack pointer

    if (!root->marked) {
        root->marked = 1;
        stack[sp++] = root;
    }

    while (sp > 0) {
        Object *cur = stack[--sp];
        if (cur->ob_type == K_REC && cur->num_children > 0) {
            for (size_t i = 0; i < cur->num_children; i++) {
                Object *o = cur->children[i];
                if (o && !o->marked) {
                    o->marked = 1;
                    if (sp == cap) {
                        cap <<= 1;
                        stack = realloc(stack, cap * sizeof(Object*));
                        if (!stack) {
                            perror("realloc mark phase");
                            exit(1);
                        }
                    }
                    stack[sp++] = o;
                }
            }
        }
    }
    free(stack);
}

void gc_mark_all_roots(void) {
    for (size_t i = 0; i < root_stack.count; ++i) {
        Object **root = root_stack.roots[i];
        if (root && *root) gc_mark(*root);
    }
}

void gc_sweep() {
    Object **cur = &gc_objects;
    while (*cur) {
        Object *o = *cur;
        if (o->marked) {
            o->marked = 0;
            cur = &o->next;
        }
        else {
            *cur = o->next;
            if (o->children) free(o->children);
            if (o->data) free(o->data);
            free(o);
        }
    }
}

size_t count_live_objects() {
    Object *cur = gc_objects;
    size_t live_objs = 0;
    while (cur) {
        live_objs += 1;
        cur = cur->next;
    }
    return live_objs;
}

void gc_collect() {
    gc_mark_all_roots();
    gc_sweep();
}

Object* make_rt_int(uint64_t value) {
    return object_new(8, K_INT, (void*) &value);
}

Object* make_rt_rec(void *src, uint64_t size) {
    return object_new(size, K_REC, src);
}

Object* make_rt_str(void *src, uint64_t size) {
    return object_new(size, K_STR, src);
}

void insert_object(Object* dest, Object *src, uint64_t index) {
    if (!dest || !src) {
        fprintf(stderr, "insert_object: dest and src cannot be null");
        exit(1);
    }
    if (!dest->children) {
        fprintf(stderr, "insert_object: object cannot contain any children");
        exit(1);
    }
    if (index >= dest->num_children) {
        fprintf(stderr, "insert_object: child index out of range\n");
        exit(1);
    }
    dest->children[index] = src;
}

void dbg_print_heap() {
    fprintf(stderr, "heap objects:\n");
    for (Object *p = gc_objects; p; p = p->next) {
        fprintf(stderr, "  obj %p type=%d marked=%llu children=%llu\n",
                (void*)p, (int)p->ob_type, p->marked, p->num_children);
    }
    fprintf(stderr, "roots=%zu\n", root_stack.count);
}
