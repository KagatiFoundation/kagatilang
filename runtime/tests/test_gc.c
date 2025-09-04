#include "gc/gc.h"
#include <stdio.h>
#include <assert.h>

int main() {
    printf("=== Starting GC tests ===\n");

    printf("[1] Testing kgc_alloc...\n");
    gc_object_t* obj = kgc_alloc(16);
    assert(obj != NULL);
    assert(obj->ref_count == 1);
    assert(obj->size == 16);
    assert(obj->num_children == 0);
    assert(obj->data != NULL);
    printf("  kgc_alloc passed.\n");

    kgc_release(obj);
    printf("  kgc_release passed.\n");

    printf("[2] Testing kgc_retain & kgc_release...\n");
    obj = kgc_alloc(8);
    assert(obj->ref_count == 1);

    kgc_retain(obj);
    assert(obj->ref_count == 2);
    printf("  kgc_retain passed.\n");

    kgc_release(obj);
    assert(obj->ref_count == 1);
    printf("  first kgc_release passed.\n");

    kgc_release(obj);  // obj freed
    printf("  second kgc_release (object freed) passed.\n");

    printf("[3] Testing kgc_add_child...\n");
    gc_object_t* parent = kgc_alloc(8);
    gc_object_t* child1 = kgc_alloc(4);
    gc_object_t* child2 = kgc_alloc(4);

    kgc_add_child(parent, child1);
    kgc_add_child(parent, child2);

    assert(parent->num_children == 2);
    assert(parent->children[0] == child1);
    assert(parent->children[1] == child2);
    assert(child1->ref_count == 2);  // retained by parent
    assert(child2->ref_count == 2);
    printf("  kgc_add_child passed.\n");

    kgc_release(parent);  // should decrease children ref counts
    assert(child1->ref_count == 1);
    assert(child2->ref_count == 1);
    printf("  releasing parent updates children passed.\n");

    kgc_release(child1);
    kgc_release(child2);
    printf("  children released manually passed.\n");

    printf("[4] Testing zero-size allocation...\n");
    gc_object_t* zero_obj = kgc_alloc(0);
    assert(zero_obj == NULL);
    printf("  zero-size allocation correctly returned NULL.\n");

    printf("[5] Running stress test with multiple objects...\n");
    for (int i = 0; i < 1000; i++) {
        gc_object_t* p = kgc_alloc(16);
        for (int j = 0; j < 10; j++) {
            gc_object_t* c = kgc_alloc(8);
            kgc_add_child(p, c);
            kgc_release(c);  // should stay alive because parent retained
        }
        kgc_release(p); // should free parent and all children
    }
    printf("  Stress test passed.\n");

    printf("=== All GC tests passed! ===\n");
    return 0;
}