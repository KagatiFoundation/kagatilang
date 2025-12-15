#include "../csrc/gc/gc.h"
#include "../csrc/gc/object.h"

#include <stdio.h>

#define FORCE_GC() gc_collect()

static void assert_live(size_t expected) {
    size_t live = count_live_objects();
    if (live != expected) {
        fprintf(stderr, "Expected %zu live objects, got %zu\n",
                expected, live);
        dbg_print_heap();
        abort();
    }
}

void test_single_object() {
    init_gc();

    Object *o = make_rt_int(42);
    gc_push_root(&o);

    FORCE_GC();
    assert_live(1);

    gc_pop_roots(1);
    FORCE_GC();
    assert_live(0);
}

void test_parent_child() {
    init_gc();

    Object *child = make_rt_int(1);
    Object *children[] = { child };
    Object *parent = make_rt_rec(sizeof(children));

    gc_push_root(&parent);
    FORCE_GC();

    assert_live(2);

    gc_pop_roots(1);
    FORCE_GC();
    assert_live(0);
}

void test_cycle() {
    init_gc();

    Object *a = make_rt_rec(2 * MACH_PTR_SIZE);
    Object *b = make_rt_rec(2 * MACH_PTR_SIZE);

    assign_ref(a, b, 0);
    assign_ref(b, a, 0);

    gc_push_root(&a);
    FORCE_GC();
    assert_live(2);

    gc_pop_roots(1);
    FORCE_GC();
    assert_live(0);
}

int main() {
    test_cycle();
}
