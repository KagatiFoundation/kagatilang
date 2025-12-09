#include "gc/gc.h"
#include "io/print.h"
#include <stdio.h>
#include <assert.h>

int main() {
    init_gc();
    Object* value = make_rt_int(12345);
    print(value);
    Object* v = make_rt_str("hello world", 12);
    print(v);
    dbg_print_heap();

    assert(48 == __offsetof(Object, data));
    gc_collect();
}