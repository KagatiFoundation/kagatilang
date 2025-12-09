#include "../csrc/gc/gc.h"
#include "../csrc/io/print.h"
#include <stdio.h>
#include <assert.h>

void println(Object* a) {
    print(a);
    Object* b = make_rt_str("\n", 2);
    print(b);
}

int main() {
    Object* a1 = make_rt_str("hello world", 12);
    println(a1);
    print(a1);
    Object* a2 = make_rt_str("this is cool", 13);
    println(a2);
    Object* a3 = make_rt_str("okay but not this", 18);
    print(a3);
    Object* a4 = make_rt_str("ramesh poudel", 14);
    print(a4);
}