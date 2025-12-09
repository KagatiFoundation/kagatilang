fn main() {
    // cc::Build::new()
    //     .file("csrc/gc/object.h")
    //     .file("csrc/gc/gc.c")
    //     .compile("kagruntime_c");
    println!("cargo:rustc-link-search=native=/tmp/kaglang");
    println!("cargo:rustc-link-lib=dylib=kag");
}