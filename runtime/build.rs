fn main() {
    println!("cargo:rustc-link-search=native=/tmp/kaglang"); // Windows platform not handled
    println!("cargo:rustc-link-lib=dylib=kag");
}