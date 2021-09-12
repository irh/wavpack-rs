use std::env;
use std::path::PathBuf;

fn main() {
    let dst = cmake::build("WavPack");
    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=wavpack");
    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .generate()
        .expect("Unable to genarate bindings");
    let out_path = env::var("OUT_DIR").unwrap();
    let out_path = PathBuf::from(out_path);
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
