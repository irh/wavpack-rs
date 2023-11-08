use std::env;
use std::path::PathBuf;

fn main() {
    let wavpack_lib_dir = cmake::Config::new("WavPack")
        .define("BUILD_TESTING", "OFF")
        .define("WAVPACK_BUILD_COOLEDIT_PLUGIN", "OFF")
        .define("WAVPACK_BUILD_DOCS", "OFF")
        .define("WAVPACK_BUILD_PROGRAMS", "OFF")
        .define("WAVPACK_BUILD_WINAMP_PLUGIN", "OFF")
        .build();
    let wavpack_dir = wavpack_lib_dir.display();

    println!("cargo:rustc-link-search=native={wavpack_dir}/lib");
    println!("cargo:rustc-link-lib=wavpack");
    println!("cargo:rerun-if-changed=wrapper.h");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_arg(format!("-I{wavpack_dir}/include"))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("Unable to generate bindings");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
