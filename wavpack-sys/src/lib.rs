#![doc = include_str!("../README.md")]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    deref_nullptr,
    clippy::redundant_static_lifetimes
)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
