# wavpack-rs

Rust bindings for [WavPack](https://www.wavpack.com/). 

The included WavPack version is [5.6.0](https://www.wavpack.com/changelog.txt).

## Current Status 

Reading and writing support is implemented and appears to work correctly,
although is under-tested.

Not all APIs are exposed, but please feel encouraged to open PRs for anything 
that you need that's missing. APIs that have been exposed are thin wrappers for
calls to the underlying WavPack functions, for usage information see the 
[WavPack API documentation](https://www.wavpack.com/WavPack5LibraryDoc.pdf).

## Dependencies

* [CMake](https://cmake.org) is used to build WavPack.  
* A C++ compiler.

## Acknowledgments

Many thanks to [Toru3](https://toru3.gitlab.io) who started this project.
