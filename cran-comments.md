## Resubmission
This is the resubmission of a new version. In this version I have:

* Fixed the URLs for travis-ci and package (canonical form) in the README.
* Avoided PDF manual due to issues with r-devel-windows-x86_64-gcc10-UCRT.
* Added routine to check Internet resource and fail gracefully.
* Included the package citation.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder x86_64-w64-mingw32 (64-bit) (r-devel)
* x86_64-w64-mingw32/x64 (64-bit) (on appveyor), R 4.1.0

## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
