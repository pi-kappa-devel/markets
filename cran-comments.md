## Changes in version 1.1.6

* Addressing documentation issues before 2025-08-25:
  - Removed relative links to README file from `markets.R` documentation.
  - Removed internal keyword from overview page.
* Further documentation improvements:
  - Added alt text for vignette figures and images.
* Unit test changes:
  - Patched summary output tests for setups with customized output width.
* Continuous integration Changes
  - Replaced custom github workflows with rhub-provided workflows.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 41
Check time in seconds: 205
Status: 1 NOTE
R Under development (unstable) (2024-02-15 r85925 ucrt)

* checking CRAN incoming feasibility ... [10s] NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.18637/jss.v108.i02
    From: README.md
    Status: 404
    Message: Not Found

Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v108.i02
    From: DESCRIPTION
          inst/CITATION
    Status: 404
    Message: Not Found

Log Files: https://win-builder.r-project.org/lMc0AuUbL188/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 47
Check time in seconds: 234
Status: 1 NOTE
R version 4.2.3 (2023-03-15 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1080/01621459.1978.10480085
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.18637/jss.v108.i02
    From: README.md
    Status: 404
    Message: Not Found
  URL: https://doi.org/10.2307/1913181
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/2526311
    From: README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v108.i02
    From: DESCRIPTION
          inst/CITATION
    Status: Not Found
    Message: 404
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Log Files: https://win-builder.r-project.org/lo3d4rwDBbcL/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 46
Check time in seconds: 231
Status: 1 NOTE
R version 4.3.2 (2023-10-31 ucrt)

* checking CRAN incoming feasibility ... [11s] NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.18637/jss.v108.i02
    From: README.md
    Status: 404
    Message: Not Found

Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v108.i02
    From: DESCRIPTION
          inst/CITATION
    Status: 404
    Message: Not Found

Log Files: https://win-builder.r-project.org/5HDb1Nrh61CW/

### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.1.5.tar.gz-6f50040fa9af42b3ab5c2faf644d1a8d
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	8 minutes 56.8 seconds ago
Build time:	8 minutes 49.5 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-6f50040fa9af42b3ab5c2faf644d1a8d/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.1.5.tar.gz-0e44f347eed546dfbc358c8072caccb9
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	1 hour 25 minutes 36.4 seconds ago
Build time:	1 hour 25 minutes 0.8 seconds

NOTES:
* checking installed package size ... NOTE
  installed size is  5.5Mb
  sub-directories of 1Mb or more:
    libs   3.6Mb
    

See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-0e44f347eed546dfbc358c8072caccb9/

### (6) rhub debian-gcc-devel -- OK
Build ID:	markets_1.1.5.tar.gz-1e9e8c193f8a4edf8f03c92ffad5b55e
Platform:	Debian Linux, R-devel, GCC
Submitted:	1 hour 39 minutes 23.5 seconds ago
Build time:	1 hour 39 minutes 10.7 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-1e9e8c193f8a4edf8f03c92ffad5b55e/markets.Rcheck/

### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	markets_1.1.5.tar.gz-2240e9823908467fbe78babdaa3fa8a7
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	1 hour 38 minutes 57 seconds ago
Build time:	1 hour 38 minutes 39.2 seconds
See the full build log: HTML, text, artifacts.


See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-2240e9823908467fbe78babdaa3fa8a7/

### (8) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- OK
── R CMD check results ───────────────────────────────── markets 1.1.5 ────
Duration: 1m 45.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

### (9) rhub ubuntu-rchk -- PREERROR
Build ID:	markets_1.1.5.tar.gz-cea5d554e60f493fb73ad2d8a501827e
Platform:	Ubuntu Linux 20.04.1 LTS, R-devel with rchk
Submitted:	1 minute 56.5 seconds ago
Build time:	1 minute 49.6 seconds

Log Files: https://builder.r-hub.io/status/original/markets_1.1.5.tar.gz-cea5d554e60f493fb73ad2d8a501827e

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	markets_1.1.5.tar.gz-c7b5db1f8d9b4c40a6addaefdee57e35
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	1 hour 48 minutes 28.3 seconds ago
Build time:	1 hour 48 minutes 4.6 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-c7b5db1f8d9b4c40a6addaefdee57e35/

### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.1.5.tar.gz-02acc5aeb5de4ab3bc9cde2a8ece21b8
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	1 hour 18 minutes 35 seconds ago
Build time:	1 hour 18 minutes 19 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.5.tar.gz-02acc5aeb5de4ab3bc9cde2a8ece21b8/


## Macos
### (12) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0

Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    libs   3.3Mb

Log Files: https://mac.r-project.org/macbuilder/results/1708119395-4571ea21616fc988/
