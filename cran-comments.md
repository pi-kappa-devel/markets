## Changes in version 1.1.2

* Corrected bug of ignoring custom title, or axis labels in market fit plots.
* Switched to `ginv` from `MASS` for the variance-covariance matrix calculation in `2SLS` estimations.
* Added previously unhanded corner cases when using splines in `2SLS` estimations.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- OK
Installation time in seconds: 99
Check time in seconds: 426
Status: OK
R Under development (unstable) (2022-09-06 r82818 ucrt)

log: https://win-builder.r-project.org/02Svje0got6g/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 72
Check time in seconds: 297
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

log: https://win-builder.r-project.org/EE9Lp0jni0lm/

### (3) R-release win-builder.r-project.org -- OK
Installation time in seconds: 102
Check time in seconds: 429
Status: 1 NOTE
R version 4.2.1 (2022-06-23 ucrt)

log: https://win-builder.r-project.org/V5L2FQ276b0F/

### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.1.2.tar.gz-2056ba15a3a04fa49ea2318434811bd5
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	6 minutes 6.1 seconds ago
Build time:	6 minutes 2.6 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-2056ba15a3a04fa49ea2318434811bd5/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.1.2.tar.gz-4cc0bbb1e6fd4502ab07c99f6c0615b7
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	52 minutes 14.1 seconds ago
Build time:	52 minutes 11.1 seconds

NOTES:
* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   3.5Mb
See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-4cc0bbb1e6fd4502ab07c99f6c0615b7/

### (6) rhub debian-gcc-devel -- PREPERROR
Build ID:	markets_1.1.2.tar.gz-811d8ce859d14b9e8715fb88c58dc812
Platform:	Debian Linux, R-devel, GCC
Submitted:	42 minutes 23.4 seconds ago
Build time:	42 minutes 20.1 seconds

Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
ERROR: dependency ‘RcppGSL’ is not available for package ‘markets’
* removing ‘/home/docker/R/markets’
Warning messages:
1: In i.p(...) :
  installation of package ‘RcppGSL’ had non-zero exit status
2: In i.p(...) :
  installation of package ‘/tmp/RtmpCZ6YAC/file1314539a99c/markets_1.1.2.tar.gz’ had non-zero exit status
> 
> 
Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install

See the full build log: https://builder.r-hub.io/status/original/markets_1.1.2.tar.gz-811d8ce859d14b9e8715fb88c58dc812

### (7) rhub debian-gcc-devel-nold -- PREPERROR
Build ID:	markets_1.1.2.tar.gz-6c76a9e893694bda8a5e96e56d7c20bd
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	42 minutes 20.4 seconds ago
Build time:	42 minutes 16.2 seconds

Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
ERROR: dependency ‘RcppGSL’ is not available for package ‘markets’
* removing ‘/home/docker/R/markets’
Warning messages:
1: In i.p(...) :
  installation of package ‘RcppGSL’ had non-zero exit status
2: In i.p(...) :
  installation of package ‘/tmp/Rtmpn7IyeZ/file13222da2e9b/markets_1.1.2.tar.gz’ had non-zero exit status
> 
> 
Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install

See the full build log: https://builder.r-hub.io/status/original/markets_1.1.2.tar.gz-6c76a9e893694bda8a5e96e56d7c20bd

### (8) rhub rocker-gcc-san -- OK
Build ID:	markets_1.1.2.tar.gz-0f1eceb7c10b4685a74983effecd8e1c
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	1 hour 36 minutes 18.8 seconds ago
Build time:	1 hour 35 minutes 57.3 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-0f1eceb7c10b4685a74983effecd8e1c/

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ────────────────────────────────────────────────────────────── markets 1.1.2 ────
Duration: 1m 44.4s

❯ checking installed package size ... NOTE
    installed size is  6.2Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- PREPERROR
Build ID:	markets_1.1.2.tar.gz-702691ed887c4987a4cb4c0bad958969
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	45 minutes 19.7 seconds ago
Build time:	45 minutes 15.7 seconds

Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
ERROR: dependency 'RcppGSL' is not available for package 'markets'
* removing '/home/docker/R/markets'
Warning messages:
1: In i.p(...) :
  installation of package 'RcppGSL' had non-zero exit status
2: In i.p(...) :
  installation of package '/tmp/RtmptKVnqB/file1324acba9ed/markets_1.1.2.tar.gz' had non-zero exit status
> 
> 
Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install


See the full build log: https://builder.r-hub.io/status/original/markets_1.1.2.tar.gz-702691ed887c4987a4cb4c0bad958969


### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.1.2.tar.gz-de83dc4176f74067ac47408533d65ffd
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	53 minutes 22.1 seconds ago
Build time:	53 minutes 18.7 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-de83dc4176f74067ac47408533d65ffd/

## Solaris
### (12) rhub solaris-x86-patched -- 1 ERROR
Build ID:	markets_1.1.2.tar.gz-718850f5c2bc4b19aa876ee4bc700823
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	39 minutes 13.6 seconds ago
Build time:	39 minutes 9.3 seconds

ERRORS:
* checking package dependencies ... ERROR
Package suggested but not available: ‘testthat’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-718850f5c2bc4b19aa876ee4bc700823/


## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.1.2.tar.gz-0e7d2869221d4dd784cf3f90d4c70afb
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 23.9 seconds ago
Build time:	8 minutes 19.9 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-0e7d2869221d4dd784cf3f90d4c70afb/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.1.2.tar.gz-3cad50690a734ec885bc96f32946516d
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	7 minutes 52.4 seconds ago
Build time:	7 minutes 50.9 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-3cad50690a734ec885bc96f32946516d/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    libs   3.8Mb
	
log: https://mac.r-project.org/macbuilder/results/1662571801-ac26656d892491a7/

### (16) rhub macos-m1-bigsur-release  -- 1 WARNING
Build ID:	markets_1.1.2.tar.gz-64c8f3467b12499c8de9adfeaeebc5f9
Platform:	Apple Silicon (M1), macOS 11.6 Big Sur, R-release
Submitted:	3 minutes 2.2 seconds ago
Build time:	2 minutes 58.3 seconds

WARNINGS:
* checking re-building of vignette outputs ... WARNING
Error(s) in re-building vignettes:
  ...
--- re-building ‘basic_usage.Rmd’ using rmarkdown
Quitting from lines 186-218 (basic_usage.Rmd) 
Error: processing vignette 'basic_usage.Rmd' failed with diagnostics:
polygon edge not found
--- failed re-building ‘basic_usage.Rmd’

--- re-building ‘market_clearing_assessment.Rmd’ using rmarkdown
--- finished re-building ‘market_clearing_assessment.Rmd’

--- re-building ‘model_details.Rmd’ using rmarkdown
--- finished re-building ‘model_details.Rmd’

--- re-building ‘package.Rmd’ using rmarkdown
--- finished re-building ‘package.Rmd’

SUMMARY: processing the following file failed:
  ‘basic_usage.Rmd’

Error: Vignette re-building failed.
Execution halted

See the full build log: https://artifacts.r-hub.io/markets_1.1.2.tar.gz-64c8f3467b12499c8de9adfeaeebc5f9/
