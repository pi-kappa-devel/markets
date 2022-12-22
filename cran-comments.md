## Changes in version 1.1.3

* Corrected `houses` dataset documentation. The data range starts in January instead of July 1958.
* Switched from `@describein` to `@rdname` for documenting classes. This change was implemented to fix the continuous integration processes of the package (`R` checks and documentation deployment using `GitHub` actions). These processes failed with version 7.2.3 of `roxygen2`, after which `@describein` cannot be used to document multiple classes in a single documentation entry.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- OK
Installation time in seconds: 50
Check time in seconds: 236
Status: OK
R Under development (unstable) (2022-12-20 r83482 ucrt)

log: https://win-builder.r-project.org/T9MhKa9xAc75/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 122
Check time in seconds: 328
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]mail[dot]com>'

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (19:5)
  Maddala (17:5)
  
log: https://win-builder.r-project.org/uG4pYwL6RU9a/

### (3) R-release win-builder.r-project.org -- OK
Installation time in seconds: 50
Check time in seconds: 241
Status: OK
R version 4.2.2 (2022-10-31 ucrt)

log: https://win-builder.r-project.org/knx9hkq2AnlU/

### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.1.3.tar.gz-8bbdd98ebc91454f8d2543e11d5c9bfa
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	10 minutes 37.5 seconds ago
Build time:	10 minutes 12.3 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-8bbdd98ebc91454f8d2543e11d5c9bfa/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.1.3.tar.gz-07ceba1c72654f9d8f1b3d01797611b2
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	9 hours 20 minutes 34 seconds ago
Build time:	1 hour 30 minutes 17.2 seconds

NOTES:
* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   3.5Mb
See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-07ceba1c72654f9d8f1b3d01797611b2/

### (6) rhub debian-gcc-devel -- OK
Build ID:	markets_1.1.3.tar.gz-39968ac0e83d490bb355aad7a2c6c239
Platform:	Debian Linux, R-devel, GCC
Submitted:	10 hours 30 minutes 47.8 seconds ago
Build time:	1 hour 50 minutes 10.8 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-39968ac0e83d490bb355aad7a2c6c239/


### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	markets_1.1.3.tar.gz-954477e711214c7aaf2072f6d51dfaac
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	10 hours 28 minutes 18.8 seconds ago
Build time:	1 hour 49 minutes 52 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-954477e711214c7aaf2072f6d51dfaac/

### (8) rhub rocker-gcc-san -- OK

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ───────────────────────────────────────────────────────────────────── markets 1.1.3 ────
Duration: 1m 42.8s

❯ checking installed package size ... NOTE
    installed size is  6.3Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	markets_1.1.3.tar.gz-c664024c2ad148a1a232e4703bfd4ebd
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	9 hours 59 minutes 43.3 seconds ago
Build time:	1 hour 56 minutes 53.2 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-c664024c2ad148a1a232e4703bfd4ebd/

### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.1.3.tar.gz-d91b261d51ee4915addbd89160375d30
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	9 hours 13 minutes 6.2 seconds ago
Build time:	1 hour 34 minutes 21.7 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-d91b261d51ee4915addbd89160375d30/

## Solaris
### (12) rhub solaris-x86-patched -- 1 ERROR, 1 NOTE
ERRORS:
* checking tests ...
  Running ‘testthat.R’
 ERROR
Running the tests in ‘tests/testthat.R’ failed.
Complete output:
  > library(testthat)
  Error in library(testthat) : there is no package called 'testthat'
  Execution halted
NOTES:
* checking package dependencies ... NOTE
Package suggested but not available for checking: ‘testthat’
See the full build log: HTML, text, artifacts. 


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-f7a2faed5f0647acbbe6084ba6f658f9/

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.1.3.tar.gz-a8e2799c2533452e9b0befa3d79dce62
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	10 minutes 6.9 seconds ago
Build time:	9 minutes 27.3 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-a8e2799c2533452e9b0befa3d79dce62/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.1.3.tar.gz-e7a4b514320f4e8788df21a9271767a5
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	11 minutes 42.1 seconds ago
Build time:	10 minutes 59.4 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.3.tar.gz-e7a4b514320f4e8788df21a9271767a5/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    libs   3.8Mb
	
log: https://mac.r-project.org/macbuilder/results/1671661683-fa349fc513b815f9/

