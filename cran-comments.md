## Changes in version 1.1.4

Sorry for the short time window during the vacation period since the last update. I wouldn't want the package to be archived. This update is in response to the email from Thu 12/22/2022 1:23 PM:
"""
This is thoroughly muddled!  On Linux

SystemRequirements: 	C++11

checking whether we are using the GNU C++ compiler... yes checking whether g++ -std=gnu++14 accepts -g... yes

     cppflags:  -D_MARKETS_HAS_GSL_ -I/usr/include -D_MARKETS_HAS_EXECUTION_POLICIES_  -std=c++17

g++ -std=gnu++11 -I"/data/gannet/ripley/R/MKL/include" -DNDEBUG
-D_MARKETS_HAS_GSL_ -I/usr/include -D_MARKETS_HAS_EXECUTION_POLICIES_
-std=c++17 ...

So the DESCRIPTION specifies C++11, the configure script does not use it and by misuse of cppflags (this has nothing to do with cpp) compilation uses C++17.

On macOS, C++14 is used to configure and C++11 to compile.

Please correct before 2023-01-09 to safely retain your package on CRAN.
"""

* No functionality changes. Only changes in the autoconf script:
** Separated pre-processor flags to PKG_CPPFLAGS.
** Removed compiler flags. Only the compiler flag `-std=c++17` (for the `par_unseq` execution policy) was previously used from in some systems. 
** Instead, setting CXX_STD to CXX17 if the C++17 configuration is available (R CMD config CXX17FLAGS).
** Removed `SystemRequirements` from  the `DESCRIPTION` file. 
** I have additionally tried to set `SystemRequirements` to `c++17` (attempt 2). 
** Unfortunately, both the current configuration and the configuration of attempt 2 completely breaks the installation in Solaris systems (the error is "Error: C++17 standard requested but CXX17 is not defined"). In addition, attempt 2 it fails the debian-clang incoming checks of `win-builder.r-project`,  while linking with `tbb` succeeds in `rhubs`'s `debian-clang-devel`
** Setting `SystemRequirements` to an old standard, e.g., C++11, and enabling `c++17` in the `configure.ac` is the only way that I could make the installation work in all the platforms (for the last two years). 


# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 100
Check time in seconds: 422
Status: 1 NOTE
R Under development (unstable) (2022-12-24 r83500 ucrt)

* checking CRAN incoming feasibility ... [16s] NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Days since last update: 3

Log Files: https://win-builder.r-project.org/Zag6BeLI5R26/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 74
Check time in seconds: 313
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (19:5)
  Maddala (17:5)

Log Files: https://win-builder.r-project.org/2cstjHC7qhUy/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 99
Check time in seconds: 432
Status: 1 NOTE
R version 4.2.2 (2022-10-31 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Days since last update: 3

Log Files: https://win-builder.r-project.org/31pHk6FYO27e/

### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.1.4.tar.gz-de182b7e0e0f4ae8aef8f18b995d5d75
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	10 minutes 14 seconds ago
Build time:	10 minutes 4.6 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-de182b7e0e0f4ae8aef8f18b995d5d75/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.1.4.tar.gz-5fed09df7700458793ae759c06f5c2fc
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	1 hour 4 minutes 38.5 seconds ago
Build time:	1 hour 3 minutes 52.4 seconds

NOTES:
* checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    libs   3.7Mb

See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-5fed09df7700458793ae759c06f5c2fc/

### (6) rhub debian-gcc-devel -- OK
Build ID:	markets_1.1.4.tar.gz-4d52a62e83594d569c0fc9f8006d9e55
Platform:	Debian Linux, R-devel, GCC
Submitted:	1 hour 16 minutes 52.2 seconds ago
Build time:	1 hour 16 minutes 40.5 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-4d52a62e83594d569c0fc9f8006d9e55/

### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	markets_1.1.4.tar.gz-3cf6732c936f4cd3bd84956f87deda06
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	1 hour 16 minutes 56.6 seconds ago
Build time:	1 hour 16 minutes 33.9 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-3cf6732c936f4cd3bd84956f87deda06/

### (8) rhub rocker-gcc-san -- OK
Build ID:	markets_1.1.4.tar.gz-0c41cedb9ba9497396caaebb3d66a034
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	2 hours 26 minutes 29.2 seconds ago
Build time:	2 hours 26 minutes 1.9 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.1.4.tar.gz-0c41cedb9ba9497396caaebb3d66a034

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ────────────────────────────────────────── markets 1.1.4 ────
Duration: 1m 40.8s

❯ checking installed package size ... NOTE
    installed size is  6.3Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	markets_1.1.4.tar.gz-dd4a86373a6f495284124972076ee47c
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	1 hour 20 minutes 49.4 seconds ago
Build time:	1 hour 20 minutes 15.7 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-dd4a86373a6f495284124972076ee47c/

### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.1.4.tar.gz-0dd6b994628141baaa1d705e33406631
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	1 hour 7 minutes 5.5 seconds ago
Build time:	1 hour 6 minutes 26.8 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-0dd6b994628141baaa1d705e33406631/

## Solaris
### (12) rhub solaris-x86-patched -- PREERROR
** libs
Error: C++17 standard requested but CXX17 is not defined
* removing ‘/export/home/XOoY23s/Rtemp/RtmpAk4IWI/Rinst71643953286/markets’
      -----------------------------------
ERROR: package installation failed

Log Files: https://builder.r-hub.io/status/original/markets_1.1.4.tar.gz-4ad464aa95014cfeb1bffb4a05512e21

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.1.4.tar.gz-69485e5826ff4211a246a3de904bcc6b
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 50.2 seconds ago
Build time:	8 minutes 33.1 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-69485e5826ff4211a246a3de904bcc6b/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.1.4.tar.gz-cd35538be1924227ad3bf0059a124d32
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	11 minutes 13.3 seconds ago
Build time:	10 minutes 43.6 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-cd35538be1924227ad3bf0059a124d32/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    libs   3.9Mb

Log Files: https://mac.r-project.org/macbuilder/results/1671991833-61406d910219ce75/
