## Changes in version 1.1.4

In response to email from Thu 12/22/2022 1:23 PM:
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

* Separated pre-processor and compiler flags in auto-conf script. The only compiler flat was `-std=c++17` (required for the `par_unseq` execution policy) whenever available and this was moved to `CXXFLAGS`.
* Upgraded `SystemRequirements` in the `DESCRIPTION` file to `c++17` breaks completely the installation in Solaris systems. The error is: "Error: C++17 standard requested but CXX17 is not defined". The previous value of `SystemRequirements` set to `c++11` was to ensure compilation in all systems (besides solaris, I think also macOS was failing with `c++17` two years ago if I recall correctly).
* Explicitly configuring using `R CMD config CXX17` in auto-conf script.


# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- OK
Build ID:	markets_1.1.4.tar.gz-e753c9bb069c4e44a1486a6779ed688f
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	6 minutes 47.9 seconds ago
Build time:	6 minutes 46.7 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-e753c9bb069c4e44a1486a6779ed688f/


### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 138
Check time in seconds: 484
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (19:5)
  Maddala (17:5)

Log Files: https://win-builder.r-project.org/iofQdWSf5Gq7/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 120
Check time in seconds: 454
Status: 1 NOTE
R version 4.2.2 (2022-10-31 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Days since last update: 2

Log Files: https://win-builder.r-project.org/m8it14mE4c1w/

### (4) rhub windows-x86_64-devel -- 1 NOTE
Installation time in seconds: 99
Check time in seconds: 416
Status: 1 NOTE
R Under development (unstable) (2022-12-23 r83498 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel[at]gmail[dot]com>'

Days since last update: 2

Log Files: https://win-builder.r-project.org/U7mvrjjBMZ5l/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.1.4.tar.gz-1c994105bbf94f99937ccb73d17234ee
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	1 hour 9 minutes 16.4 seconds ago
Build time:	1 hour 9 minutes 12 seconds

NOTES:
* checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    libs   3.7Mb
See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-1c994105bbf94f99937ccb73d17234ee/

### (6) rhub debian-gcc-devel -- OK
Build ID:	markets_1.1.4.tar.gz-151108beb09547d39eef7a722c7f6302
Platform:	Debian Linux, R-devel, GCC
Submitted:	1 hour 21 minutes 42.6 seconds ago
Build time:	1 hour 21 minutes 39.8 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-151108beb09547d39eef7a722c7f6302/


### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	markets_1.1.4.tar.gz-5b3c7302c8e444749bfcd653c262ea12
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	1 hour 21 minutes 30.8 seconds ago
Build time:	1 hour 21 minutes 27.8 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-5b3c7302c8e444749bfcd653c262ea12/


### (8) rhub rocker-gcc-san -- OK
Build ID:	markets_1.1.4.tar.gz-7dc1a5015cec4355a91bf2c31e555e6c
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	2 hours 27 minutes 26.7 seconds ago
Build time:	2 hours 27 minutes 22.3 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.1.4.tar.gz-7dc1a5015cec4355a91bf2c31e555e6c

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ────────────────────────────────────────────────────────────────────────── markets 1.1.4 ────
Duration: 1m 39.8s

❯ checking installed package size ... NOTE
    installed size is  6.3Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	markets_1.1.4.tar.gz-c51e8ab4d8874b59abd407a3398c5502
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	1 hour 25 minutes 16.2 seconds ago
Build time:	1 hour 25 minutes 11.9 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-c51e8ab4d8874b59abd407a3398c5502/

### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.1.4.tar.gz-04d0cd2228f844e2bbece9293a066549
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	1 hour 11 minutes 40.7 seconds ago
Build time:	1 hour 11 minutes 37.2 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-04d0cd2228f844e2bbece9293a066549/

## Solaris
### (12) rhub solaris-x86-patched -- PREERROR
Build ID:	markets_1.1.4.tar.gz-d7b173fd01b541e6bb6c956980092e8b
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	56 minutes 43 seconds ago
Build time:	56 minutes 42.3 seconds

** libs
Error: C++17 standard requested but CXX17 is not defined
* removing ‘/export/home/XbvtT6M/Rtemp/RtmptEPeZN/Rinst5fab752f51f9/markets’
      -----------------------------------
ERROR: package installation failed

See the full build log: https://builder.r-hub.io/status/original/markets_1.1.4.tar.gz-d7b173fd01b541e6bb6c956980092e8b

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.1.4.tar.gz-1a8ca8d02eb441d0aaef2c842dc0c325
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 26.5 seconds ago
Build time:	8 minutes 23.5 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-1a8ca8d02eb441d0aaef2c842dc0c325/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.1.4.tar.gz-a67db3af521d489daf6ffff1d2060031
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	10 minutes 19.1 seconds ago
Build time:	10 minutes 17 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.4.tar.gz-a67db3af521d489daf6ffff1d2060031/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    libs   3.9Mb

Logs: https://mac.r-project.org/macbuilder/results/1671882738-9920f92509cd4e6c/
