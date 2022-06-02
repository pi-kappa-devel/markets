## Changes in version 1.0.3

* Updated package vignette (PDF manuscript) to reflect the latest functionality.
* Corrected one of the vignettes that was outdated. No code changes.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 55
Check time in seconds: 277
Status: 1 NOTE
R Under development (unstable) (2022-06-01 r82439 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
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
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Results: https://win-builder.r-project.org/h88brEjOv98e

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 149
Check time in seconds: 579
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/houses.Rd
          man/markets.Rd
          man/minus_log_likelihood.Rd
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/2526311
    From: README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Results: https://win-builder.r-project.org/9nzkESTq6Mdz

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 55
Check time in seconds: 281
Status: 1 NOTE
R version 4.2.0 (2022-04-22 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
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
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Results: https://win-builder.r-project.org/kdYGfYLpSIL9

### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.0.3.tar.gz-479233a528954ac09eba1f1fa8953a99
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 52.3 seconds ago
Build time:	9 minutes 50.8 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-479233a528954ac09eba1f1fa8953a99

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	markets_1.0.3.tar.gz-afa7178e6b334b73800a3643800a2ff3
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	29 minutes 7.6 seconds ago
Build time:	29 minutes 5.9 seconds

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb
	
See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-afa7178e6b334b73800a3643800a2ff3


### (6) rhub debian-gcc-devel -- OK
Build ID:	markets_1.0.3.tar.gz-12ae883871b845f09b0dc48c9f9dded3
Platform:	Debian Linux, R-devel, GCC
Submitted:	33 minutes 12.9 seconds ago
Build time:	33 minutes 10.8 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-12ae883871b845f09b0dc48c9f9dded3

### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	markets_1.0.3.tar.gz-4d74dcf5a69c4bfbb55e6e6e4a8f850e
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	33 minutes 17.2 seconds ago
Build time:	33 minutes 15.2 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-4d74dcf5a69c4bfbb55e6e6e4a8f850e

### (8) rhub rocker-gcc-san -- PREPERROR
Build ID:	markets_1.0.3.tar.gz-c0f9613250bd447099d849a5462541b0
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	47 minutes 11.1 seconds ago
Build time:	47 minutes 8.3 seconds

Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
  
See the full build log: https://builder.r-hub.io/status/original/markets_1.0.3.tar.gz-c0f9613250bd447099d849a5462541b0

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ─────────────────────────────────────────────────────────────── markets 1.0.3 ────
Duration: 2m 8.8s

❯ checking installed package size ... NOTE
    installed size is  6.3Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	markets_1.0.3.tar.gz-d624a6ae7d65485cbdf8a751afc0a123
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	37 minutes 23.7 seconds ago
Build time:	37 minutes 21.3 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-d624a6ae7d65485cbdf8a751afc0a123

### (11) rhub fedora-clang-devel -- OK
Build ID:	markets_1.0.3.tar.gz-5fa8687a404e4d5e9564608a490042b2
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	29 minutes 57.9 seconds ago
Build time:	29 minutes 56.5 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-5fa8687a404e4d5e9564608a490042b2

## Solaris
### (12) rhub solaris-x86-patched -- PREPERROR
Build ID:	markets_1.0.3.tar.gz-c46ff21736c548909d88af0cc72654d0
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	30 minutes 56 seconds ago
Build time:	30 minutes 54.8 seconds

Error: Failed to install 'markets' from local:
  Failed to `R CMD build` package, try `build = FALSE`.
In addition: Warning messages:
1: In i.p(...) : installation of package ‘ps’ had non-zero exit status
2: In i.p(...) :
  installation of package ‘processx’ had non-zero exit status
3: In i.p(...) : installation of package ‘callr’ had non-zero exit status
4: In i.p(...) :
  installation of package ‘testthat’ had non-zero exit status
5: In i.p(...) : installation of package ‘nloptr’ had non-zero exit status
6: In i.p(...) : installation of package ‘lme4’ had non-zero exit status
7: In i.p(...) :
  installation of package ‘pbkrtest’ had non-zero exit status
8: In i.p(...) : installation of package ‘car’ had non-zero exit status
9: In i.p(...) :
  installation of package ‘systemfit’ had non-zero exit status
Execution halted

See the full build log: https://builder.r-hub.io/status/original/markets_1.0.3.tar.gz-c46ff21736c548909d88af0cc72654d0

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.0.3.tar.gz-64e73ea4c7c6454c8f06004ddb52fd84
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 1.4 seconds ago
Build time:	8 minutes 0.3 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-64e73ea4c7c6454c8f06004ddb52fd84

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.0.3.tar.gz-faf7807130964f50889d065682358b00
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	8 minutes 2.8 seconds ago
Build time:	7 minutes 59.8 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.3.tar.gz-faf7807130964f50889d065682358b00/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    libs   3.8Mb
	
Results: https://mac.r-project.org/macbuilder/results/1654201965-10de135b8de48fc1/
