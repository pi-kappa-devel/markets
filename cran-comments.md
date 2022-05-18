# Name Change Request 

This commit exclusively concerns a name change request. Although it is more than understandable that name changes are not desirable, several reasons could justify such a request on this occasion.

The package is named diseq, because the code that led to its conception focused exclusively on estimating two disequilibrium market models. The original code was extended into a package to improve collaboration with colleagues using the code. The name diseq was appropriate for the initial package's scope. 

Despite subsequent extensions and improvements added two more disequilibrium and an equilibrium model in the package, the name did not change to avoid breaking existing code. Nevertheless, the package moved more and more away from focusing on disequilibrium models and shortage estimation to providing a wider range of market model estimates, such as demanded, supplied quantities, and aggregated market quantities. Currently, the package offers various options for estimating markets irrespective of if they are in equilibrium or disequilibrium states. Moreover, it provides a comprehensive set of overloaded standard R model functions such as logLik, plot, and summary.

To the best of my knowledge, there is no package focusing on market model estimation in CRAN, and markets can fill this gap. In particular, its new Formula-based interface can fit well with other packages on CRAN. Further, the package's current interface (and any future expansions) does not focus on disequilibrium models but rather on markets in general.

With such functionality provided, the name diseq is not indicative anymore of the package's content. Moreover, if someone searches for market estimation in R, it is more likely to search for markets instead of diseq. So, the name change can significantly improve the chances of the package being found by its intended audience.

Hopefully, these arguments justify the inconvenience of the requested name change.

## Changes compared to diseq version 0.4.3
* Changed Name in DESCRIPTION file
* Adjusted gitignore and Rbuildignore
* Adjusted documentation, vignettes, CITATION, and NEWS.md
* Adjusted configure.ac and _pakgdown.yml
* Renamed diseq.R to markets.R

## Changes compared to markets version 1.0.0 (Thanks for the comments)
* Corrected date in DESCRIPTION.

## Changes compared to markets version 1.0.1 (Thanks for the comments)
* Documented `market_fit` object.
* Documented return values of `plot`, `show`, and `summaries`.
* Updated documentation website.


# Test Environments and Results (version 1.0.1)
## Rhub checks freeze for the last two days.

## Linux, GCC
### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ─────────────────────────────────────────────────────────────────── markets 1.0.2 ────
Duration: 2m 12s

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖
## Macos
### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 Note
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

Results: https://mac.r-project.org/macbuilder/results/1652871080-6c182ae52049aaae/


# Test Environments and Results (version 1.0.1)
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 69
Check time in seconds: 273
Status: 1 NOTE
R Under development (unstable) (2022-05-12 r82348 ucrt)

Results: https://win-builder.r-project.org/hUFD3AbSbYMP/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 150
Check time in seconds: 574
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

Results: https://win-builder.r-project.org/H7aFUnNUKEPr/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 57
Check time in seconds: 279
Status: 1 NOTE
R version 4.2.0 (2022-04-22 ucrt)

Results: https://win-builder.r-project.org/UScvE2r58un2/


### (4) rhub windows-x86_64-devel -- OK
Build ID:	markets_1.0.0.tar.gz-9e380be640184f8dbedbaed831c6177f
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 55.7 seconds ago
Build time:	9 minutes 52 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-9e380be640184f8dbedbaed831c6177f

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-a74c3b2118144f6f99d1dbcc3e94b659
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	11.4 seconds ago
Build time:	4.5 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-a74c3b2118144f6f99d1dbcc3e94b659


### (6) rhub debian-gcc-devel -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-9619b8ed4ce94f1d8ad3c7b3b7cca972
Platform:	Debian Linux, R-devel, GCC
Submitted:	14.7 seconds ago
Build time:	3.5 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-9619b8ed4ce94f1d8ad3c7b3b7cca972

### (7) rhub debian-gcc-devel-nold -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-617d10545fe445189bc63de931e62193
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	30.7 seconds ago
Build time:	3.4 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-617d10545fe445189bc63de931e62193

### (8) rhub rocker-gcc-san -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-cf2ccb8a4a394bf59096fc29c3d11e22
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	13.5 seconds ago
Build time:	4.1 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-cf2ccb8a4a394bf59096fc29c3d11e22

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ──────────────────────────────────────────────────────────────────────────── markets 1.0.2 ────
Duration: 2m 12s

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-b3c7168bc9f94af48ffaab274e4823fd
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	29.8 seconds ago
Build time:	3.4 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-b3c7168bc9f94af48ffaab274e4823fd

### (11) rhub fedora-clang-devel -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-fbeaafbafdb344d7bab80a80d8fd9d15
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	22 seconds ago
Build time:	3.5 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-fbeaafbafdb344d7bab80a80d8fd9d15


## Solaris
### (12) rhub solaris-x86-patched -- PREPERROR
Build ID:	markets_1.0.0.tar.gz-9925c698568244828c192282cc6b30c1
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	31 minutes 53.8 seconds ago
Build time:	31 minutes 29.7 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-9925c698568244828c192282cc6b30c1

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.0.0.tar.gz-2144ca18eb254d46a4465d69ddf948f4
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	10 minutes 14.2 seconds ago
Build time:	9 minutes 53.2 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-2144ca18eb254d46a4465d69ddf948f4

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.0.0.tar.gz-1933cff095204cf0b857cd0bfaf14442
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	9 minutes 40.8 seconds ago
Build time:	9 minutes 24.4 seconds


See the full build log: https://builder.r-hub.io/status/original/markets_1.0.0.tar.gz-1933cff095204cf0b857cd0bfaf14442

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 Note
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

Results: https://mac.r-project.org/macbuilder/results/1652473853-07eb9562409912bf/
