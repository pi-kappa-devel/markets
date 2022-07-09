## Changes in version 1.1.0

** Functionality changes:
* Important (main reason for the short update window): Fixed calculation error in two stage least square correlation parameter (introduced in 1.0.6).
* Added significance stars in summaries.

* Changes affecting user space:
** Integrated functionality of `maximize_log_likelihood` to `estimate`. Equilibrium likelihoods can be optimized via `GSL` by passing the option `optimizer = gsl` to `estimate`.

* Documentation changes:
** Reorganized and extended documentation.
** Improved figure resolution in the package's paper.
** Removed non exported variable name access functions from documentation.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 99
Check time in seconds: 429
Status: 1 NOTE
R Under development (unstable) (2022-07-08 r82567 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 4

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
	
Results: https://win-builder.r-project.org/Z4nV7G6UPpb3/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 134
Check time in seconds: 512
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 4

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/houses.Rd
          man/markets.Rd
          man/model_likelihoods.Rd
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

Results: https://win-builder.r-project.org/uU16K3gP0ZAp/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 110
Check time in seconds: 468
Status: 1 NOTE
R version 4.2.1 (2022-06-23 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 4

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
		
Results: https://win-builder.r-project.org/3yw4NO4LuiD4/

### (4) rhub windows-x86_64-devel -- Unavailable
Platform was not available in rhub
## Linux, GCC
### (5) rhub ubuntu-gcc-release -- Unavailable
Platform was not available in rhub

### (6) rhub debian-gcc-devel -- Unavailable
Platform was not available in rhub

### (7) rhub debian-gcc-devel-nold -- Unavailable
Platform was not available in rhub

### (8) rhub rocker-gcc-san -- Unavailable
Platform was not available in rhub

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ──────────────────────────────────────────────────────────────────────── markets 1.1.0 ────
Duration: 1m 42.7s

❯ checking installed package size ... NOTE
    installed size is  6.2Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- Unavailable
Platform was not available in rhub

### (11) rhub fedora-clang-devel -- Unavailable
Platform was not available in rhub

## Solaris
### (12) rhub solaris-x86-patched -- 1 ERROR
Build ID:	markets_1.1.0.tar.gz-68c763833e0a4a3a8ecff95723705a66
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	39 minutes 28.4 seconds ago
Build time:	39 minutes 19.9 seconds

ERRORS:
* checking package dependencies ... ERROR
Package suggested but not available: ‘testthat’

The suggested packages are required for a complete check.
Checking can be attempted without them by setting the environment
variable _R_CHECK_FORCE_SUGGESTS_ to a false value.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.

See the full build log: https://artifacts.r-hub.io/markets_1.1.0.tar.gz-68c763833e0a4a3a8ecff95723705a66/

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.1.0.tar.gz-2778a254968642efb9c40cba646a6d04
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	9 minutes 25.4 seconds ago
Build time:	9 minutes 20.9 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.1.0.tar.gz-2778a254968642efb9c40cba646a6d04/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.1.0.tar.gz-4ee867e1d29e469b8b67ccde648f79fd
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	9 minutes 19.5 seconds ago
Build time:	9 minutes 4.4 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.1.0.tar.gz-4ee867e1d29e469b8b67ccde648f79fd/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    libs   3.8Mb
	
Results: https://mac.r-project.org/macbuilder/results/1657356134-f7de9250f32f3c72/

### (16) rhub macos-m1-bigsur-release  -- 1 WARNING
Build ID:	markets_1.1.0.tar.gz-b2dff573ad2b461c8ae70136c57f1d81
Platform:	Apple Silicon (M1), macOS 11.6 Big Sur, R-release
Submitted:	3 minutes 49.5 seconds ago
Build time:	3 minutes 35.8 seconds

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

See the full build log: https://artifacts.r-hub.io/markets_1.1.0.tar.gz-b2dff573ad2b461c8ae70136c57f1d81/
