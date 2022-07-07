## Changes in version 1.0.6

* Removed dependencies to packages `systemfit`,  `tidyr`, `tibble`, `bbmle`, `magrittr` that were not extensively used in the package. 
* Updated formula implementation to allow using inline variable transformations (offset, splines, log, etc). Added unit tests checking the updated functionality.
* Changed the relationship between market models and fits from 'is-a' to 'has-a' to avoid class union inheritance issues.
* Replaced `minus_log_likelihood` with `log_likelihood` and adjusted `gradient`, `scores` and `hessian`. Gradient and hessian return derivatives of log-likelihood instead of derivatives of minus log-likelihood.
* Re-implemented the logging, the summary, and the show functionality to respect the width set in `options()`.
* Improvements in the organization of the documentation.
* Updated package paper.

# Test Environments and Results
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 134
Check time in seconds: 422
Status: 1 NOTE
R Under development (unstable) (2022-07-04 r82541 ucrt)

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
	
Results: https://win-builder.r-project.org/V8H99c2I8SpM/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 133
Check time in seconds: 462
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

Results: https://win-builder.r-project.org/92mo4wExjUTf/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 120
Check time in seconds: 419
Status: 1 NOTE
R version 4.2.1 (2022-06-23 ucrt)

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
	
Results: https://win-builder.r-project.org/qoQOWwnA4kjG/

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
── R CMD check results ──────────────────────────────────────────────────── markets 1.0.6 ────
Duration: 1m 38.1s

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
### (12) rhub solaris-x86-patched -- OK
Build ID:	markets_1.0.6.tar.gz-691239b68a904937990fea86364c9d9e
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	33 minutes 51.8 seconds ago
Build time:	33 minutes 47.7 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.6.tar.gz-691239b68a904937990fea86364c9d9e/markets.Rcheck/

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	markets_1.0.6.tar.gz-18d29d4c410d4fffa087f59fd7c40afe
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	7 minutes 52.3 seconds ago
Build time:	7 minutes 49 seconds


See the full build log: https://artifacts.r-hub.io/markets_1.0.6.tar.gz-18d29d4c410d4fffa087f59fd7c40afe/markets.Rcheck/

### (14) rhub macos-highsierra-release -- OK
Build ID:	markets_1.0.6.tar.gz-47b05f1bdc5040eeb0cf468391b10b6a
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	7 minutes 51.7 seconds ago
Build time:	7 minutes 49.3 seconds

See the full build log: https://artifacts.r-hub.io/markets_1.0.6.tar.gz-47b05f1bdc5040eeb0cf468391b10b6a/markets.Rcheck/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    libs   3.8Mb
	
Results: https://mac.r-project.org/macbuilder/results/1657045898-c9f63db64cd04f3f/
### (16) rhub macos-m1-bigsur-release  -- 1 WARNING
Build ID:	markets_1.0.6.tar.gz-8844aeeeb6fe48bc9e2b4ca279d15be1
Platform:	Apple Silicon (M1), macOS 11.6 Big Sur, R-release
Submitted:	3 minutes 2.5 seconds ago
Build time:	2 minutes 56.9 seconds

WARNINGS:
* checking re-building of vignette outputs ... WARNING
Error(s) in re-building vignettes:
  ...
--- re-building ‘basic_usage.Rmd’ using rmarkdown
Quitting from lines 187-219 (basic_usage.Rmd) 
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

See the full build log: https://artifacts.r-hub.io/markets_1.0.6.tar.gz-8844aeeeb6fe48bc9e2b4ca279d15be1/markets.Rcheck/
