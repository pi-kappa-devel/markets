## Changes in version 1.1.7

* Addressing CRAN issues need 2026-02-25:
  - Using `.data` to access `id` in `simulate_quantities_and_prices,simulated_model`.
* Further documentation improvements:
  - Updated logo to hexagon using `hexSticker`.

# Test Environments and Results
## Rhub
✔ Check started: macos-arm64, windows, clang20, gcc15, nold, ubuntu-next, ubuntu-release
  (bridal-pacificparrotlet).
  See <https://github.com/pi-kappa-devel/markets/actions> for live output!

### macos-arm64 [Status: Error]

- fatal error: 'R_ext/Callbacks.h' file not found

### clang20 [Status: 1 Note]

* checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  ‘-Wp,-D_FORTIFY_SOURCE=3’

### gcc15, nold [Status: Ok]

### ubuntu-next, ubuntu-release [Status: 2 NOTEs]
* checking tests ...
  Running ‘testthat.R’ [27s/8s]
Running R code in ‘testthat.R’ had CPU time 3.2 times elapsed time
 [28s/9s] NOTE
* checking re-building of vignette outputs ... [262s/73s] NOTE
Re-building vignettes had CPU time 3.6 times elapsed time

### windows [Status: 1 WARNING]
* checking line endings in shell scripts ... WARNING
  Warning: Found the following shell script(s) with CR or CRLF line endings:
  configure.ac
  Non-Windows OSes require LF line endings.

## [Mac builder](https://mac.r-project.org/macbuilder/submit.html)

### Devel [Status: Error]

- fatal error: 'R_ext/Callbacks.h' file not found

https://mac.r-project.org/macbuilder/results/1770229157-d9ec9c65474930b0/

## [Win builder](https://win-builder.r-project.org/)

### Devel [Status: OK]

https://win-builder.r-project.org/P8R5GPjWxgk2/

### Release [Status: OK]

https://win-builder.r-project.org/6wI6Xx29KoI7/

### Old Release [Status: 1 NOTE]
* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Pantelis Karapanagiotis [aut, cre] (ORCID: <https://orcid.org/0000-0001-9871-1908>)'
  Authors@R: 'Pantelis Karapanagiotis [aut, cre] (<https://orcid.org/0000-0001-9871-1908>)'
