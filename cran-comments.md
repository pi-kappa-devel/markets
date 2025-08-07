## Changes in version 1.1.6

* Addressing documentation issues before 2025-08-25:
  - Removed relative links to README file from `markets.R` documentation.
  - Removed internal keyword from overview page.
* Further documentation improvements:
  - Added alt text for vignette figures and images.
  - Updated moved `cppreference.com` URLs.
* Unit test changes:
  - Patched summary output tests for setups with customized output width.
* Continuous integration Changes
  - Replaced custom github workflows with rhub-provided workflows.

# Test Environments and Results
## Rhub
✔ Check started: macos-arm64, windows, clang19, clang20, gcc14, gcc15, nold, ubuntu-next, ubuntu-release
  (censorious-viperfish).
  See <https://github.com/pi-kappa-devel/markets/actions> for live output!

### clang19, clang20 [Status: 1 NOTE]
* checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
  ‘-Wp,-D_FORTIFY_SOURCE=3’

### macos-arm64, gcc14, gcc15, nold [Status: OK]

### ubuntu-next [Status: 2 NOTEs]
* checking tests ...
  Running ‘testthat.R’ [27s/8s]
  Running R code in ‘testthat.R’ had CPU time 3.2 times elapsed time [27s/9s] NOTE

* checking re-building of vignette outputs ... [254s/71s] NOTE
  Re-building vignettes had CPU time 3.6 times elapsed time

### ubuntu-release [Status: 2 NOTEs]
* checking tests ...
  Running ‘testthat.R’ [27s/8s]
  Running R code in ‘testthat.R’ had CPU time 3.2 times elapsed time [27s/9s] NOTE

* checking re-building of vignette outputs ... [254s/71s] NOTE
  Re-building vignettes had CPU time 3.6 times elapsed time

### windows [Status: 1 WARNING]
* checking line endings in shell scripts ... WARNING
  Warning: Found the following shell script(s) with CR or CRLF line endings:
  configure.ac
  Non-Windows OSes require LF line endings.

## [Mac builder](https://mac.r-project.org/macbuilder/submit.html)

### Release [Status: OK]

https://mac.r-project.org/macbuilder/results/1754603049-f0f6b5c1c8d1a85e/

### Devel [Status: OK]

https://mac.r-project.org/macbuilder/results/1754603251-a7d14f51ec47c761/

## [Win builder](https://win-builder.r-project.org/)

### Devel [Status: OK]

https://win-builder.r-project.org/I0yv283bF724/

### Release [Status: OK]

https://win-builder.r-project.org/v2rBD22InK3I/

### Old Release [Status: 1 NOTE]

https://win-builder.r-project.org/KKVm108eDjk8/

* checking DESCRIPTION meta-information ... NOTE
Author field differs from that derived from Authors@R
  Author:    'Pantelis Karapanagiotis [aut, cre] (ORCID: <https://orcid.org/0000-0001-9871-1908>)'
  Authors@R: 'Pantelis Karapanagiotis [aut, cre] (<https://orcid.org/0000-0001-9871-1908>)'

