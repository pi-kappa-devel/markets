## Changes in version 1.1.6

* Addressing documentation issues before 2025-08-25:
  - Removed relative links to README file from `markets.R` documentation.
  - Removed internal keyword from overview page.
* Further documentation improvements:
  - Added alt text for vignette figures and images.
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
Running R code in ‘testthat.R’ had CPU time 3.2 times elapsed time
 [27s/9s] NOTE

 * checking re-building of vignette outputs ... [254s/71s] NOTE
Re-building vignettes had CPU time 3.6 times elapsed time

### ubuntu-release [Status: 2 NOTEs]
 * checking tests ...
  Running ‘testthat.R’ [27s/8s]
Running R code in ‘testthat.R’ had CPU time 3.2 times elapsed time
 [27s/9s] NOTE

 * checking re-building of vignette outputs ... [254s/71s] NOTE
Re-building vignettes had CPU time 3.6 times elapsed time

### windows [Status: 1 WARNING]
 * checking line endings in shell scripts ... WARNING
Warning: Found the following shell script(s) with CR or CRLF line endings:
  configure.ac
Non-Windows OSes require LF line endings.
