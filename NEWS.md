# markets 1.1.2

* Corrected bug of ignoring custom title, or axis labels in market fit plots.

# markets 1.1.1

* Switched to `ginv` from `MASS` for the variance-covariance matrix calculation in `2SLS` estimations.
* Added previously unhanded corner cases when using splines in `2SLS` estimations.

# markets 1.1.0

* Improved figure resolution in the package's paper.

# markets 1.0.12

* Reorganized and extended documentation.

# markets 1.0.11

* Removed non exported variable name access functions from documentation.

# markets 1.0.10

* Removed warnings when compiling without `GSL`. 

# markets 1.0.9

* Adjustments to the integration of `maximize_log_likelihood` to `estimate` accommodating cases where `GSL` is unavailable. 

# markets 1.0.8

* Integrated functionality of `maximize_log_likelihood` to `estimate`. Equilibrium likelihoods can be optimized via `GSL` by passing the option `optimizer = gsl` to `estimate`.

# markets 1.0.7

* Added significance stars in summaries.
* Fixed calculation of two stage least square correlation coefficient.

# markets 1.0.6

* Cumulative update of CRAN version. 
* Improvements in the organization of the documentation.

# markets 1.0.5.9013

* Fixed bug in `maximize_log_likelihood` introduced in 1.0.5.9012.

# markets 1.0.5.9012

* Removed dependency to package `systemfit`. Linear estimations of the equilibrium model use `ls`.

# markets 1.0.5.9011

* Removed dependency to package `tidyr`. Using `expand.grid` instead of `crossing`.

# markets 1.0.5.9010

* Removed dependency to package `tibble`. Using data frames for storing model data.

# markets 1.0.5.9009

* Updated package paper.

# markets 1.0.5.9008

* Changed the relationship between market models and fits from 'is a' to 'has a' to avoid class union inheritance issues.

# markets 1.0.5.9007

* Remove experimental code introduced with 1.0.5.9002 from vignettes.

# markets 1.0.5.9006

* Fixed Hessian skipping bug introduced with 1.0.5.9004.

# markets 1.0.5.9005

* Updated formula implementation to allow using inline variable transformations (offset, splines, log, etc). Added unit tests checking the updated functionality.	

# markets 1.0.5.9004

* Replaced `minus_log_likelihood` with `log_likelihood` and adjusted `gradient`, `scores` and `hessian`. Gradient and hessian return derivatives of log-likelihood instead of derivatives of minus log-likelihood.

# markets 1.0.5.9003

* Re-implemented the logging, the summary, and the show functionality to respect the width set in `options()`.

# markets 1.0.5.9002

* Removed `bbmle` imports from all files.

# markets 1.0.5.9001

* Minor vignette changes adjusting to dropping `bbmle` from the dependencies.

# markets 1.0.5

* Removed dependency to package `bbmle`. Using directly `optim` for maximum likelihood estimations.

# markets 1.0.4.9003

* Patched logging functionality to respect the console width set in options.

# markets 1.0.4.9002

* Removed logger, equation, and system back-end classes from documentation.

# markets 1.0.4.9001

* Removed faulty default from the generic of function `scores`.

# markets 1.0.4

* Removed dependency to package `magrittr`.

# markets 1.0.3

* Updated package vignette (PDF manuscript) to reflect the latest functionality.

# markets 1.0.2

* Documented `market_fit` object.
* Documented return values of `plot`, `show`, and `summaries`.
* Updated documentation website.

# markets 1.0.1

* Corrected date in `DESCRIPTION`.

# markets 1.0.0

* Changed package name from `diseq` to `markets`.

# markets 0.4.3

* Adjusted unit test gradient tolerance for m1 machines.
* Replaced omitted with missing in warning messages.

# markets 0.4.2

* Cumulative update of CRAN version. 
* Specialized calculation of initializing values at a model level. Initializing values are now calculated based on the models' assumptions.
* Better `coef` behavior with common output format for all models.
* Extended shortage analysis functionality to the equilibrium model.

# markets 0.4.1.9005

* Extended the shortage and marginal effect analyses to the cover the equilibrium model.

# markets 0.4.1.9004

* Separated identifier variables  in `market_model` class.
* Changes in aggregation functionality. Updated its documentation.
* Added aggregation plot in 'basic usage' vignette.

# markets 0.4.1.9003

* Harmonized `coef` output.
* Simplified `diseq_directional` calculations to improve numerical stability.
* Model specific initializing values for maximum likelihood estimation
 - `diseq_basic`: Demand and supply regression estimates using the whole sample.
 - `diseq_deterministic_adjustment`: Demand and supply regression estimates using the whole sample. Price differences are regressed on estimated excess demand for the price equation.
 - `diseq_directional`: Demand and supply regression estimates using the sample separation. 
 - `diseq_deterministic_adjustment`: Demand, supply, and price dynamics regression estimates using the whole sample. 
 - `equilibrium_model`: Two stage least square estimates. 
* Simplified simulation of prices and controls.

# markets 0.4.1

* Cumulative update of CRAN version. 
* Version 0.4 introduces user space changes.
 - Model can be initialized using formulas
 - Introduced functions for single call initialization and estimation of models. The old methods for constructing and estimating models are still exported.
 - Introduced estimation output class `market_fit`. The class further unifies the user interface for accessing market models.
 - Estimation output can be summarized by calling `summary` with `market_fit` objects.
 - Added new plotting functionality on the estimation output.
 - Added coefficient access method `coef`.
 - Added variance-covariance access method `vcov`.
 - Added `logLik` object access method.
 - Added `formula` object access method.
* Documentation changes.
 - Examples and vignettes were adjusted to exemplify the new user interface.
 - Documentation entry added for model initialization based on formulas.
 - Added vignette `more_details.Rmd` with initialization and estimation details.

# markets 0.3.1.9005

* Fixed equations (issue #24) in GitHub document.

# markets 0.3.1.9004

* Fixed broken link in README.

# markets 0.3.1.9003

* Updated README planned extensions section.

# markets 0.3.1.9002

* Changed the simulation parameters of the `basic_usage` vignette to produce more balanced sample data in the models that use sample separation.

# markets 0.3.1.9001

* Added link useR!2021 video and slides in the README file.
* Fixed math display issues in GITHUB markdown page.

# markets 0.3.1

* Cumulative update of CRAN version.

# markets 0.3.0.9004

* Included package article in documentation website.

# markets 0.3.0.9003

* Added package article.

# markets 0.3.0.9002

* Fixed bug in `show` and `summary` methods of `diseq_stochastic_adjustment`.
* Fixed bug in calculation of clustered standard errors.
* Changed input arguments of marginal effect calls to match the interface of the remaining post-analysis calls (changes the user space).
* Added `prefixed_quantity_variable` method.
* Added implementation figure.

# markets 0.3.0.9001

* Fixed R check missing documentation entries.

# markets 0.3.0.9000

* Changes in model simulation.
 - Simplified simulation calls (changes the user space).
 - Re-factored simulation code and exported additional functions. 
 - Added marginal system effect methods, and unified marginal probabilities effects methods (changes the user space).
* Improvements in documentation.
 - Minor typos corrections.
 - Added new examples.
 - Documented formulas in system and equation classes. 
 - Modified some of the examples to use the `houses` dataset.
 - Grouped documentation entries.

# markets 0.2.1

* Cumulative update of CRAN version.

# markets 0.2.0.9010

* Reduced file size of stochastic adjustment model's derivative calculations.

# markets 0.2.0.9009

* Reduced file size of directional model's derivative calculations.

# markets 0.2.0.9008

* Reduced file size of basic model's derivative calculations.

# markets 0.2.0.9007

* Reduced file size of deterministic adjustment gradient calculation.

# markets 0.2.0.9006

* Reduced file size of equilibrium gradient calculation.

# markets 0.2.0.9005

* Removed get from access functions to reduce the verbosity of function calls. 

# markets 0.2.0.9004

* Added validation functions for estimation input variables `gradient`, `hessian`, and `standard_errors`. 

# markets 0.2.0.9003

* The input variable `gradient` controls whether the gradient is calculated by analytic expression or is numerically approximated. Switched from Boolean input to passing sting options so that the user interface for choosing gradient and hessian options is consistent.

# markets 0.2.0.9002

* Fixed bug in `equilibrium_model` plot functionality.
* Minor improvements in `houses` documentation.

# markets 0.2.0.9001

* Better options for hessian estimation: Consolidated all three potential options in the `hessian` input variable of `estimate`.
* Better options for adjusted standard errors: Consolidated all three potential options in the `standard_errors` input variable of `estimate`.

# markets 0.1.5.9003

* Added houses dataset

# markets 0.1.5.9002

* Fixed option class concerning the calculation of the Hessian in estimation calls. Models can be now estimated by skipping the Hessian, calculating it based on the analytic expressions, or calculating it numerically.

# markets 0.1.5.9001

* Corrected bug in initialization of indicator variables.

# markets 0.1.5

* Deployed development documentation website.

# markets 0.1.4

* Cumulative patch of CRAN version.

# markets 0.1.3.9012

* Updated the description entry of the `DESCRIPTION` file.
* Shortened `use_heteroscedasticity_consistent_errors` variable of `estimate` method to `use_heteroscedastic_errors`.

# markets 0.1.3.9011

* Added python script for creating the `README` figures.

# markets 0.1.3.9010

* Corrected calls to `system.file`.

# markets 0.1.3.9009

* Updated `README.md`.
* Enclosed the plot example with `dontrun` instead of `donttest`.

# markets 0.1.3.9008

* Added `png` and `grid` to dependencies.

# markets 0.1.3.9007

* Added `plot` method for the all model classes.

# markets 0.1.3.9006

* Added `plot` method for the equilibrium and basic disequilibrium model classes.
* Patched model initialization to avoid mutate warnings.

# markets 0.1.3.9005

* Added `summary` method for the front-end model classes.
* Documentation improvements.
* Added online documentation link in `README.Rmd`
* Corrected link in the documentation of the summary method.

# markets 0.1.3.9004

* Added `show` method for the front-end model classes.

# markets 0.1.3.9003

* Removed `compile_commands.json` from source control.
* Modified the simulation parameters of the market clearing assessment vignette.

# markets 0.1.3.9002

* Included a reference section title in the README file.

# markets 0.1.3.9001

* Added documentation URL in DESCRIPTION.
* Added bibliography in the README file.

# markets 0.1.3

* Patched `M1mac` additional issues: Added compilation flag for availability of `GSL`. The native code can be compiled also in systems without `GSL`, albeit offering an empty shell functionality for the moment.
* Documented changes in `maximize_log_likelihood` function.

# markets 0.1.2

* Added `autotools` configuration script for cross-platform compilation. 
* Removed dependence on `C++20`. The sources are now `C++11` compliant and only use `C++17` and `libtbb` if it is available on the target machine. 
* Patch for `clang` compilation failure: reverting to sequential execution when compiling with clang and `libc++`. 

# markets 0.1.1.9001

* Restructured and added unit tests to increase test coverage. 

# markets 0.1.1

* Prepared CRAN submission. Small adjustments to README style. Updated CRAN comments.

# markets 0.1.0.9004

* Adjusted file names so that they are consistent with the  API changes.

# markets 0.1.0.9003

* Fixed `M1mac` issues. Adjusted README to API changes. 
* Replaced `href` with `doi` whenever relevant. 

# markets 0.1.0.9002

* Added macro checks for C++20 execution policies features in C++ sources. 
* Removed calls to `std::ragnes::iota_view` and `std::reduce` to ensure C++11 compatibility.

# markets 0.1.0.9001

* Adjusted vignettes to API changes. 

# markets 0.1.0.9000

* Introduced the option maximizing the equilibrium model likelihood using `GSL` through `Rcpp`. 
* Added linting and formatting configuration files for R and C++ code. Cleaned C++ code. 
* Reorganized R back-end classes.

# markets 0.0.14.9004

* Improved README file style.

# markets 0.0.14.9003

* Corrected style attributes of README file.

# markets 0.0.14.9002

* Corrected calculation of clustered standard errors by accounting for the number of used classes. 

# markets 0.0.14.9001

* Changes to adjust for depreciating functionality of `dplyr` (as of 0.7.0)

# markets 0.0.14

* Added option and documentation for estimating clustered standards errors.

# markets 0.0.13.9002

* Added documentation for the function that return the scores.

# markets 0.0.13.9001

* Added option for estimating heteroscedasticity-consistent (Huber-White) standard errors. 
* Added functionality for extracting the score matrices of the estimated models. 

# markets 0.0.13.9000

* Corrected documentation typos.

# markets 0.0.13

* Corrections of non-canonical web-links in README. Adjustments before CRAN submission.

# markets 0.0.12.9002

* Added sections `A quick model tour`, `Alternative packages`, and `Planned extensions` in README.

# markets 0.0.12.9001

* Added `noLD` in word exceptions list.

# markets 0.0.12

* Fixed `noLD` issues.

# markets 0.0.11.9002

* Renamed assessment vignette.

# markets 0.0.11.9001

* Enabled BFGS-based estimation with numerical gradient. 
* Added CRAN installation instructions in README.

# markets 0.0.11.9000

* Corrected punctuation errors in documentation.

# markets 0.0.11

* Removed `get_correlation_variable` from exported functions. 
* Improved the documentation of `minus_log_likelihood`. 
* Reintroduced references in description.

# markets 0.0.10

* Removed references from description to avoid CRAN notes.

# markets 0.0.9

* Ignoring README.html from build. Removed links from description. Improved documentation examples.

# markets 0.0.8

* Added examples to constructors, estimation, aggregation, and marginal effect functions.

# markets 0.0.7.9002

* Skipping directional and stochastic adjustment tests on CRAN to reduce build time.

# markets 0.0.7.9001

* Quoted all package names in DESCRIPTION. 
* To reduce build time: 1. Removed direction model estimation from equilibrium assessment vignette, 2. Decreased estimation accuracy of basic usage vignette to six digits.

# markets 0.0.7

* Fixed order of arguments in web-link of estimation documentation. 
* Improved simulation documentation.

# markets 0.0.6

* Corrected documentation typos. Fixed web-links.

# markets 0.0.5.9009

* Improved documentation.

# markets 0.0.5.9008

* Removed unused parameter from the constructor of the equilibrium two stage least square model.

# markets 0.0.5.9007

* Removed dependence on `pastecs` package.

# markets 0.0.5.9006

* Reformatted code using the `styler` package. Removed the `lintr` based test.

# markets 0.0.5.9005

* Adjustments to address breaking changes of the `tibble` package.

# markets 0.0.5.9004

* Added a vignette with an equilibrium assessment example.

# markets 0.0.5.9003

* Added model-specific simulation functions.

# markets 0.0.5.9002

* Refactored simulation code.

# markets 0.0.5.9001

* Added simulation generating processes for all supported models. 

# markets 0.0.4.9013

* Separated auto-generated derivative code to dedicated derivative files. 

# markets 0.0.4.9012

* Allowed estimation of full information maximum likelihood, equilibrium, deterministic adjustment, and stochastic adjustment with one-sided inclusion of prices.
* Modified model titles' generation.

# markets 0.0.4.9011

* Added basic_usage vignette.
* Added simulation function at model_base level. 
* Added a `NEWS.md` file to track changes to the package.
