#' @include equilibrium_model.R
#' @include diseq_basic.R
#' @include diseq_directional.R
#' @include diseq_deterministic_adjustment.R
#' @include diseq_stochastic_adjustment.R
#' @importFrom utils capture.output
#' @importFrom stats optimHess

#' @title Market model fit
#'
#' @slot model The underlying market model object.
#' @slot fit A list holding estimation outputs.
#' @examples
#' # estimate an equilibrium  model using the houses dataset
#' fit <- equilibrium_model(
#'   HS | RM | ID | TREND ~
#'     RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'       RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),
#'   estimation_options = list(method = "2SLS")
#' )
#'
#' # access an method of the underlying model
#' aggregate_demand(fit)
#'
#' # summary of results
#' summary(fit)
#' @seealso \code{\link{market_models}}
#' @description
#' This is the estimation output class for all market models of the package. It couples
#' a market model object with estimation results. It provides a common user interface
#' for accessing estimation results, irrespective of the underlying market model used.
#' The estimation results are intended to be accessed by passing \code{market_fit}
#' objects to methods such as \code{\link{plot}}, \code{\link{summary}}, and
#' \code{\link{logLik}}.
#' @details
#' The \code{market_fit} class composes the \code{\link{market_models}}
#' class with the estimation results obtained by \code{\link[stats]{optim}},
#' \code{\link[stats]{lm}} or \code{GSL}. All the public functionality of the
#' underlying market model is also directly accessible from the output class.
#'
#' Furthermore, the class is responsible for harmonizing the heterogeneous
#' outputs resulting from different estimation methods of market models. For
#' example, a \code{2SLS} estimation of the
#' \code{\linkS4class{equilibrium_model}} returns a list of linear regression
#' models (the first stage, demand, and supply models), while the maximum
#' likelihood estimation of \code{\linkS4class{diseq_basic}} returns an
#' \code{\link[stats]{optim}} list. In both cases, the \code{market_fit}
#' stores the estimation output in the member \code{fit} of type \code{list}
#' and produces additional harmonized list elements. Methods of the class
#' examine the type of the \code{fit} and direct execution accordingly to different
#' branches to produce a unified experience for the caller.
#' @export
setClass(
  "market_fit",
  representation(
    model = "market_model",
    fit = "list"
  ),
  prototype()
)

setMethod(
  "initialize", "market_fit",
  function(.Object, market_model, estimate) {
    .Object@model <- market_model
    .Object@fit <- estimate

    .Object
  }
)

market_fit_coefficients <- function(object, summary = FALSE) {
  coefs <- object@fit$par

  if (summary) {
    if (any(is.na(coefs))) {
      coefs <- cbind(
        Estimate = coefs, `Std. Error` = coefs,
        `z value` = coefs, `Pr(>|z|)` = coefs
      )
    } else {
      sds <- sapply(diag(vcov(object)), function(s) ifelse(s >= 0, sqrt(s), NaN))
      zvals <- coefs / sds
      pvals <- 2 * pnorm(-abs(zvals))
      coefs <- cbind(
        Estimate = coefs, `Std. Error` = sds,
        `z value` = zvals, `Pr(>|z|)` = pvals
      )
    }
  }

  coefs
}

common_market_fit_show <- function(object, summary = FALSE) {
  if (object@fit$method == "2SLS") {
    cat("\nLeast squares estimation:", sep = "", fill = TRUE)
  } else {
    cat("\nMaximum likelihood estimation:", sep = "", fill = TRUE)
  }
  cat(
    labels = sprintf("  %-20s:", "Method"), object@fit$method,
    sep = "", fill = TRUE
  )
  if (object@fit$method != "2SLS") {
    if (object@fit$optimizer == "gsl") {
      args <- object@fit
    } else { # optim
      args <- object@fit$call[[2]]
    }
    if (summary) {
      if (!is.null(args$control$maxit)) {
        cat(
          labels = sprintf("  %-20s:", "Max Iterations"), args$control$maxit,
          sep = "", fill = TRUE
        )
      }
      if (!is.null(args$control$reltol)) {
        cat(
          labels = sprintf("  %-20s:", "Relative Tolerance"),
          args$control$reltol,
          sep = "", fill = TRUE
        )
      }
    }
    cat(
      labels = sprintf("  %-20s:", "Convergence Status"),
      ifelse(!object@fit$convergence, "success",
        sprintf("failure (%d)", object@fit$convergence)
      ),
      sep = "", fill = TRUE
    )
  }
}

#' @rdname show
#' @export
setMethod("show", signature(object = "market_fit"), function(object) {
  show(object@model)
  common_market_fit_show(object)
})

#' @describeIn summary Summarizes the model's fit.
#' @description \code{market_fit}: Prints basic information about the
#' passed model fit. In addition to the output of
#' the model's \code{summary} method, the function prints basic
#' estimation results. For a maximum likelihood estimation, the function prints
#' \itemize{
#' \item the used optimization method,
#' \item the maximum number of allowed iterations,
#' \item the relative convergence tolerance (see \code{\link[stats]{optim}}),
#' \item the convergence status,
#' \item the initializing parameter values,
#' \item the estimated coefficients, their standard errors, Z values, and P values, and
#' \item \eqn{-2 \log L} evaluated at the maximum.
#' }
#' For a linear estimation of the equilibrium system, the function prints
#' \itemize{
#' \item the used method,
#' \item the summary of the first stage regression,
#' \item the summary of the demand (second stage) regression, and
#' \item the summary of the supply (second stage) regression.
#' }
#' @return No return value, called for for side effects (print summary).
#' @export
setMethod("summary", signature(object = "market_fit"), function(object) {
  summary(object@model)
  if (object@fit$method != "2SLS") {
    args <- object@fit$call[[2]]

    common_market_fit_show(object, summary = TRUE)
    cat(sprintf("  %-20s:", "Starting Values"), sep = "", fill = TRUE)
    print(object@fit$start, digits = 4)

    cat("\nCoefficients:", sep = "", fill = TRUE)
    coefs <- market_fit_coefficients(object, summary = TRUE)
    stars <- function(p) {
      if (is.na(p) || p >= 1e-1) {
        " "
      } else if (p < 1e-3) {
        "***"
      } else if (p < 1e-2) {
        "**"
      } else if (p < 5e-2) {
        "*"
      } else {
        "."
      }
    }

    cat(sprintf(
      "%s %s\n", capture.output(print(coefs)),
      c("", sapply(coefs[, 4], stars))
    ))
    cat(
      "---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      sep = "", fill = TRUE
    )
    cat(
      labels = sprintf("\n%s:", "-2 log L"),
      -2 * logLik(object),
      sep = "", fill = TRUE
    )
  } else {
    common_market_fit_show(object, summary = TRUE)
    cat("\nShocks:", sep = "", fill = TRUE)
    ncoef <- length(object@fit$par)
    vcovi <- ncoef - 2
    cat(
      sprintf(
        "  %-20s: %g\n", names(object@fit$par)[vcovi:ncoef],
        object@fit$par[vcovi:ncoef]
      ),
      sep = ""
    )
    cat("\nFirst Stage:", sep = "", fill = TRUE)
    print(summary(object@fit$first_stage_model))
    cat("\nDemand Equation:", sep = "", fill = TRUE)
    print(summary(object@fit$demand_model))
    cat("\nSupply Equation:", sep = "", fill = TRUE)
    print(summary(object@fit$supply_model))
  }
})

#' Model estimation
#'
#' All models are estimated using full information maximum likelihood. The
#' \code{\linkS4class{equilibrium_model}} can also be estimated using two-stage
#' least squares. The maximum likelihood estimation is based on
#' \code{\link[stats]{optim}}. If no starting values are provided, the function uses
#' linear regression estimates as initializing values. The default optimization method is
#' BFGS. For other alternatives see \code{\link[stats]{optim}}. The implementation of
#' the two-stage least square estimation of the \code{\linkS4class{equilibrium_model}}
#' is based on \code{\link[stats]{lm}}.
#' @param object A model object.
#' @param ... Additional parameter used in the model's estimation. These are
#' passed further down to the optimization call. For the
#' \code{\linkS4class{equilibrium_model}} model, the parameters are passed to
#' \code{\link[stats]{lm}}, if the method is set to
#' \code{2SLS}, or to \code{\link[stats]{optim}} for any other method. For the rest of
#' the models, the parameters are passed to \code{\link[stats]{optim}}.
#' @return A market fit object holding the estimation result.
#' @rdname estimate
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   subject = ID, time = TREND, quantity = HS, price = RM,
#'   demand = RM + TREND + W + CSHS + L1RM + L2RM + MONTH,
#'   supply = RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # let shocks be independent
#' )
#'
#' # estimate the model object (BFGS is used by default)
#' fit <- estimate(model)
#'
#' # estimate the model by specifying the optimization details passed to the optimizer.
#' fit <- estimate(model, control = list(maxit = 1e+6))
#'
#' # summarize results
#' summary(fit)
#' }
#' @export
setGeneric("estimate", function(object, ...) {
  standardGeneric("estimate")
})

#' @describeIn estimate Full information maximum likelihood estimation.
#' @param gradient One of two potential options: \code{"numerical"} and
#' \code{"calculated"}. By default, all the models are estimated using the
#' analytic expressions of their likelihoods' gradients.
#' @param hessian One of three potential options: \code{"skip"},
#' \code{"numerical"}, and \code{"calculated"}. The default is to use the
#' \code{"calculated"} Hessian for the models that expressions are
#' available and the \code{"numerical"} Hessian in other cases. Calculated
#' Hessian expressions are available for the basic and directional models.
#' @param standard_errors One of three potential options:
#' \code{"homoscedastic"}, \code{"heteroscedastic"}, or a vector with
#' variables names for which standard error clusters are to be created. The
#' default value is \code{"homoscedastic"}. If the option
#' \code{"heteroscedastic"} is passed, the variance-covariance matrix is
#' calculated using heteroscedasticity adjusted (Huber-White) standard errors.
#' If a vector with variable names is supplied, the variance-covariance
#' matrix is calculated by grouping the score matrix based on the
#' passed variables.
setMethod(
  "estimate", signature(object = "market_model"),
  function(object, gradient = "calculated", hessian = "calculated",
           standard_errors = "homoscedastic", ...) {
    validate_gradient_option(object, gradient)
    validate_hessian_option(object, hessian)
    validate_standard_error_option(object, standard_errors)

    va_args <- list(...)

    if (hessian == "skip" ||
      ((object@model_name %in% c("Basic", "Directional")) &&
        hessian == "calculated")) {
      va_args$hessian <- FALSE
    } else {
      va_args$hessian <- TRUE
      hessian <- "numerical"
    }

    va_args$par <- start <- prepare_initializing_values(object, va_args$start)
    va_args$start <- NULL

    if (is.null(va_args$method)) {
      va_args$method <- "BFGS"
    }

    if (is.null(va_args$optimizer)) {
      va_args$optimizer <- "optim"
    }
    optimizer <- va_args$optimizer
    va_args$optimizer <- NULL

    if (optimizer == "gsl") {
      cpp_model <- new(cpp_equilibrium_model, object@system)
      fit <- do.call(
        cpp_model$minimize,
        list(par = va_args$par, control = va_args$control)
      )
      names(fit$par) <- likelihood_variables(object@system)
      names(fit$gradient) <- likelihood_variables(object@system)
      if (fit$convergence != -1) {
        fit$gradient <- -fit$gradient
        if (hessian != "skip") {
          fit$hessian <- optimHess(
            fit$par, function(...) log_likelihood(object, ...)
          )
        }
      } else {
        fit$par[1:length(fit$par)] <- NaN
        fit$gradient[1:length(fit$gradient)] <- NaN
      }
    } else { # optim
      va_args$fn <- function(...) -log_likelihood(object, ...)
      if (gradient == "calculated") {
        va_args$gr <- function(...) -gradient(object, ...)
      }
      fit <- do.call(optim, va_args)
      fit$call <- call("optim", va_args)
      if (hessian == "calculated") {
        print_verbose(
          object@logger,
          "Calculating hessian and variance-covariance matrix."
        )
        fit$hessian <- hessian(object, fit$par)
      } else if (hessian == "numerical") {
        fit$hessian <- -fit$hessian
      }
    }
    fit$start <- start
    fit$optimizer <- optimizer
    fit$method <- va_args$method
    fit$value <- -fit$value

    if (!is.null(fit$hessian)) {
      if (standard_errors == "heteroscedastic") {
        fit <- set_heteroscedasticity_consistent_errors(object, fit)
      } else if (standard_errors == "homoscedastic") {
        adjustment <- nobs(object) / (nobs(object) - ncoef(object))
        tryCatch(
          fit$vcov <- -MASS::ginv(fit$hessian) * adjustment,
          error = function(e) print_warning(object@logger, e$message)
        )
      } else {
        fit <- set_clustered_errors(object, fit, standard_errors)
      }
      colnames(fit$vcov) <- likelihood_variables(object@system)
      rownames(fit$vcov) <- likelihood_variables(object@system)
    }

    new("market_fit", object, fit)
  }
)

#' @describeIn estimate Equilibrium model estimation.
#' @details
#' The likelihood of the equilibrium model can be optimized either by using \code{optim}
#' (the default option) or native
#' \href{https://www.gnu.org/software/gsl/doc/html/multimin.html}{\code{GSL}} routines.
#' The caller can override the default behavior by setting the \code{optimizer} argument
#' equal to \code{"gsl"}, in which case \code{GSL} routines are used. This does not
#' necessarily result to faster execution times. This functionality is primarily
#' intended for advanced usage. The \code{\link[stats]{optim}} functionality is a fast,
#' analysis-oriented alternative, which is more suitable for most use case.
#'
#' When \code{optimizer = "gsl"} is used, the only available optimization method is BFGS.
#' Additionally, the caller needs to specify in the control list values for the
#' optimization step (\code{step}), the objective's optimization tolerance
#' (\code{objective_tolerance}), the gradient's optimization tolerance
#' (\code{gradient_tolerance}, and the maximum allowed number of iterations (\code{maxit}).

#' If the \code{GSL} library is not available in the calling machine, the function
#' returns a trivial result list with convergence status set equal to -1. If the
#' \href{https://en.cppreference.com/w/cpp/algorithm/execution_policy_tag_t}{C++17 execution policies}
#' are available, the implementation of the optimization is parallelized.
#' @param method A string specifying the estimation method. When the passed value is
#' among \code{"Nelder-Mead"}, \code{"BFGS"}, \code{"CG"}, \code{"L-BFGS-B"},
#' \code{"SANN"}, and \code{"Brent"}, the model is estimated using
#' full information maximum likelihood based on \code{\link[stats]{optim}} functionality.
#' When \code{"2SLS"} is supplied, the model is estimated using two-stage least squares
#' via \code{\link[stats]{lm}}. In this case, the function returns a
#' list containing the first and second stage estimates. The default value is
#' \code{"BFGS"}.
#' @param optimizer One of two options:
#' \code{"optim"}, \code{"gsl"}. The default value is \code{"optim"}. If the
#' option \code{"gsl"} is set, the equilibrium likelihood is maximized using
#' \href{https://www.gnu.org/software/gsl/doc/html/multimin.html}{\code{GSL}}.
#' @examples
#' \donttest{
#' # simulate an equilibrium model
#' model <- simulate_model(
#'   "equilibrium_model", list(
#'     # observed entities, observed time points
#'     nobs = 500, tobs = 3,
#'     # demand coefficients
#'     alpha_d = -1.9, beta_d0 = 24.9, beta_d = c(2.3, -1.2), eta_d = c(2.0, -1.5),
#'     # supply coefficients
#'     alpha_s = .9, beta_s0 = 8.2, beta_s = c(3.3), eta_s = c(1.5, -2.2)
#'   ),
#'   seed = 99
#' )
#'
#' # maximize the model's log-likelihood
#' fit <- estimate(
#'   model,
#'   optimizer = "gsl", control = list(
#'     step = 1e-2, objective_tolerance = 1e-8,
#'     gradient_tolerance = 1e-2, maxit = 1e+3
#'   )
#' )
#'
#' summary(fit)
#' }
#' @export
setMethod(
  "estimate", signature(object = "equilibrium_model"),
  function(object, method = "BFGS", optimizer = "optim", ...) {
    if (method != "2SLS") {
      validate_optimizer_option(object, optimizer)
      return((selectMethod("estimate", "market_model"))(
        object, method = method, optimizer = optimizer, ...
      ))
    }

    quantity_variable <- colnames(object@system@quantity_vector)
    price_variable <- colnames(object@system@price_vector)

    ## create fitted variable
    fitted_column <- paste0(price_variable, "_FITTED")

    ## estimate first stage
    first_stage_controls <- attr(terms(object@system@formula, lhs = 0), "term.labels")
    first_stage_controls <- first_stage_controls[
      first_stage_controls != price_variable
    ]
    first_stage_formula <- paste0(
      price_variable, " ~ ", paste0(first_stage_controls, collapse = " + ")
    )

    first_stage_model <- lm(first_stage_formula, object@data)
    object@data[, fitted_column] <- first_stage_model$fitted.values

    ## estimate demand model
    independent <- gsub(
      sprintf("\\b%s\\b", price_variable), fitted_column,
      attr(terms(object@system@formula, lhs = 0, rhs = 1), "term.labels")
    )
    demand_formula <- formula(paste0(
      quantity_variable, " ~ ", paste0(independent, collapse = " + ")
    ))
    demand_model <- lm(demand_formula, object@data)

    ## estimate supply model
    independent <- gsub(
      sprintf("\\b%s\\b", price_variable), fitted_column,
      attr(terms(object@system@formula, lhs = 0, rhs = 2), "term.labels")
    )
    supply_formula <- formula(paste0(
      quantity_variable, " ~ ", paste0(independent, collapse = " + ")
    ))
    supply_model <- lm(supply_formula, object@data)

    inst <- formula(paste0(" ~ ", paste0(first_stage_controls, collapse = " + ")))

    # adjust coefficient names
    price_variable <- colnames(object@system@price_vector)
    adjust_names <- function(prefix, side) {
      paste(
        prefix, gsub(
          sprintf("\\b%s_FITTED\\b", price_variable), price_variable,
          names(side)
        ),
        sep = ""
      )
    }

    dcoefs <- demand_model$coefficients
    dcoefs <- c(dcoefs[2], dcoefs[1], dcoefs[-c(1, 2)])
    names(dcoefs) <- adjust_names("D_", dcoefs)

    scoefs <- supply_model$coefficients
    scoefs <- c(scoefs[2], scoefs[1], scoefs[-c(1, 2)])
    names(scoefs) <- adjust_names("S_", scoefs)

    var_d <- var(demand_model$residuals - dcoefs[1] * first_stage_model$residuals)
    names(var_d) <- prefixed_variance_variable(object@system@demand)

    var_s <- var(supply_model$residuals - scoefs[1] * first_stage_model$residuals)
    names(var_s) <- prefixed_variance_variable(object@system@supply)

    par <- c(dcoefs, scoefs, var_d, var_s)

    if (object@system@correlated_shocks) {
      rho <- cor(
        demand_model$residuals - dcoefs[1] * first_stage_model$residuals,
        supply_model$residuals - scoefs[1] * first_stage_model$residuals
      )
      names(rho) <- correlation_variable(object@system)
      par <- c(par, rho)
    }
    names(par) <- gsub("\\(Intercept\\)", "CONST", names(par))

    ## coefficient covariance matrix (following Henningsen A, Hamann JD (2007))
    coefs <- c(demand_model$coefficients, supply_model$coefficients)
    mp <- model.matrix(first_stage_model)
    md <- model.matrix(demand_model)
    ms <- model.matrix(supply_model)
    nobsd <- nrow(md)
    ncoefd <- ncol(md)
    nobss <- nrow(ms)
    ncoefs <- ncol(ms)
    nobsall <- nobsd + nobss
    ncoefall <- ncoefd + ncoefs
    var <- (
      sum(demand_model$residuals**2) + sum(demand_model$residuals**2)
    ) / (nobsall - ncoefall)

    zz <- crossprod(mp)
    xx <- rbind(
      mp %*% MASS::ginv(zz) %*% crossprod(mp, cbind(md, matrix(0, nobss, ncoefs))),
      mp %*% MASS::ginv(zz) %*% crossprod(mp, cbind(matrix(0, nobsd, ncoefd), ms))
    )
    vc <- var * MASS::ginv(crossprod(xx))
    vc <- vc[c(2, 1, 3:ncoefall), ]
    vc <- vc[, c(2, 1, 3:ncoefall)]
    colnames(vc) <- names(par)[1:ncoefall]
    rownames(vc) <- names(par)[1:ncoefall]

    new(
      "market_fit", object,
      list(
        method = method, first_stage_model = first_stage_model,
        demand_model = demand_model, supply_model = supply_model,
        par = par, vcov = vc
      )
    )
  }
)

#' Market fit coefficients
#'
#' Returns the coefficients of the fitted market model.
#' @param object A fitted model object.
#' @return A named vector of estimated model coefficients.
#' @name coef
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'     RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'       RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),
#'   correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+6))
#' )
#'
#' # access the estimated coefficients
#' coef(fit)
#' coefficients(fit)
#' }
#' @export
NULL

#' @describeIn coef Estimated coefficients.
#' @export
setMethod(
  "coef", signature(object = "market_fit"),
  function(object) market_fit_coefficients(object)
)

#' @describeIn coef Estimated coefficients alias.
#' @export
setMethod(
  "coefficients", signature(object = "market_fit"),
  function(object) market_fit_coefficients(object)
)

#' @rdname nobs
#' @export
setMethod(
  "nobs", signature(object = "market_fit"),
  function(object) {
    nobs(object@model)
  }
)

#' @rdname ncoef
#' @export
setMethod(
  "ncoef", signature(object = "market_fit"),
  function(object) {
    ncoef(object@model)
  }
)

#' Variance-covariance matrix for a fitted market model
#'
#' Returns the variance-covariance matrix of the estimated coefficients for
#' the fitted model. Specializes the \code{\link[stats]{vcov}} function for
#' fitted market models.
#' @param object A fitted model object.
#' @return A matrix of covariances for the estimated model coefficients.
#' @rdname vcov
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'     RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'       RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),
#'   correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+6))
#' )
#'
#' # access the variance-covariance matrix
#' head(vcov(fit))
#' }
#' @export
setMethod(
  "vcov", signature(object = "market_fit"),
  function(object) {
    object@fit$vcov
  }
)


#' Log likelihood of a fitted market model
#'
#' Specializes the \code{\link[stats]{logLik}} function for the market models
#' of the package estimated with full information minimum likelihood. It
#' returns \code{NULL} for the equilibrium model estimated with two stage
#' least squares (\code{method = "2SLS"}).
#' @param object A fitted model object.
#' @param ... Additional arguments. Unused.
#' @return A \code{\link[stats]{logLik}} object.
#' @rdname logLik
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'     RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'       RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),
#'   correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+6))
#' )
#'
#' # get the log likelihood object
#' logLik(fit)
#' }
#' @method logLik market_fit
#' @export
logLik.market_fit <- function(object, ...) {
  ll <- NULL
  if (object@fit$method != "2SLS") {
    ll <- structure(object@fit$value,
      df = length(coef(object)), class = "logLik"
    )
  }
  ll
}

#' @rdname model_description
setMethod(
  "name", signature(object = "market_fit"),
  function(object) name(object@model)
)

#' @rdname model_description
setMethod(
  "describe", signature(object = "market_fit"),
  function(object) describe(object@model)
)

#' @rdname model_description
setMethod(
  "market_type", signature(object = "market_fit"),
  function(object) market_type(object@model)
)

#' @rdname logLik
#' @export
setMethod("logLik", signature(object = "market_fit"), logLik.market_fit)

#' @rdname shortage_analysis
setMethod(
  "shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) shortages(model = fit@model, parameters = coef(fit))
)

#' @rdname shortage_analysis
setMethod(
  "normalized_shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) normalized_shortages(model = fit@model, parameters = coef(fit))
)

#' @rdname shortage_analysis
setMethod(
  "relative_shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) relative_shortages(model = fit@model, parameters = coef(fit))
)

#' @rdname shortage_analysis
setMethod(
  "shortage_probabilities", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) shortage_probabilities(model = fit@model, parameters = coef(fit))
)

#' @rdname shortage_analysis
setMethod(
  "shortage_indicators", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) shortage_indicators(model = fit@model, parameters = coef(fit))
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) shortage_standard_deviation(model = fit@model, parameters = coef(fit))
)

#' @rdname marginal_effects
setMethod(
  "shortage_marginal", signature(
    fit = "market_fit",
    model = "missing", parameters = "missing"
  ),
  function(fit, variable) {
    shortage_marginal(
      model = fit@model, parameters = coef(fit), variable = variable
    )
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_probability_marginal", signature(
    fit = "market_fit",
    model = "missing", parameters = "missing"
  ),
  function(fit, variable, aggregate) {
    shortage_probability_marginal(
      model = fit@model,
      parameters = coef(fit), variable = variable,
      aggregate = aggregate
    )
  }
)

#' @rdname model_likelihoods
setMethod(
  "scores",
  signature(object = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) scores(fit@model, coef(fit))
)

#' @rdname market_aggregation
setMethod(
  "aggregate_demand",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    aggregate_demand(model = fit@model, parameters = coef(fit))
  }
)

#' @rdname market_aggregation
setMethod(
  "aggregate_supply",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    aggregate_supply(model = fit@model, parameters = coef(fit))
  }
)

#' @rdname market_quantities
setMethod(
  "demanded_quantities",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    demanded_quantities(model = fit@model, parameters = coef(fit))
  }
)

#' @rdname market_quantities
setMethod(
  "supplied_quantities",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    supplied_quantities(model = fit@model, parameters = coef(fit))
  }
)

#' Plots the fitted model
#'
#' Displays a graphical illustration of the passed fitted model object. The
#' function creates a scatter plot of quantity-price pairs for the records
#' corresponding to the given subject and time identifiers. Then, it plots
#' the average fitted demand and supply quantities for the same data subset
#' letting prices vary between the minimum and maximum price
#' points observed in the data subset.
#'
#' If the \code{subject} argument is missing, all subjects are used. If the
#' \code{time} argument is missing, all time points are used. The scatter
#' plot of the quantity-price data can be suppressed by setting
#' \code{show_scatter = FALSE}.
#'
#' @param x A model object.
#' @param subject A vector of subject identifiers to be used in the
#' visualization.
#' @param time A vector of time identifiers to be used in the visualization.
#' @param show_scatter Should the price-quantity scatter be plotted? By default
#' \code{TRUE}.
#' @param ... Additional parameters to be used for styling the figure.
#' Specifically \code{xlab}, \code{ylab}, and \code{main} are currently
#' handled by the function.
#' @return No return value, called for for side effects (visualization).
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'     RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'       RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),
#'   correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+6))
#' )
#'
#' # show model's illustration plot
#' plot(fit)
#' }
#' @rdname plot
#' @export
setMethod("plot", signature(x = "market_fit"), function(
  x, subject, time, show_scatter = TRUE, ...
) {
  if (missing(subject)) {
    subject <- x@model@data |>
      dplyr::distinct(!!as.symbol(x@model@subject_column)) |>
      dplyr::pull()
  }
  if (missing(time)) {
    time <- x@model@data |>
      dplyr::distinct(!!as.symbol(x@model@time_column)) |>
      dplyr::pull()
  }
  va_args <- list(...)
  if (is.null(va_args$xlab)) {
    xlab <- colnames(x@model@system@price_vector)[1]
  } else {
    xlab <- va_args$xlab
  }
  if (is.null(va_args$ylab)) {
    ylab <- quantity_variable(x@model@system@demand)
  } else {
    ylab <- va_args$ylab
  }
  if (is.null(va_args$main)) {
    main <- x@model@model_name
  } else {
    main <- va_args$main
  }
  x@model@system <- set_parameters(x@model@system, coef(x))
  indices <- x@model@data |>
    dplyr::mutate(row = row_number()) |>
    dplyr::filter(!!as.symbol(x@model@subject_column) %in% subject &
      !!as.symbol(x@model@time_column) %in% time) |>
    dplyr::pull(row)
  a <- x@model@system@demand@control_matrix[indices, ] %*% x@model@system@demand@beta
  d <- function(p) mean(c(a) + x@model@system@demand@alpha * p)
  c <- x@model@system@supply@control_matrix[indices, ] %*% x@model@system@supply@beta
  s <- function(p) mean(c(c) + x@model@system@supply@alpha * p)
  prices <- x@model@system@price_vector[indices]
  bandwidth <- 0.01
  fprices <- min(prices) * (1 - bandwidth)
  tprices <- max(prices) * (1 + bandwidth)
  quantities <- x@model@system@quantity_vector[indices]
  fquantities <- min(quantities) * (1 - bandwidth)
  tquantities <- max(quantities) * (1 + bandwidth)
  dom <- seq(from = min(fprices), to = max(tprices), length.out = 100)

  labels <- c("avg demand", "avg supply", "data")
  colors <- c("blue", "orange", "red")
  line_types <- c(3, 2, NA)
  marks <- c(NA, NA, "o")

  if (!show_scatter) {
    prices <- NULL
    quantities  <- NULL
    labels <- labels[-3]
    colors <- colors[-3]
    line_types <- line_types[-3]
    marks <- marks[-3]
  }

  plot(
    prices, quantities,
    pch = "o", col = "red",
    main = main, xlab = xlab, ylab = ylab,
    xlim = c(fprices, tprices), ylim = c(fquantities, tquantities)
  )
  lines(dom, sapply(dom, d), type = "l", lwd = 2.0, lty = 3, col = "blue")
  lines(dom, sapply(dom, s), type = "l", lwd = 2.0, lty = 2, col = "orange")
  legend("topleft",
    legend = labels, col = colors, lty = line_types, pch = marks
  )
})
