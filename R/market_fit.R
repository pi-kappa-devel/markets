#' @include equilibrium_model.R
#' @include diseq_basic.R
#' @include diseq_directional.R
#' @include diseq_deterministic_adjustment.R
#' @include diseq_stochastic_adjustment.R


#' @title Market model Fit
#'
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
#' # access an inherited method from the underlying model
#' aggregate_demand(fit)
#'
#' # summary of results
#' summary(fit)
#' @name market_fits
#' @seealso \code{\linkS4class{market_model}}
NULL

#' @describeIn market_fits Fit class for market models
#'
#' @description
#' This is the estimation output class for all market models of the package. It couples
#' a market model object with estimation results. It provides a common user interface
#' for accessing estimation results, irrespective of the underlying market model used.
#' The estimation results are intended to be accessed by passing \code{market_fit}
#' objects to methods such as \code{\link{plot}}, \code{\link{summary}}, and
#' \code{\link{logLik}}.
#' @details
#' The \code{market_fit} class derives from the \code{\linkS4class{market_model}}
#' class. Thus, all the public functionality of the underlying market model is also
#' directly accessible from the output class.
#'
#' Furthermore, the class is responsible for harmonizing the heterogeneous outputs
#' resulting from different estimation methods of market models. For example, a
#' \code{2SLS} estimation of the \code{\linkS4class{equilibrium_model}} gives a
#' \code{\link[systemfit]{systemfit}} object, while the maximum likelihood estimation
#' of \code{\linkS4class{diseq_basic}} returns an \code{\link[stats]{optim}} list. In
#' both cases, the \code{market_fit} stores the estimation output in the member
#' \code{fit} of type \code{list}. Methods of the class examine the type of the
#' \code{fit} and direct execution accordingly to different branches to produce
#' a unified experience for the caller.
#' @export
setClass(
  "market_fit",
  contains = "market_model",
  representation(
    fit = "list"
  ),
  prototype()
)

setMethod(
  "initialize", "market_fit",
  function(.Object, market_model, estimate) {
    for (slot_name in slotNames(market_model)) {
      slot(.Object, slot_name) <- slot(market_model, slot_name)
    }
    .Object@fit <- estimate

    .Object
  }
)

#' @describeIn summaries Summarizes the model's fit.
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
#' For a linear estimation of the equilibrium system, the function prints the
#' estimation summary provided by \code{\link[systemfit]{systemfit}} in
#' addition to the model's \code{summary} output.
#' @return No return value, called for for side effects (print summary).
#' @export
setMethod("summary", signature(object = "market_fit"), function(object) {
  (selectMethod("summary", "market_model"))(object)
  if (object@fit$method != "2SLS") {
    args <- object@fit$call[[2]]

    cat("\nMaximum likelihood estimation:", sep = "", fill = TRUE)
    cat(
      labels = sprintf("  %-20s:", "Method"), args$method,
      sep = "", fill = TRUE
    )
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
    cat(
      labels = sprintf("  %-20s:", "Convergence Status"),
      ifelse(!object@fit$convergence, "success", "failure"),
      sep = "", fill = TRUE
    )
    cat(sprintf("  %-20s:", "Starting Values"), sep = "", fill = TRUE)
    print(object@fit$start, digits = 4)
    cat("\nCoefficients:", sep = "", fill = TRUE)
    means <- object@fit$par
    sds <- sqrt(diag(object@fit$vcov))
    zvals <- means / sds
    pvals <- 2 * pnorm(abs(zvals), lower.tail = FALSE)
    r <- cbind(
      Estimate = means, `Std. Error` = sds,
      `z value` = zvals, `Pr(z)` = pvals
    )
    print(r, digits = 4)
    cat(
      labels = sprintf("\n%s:", "-2 log L"),
      -2 * logLik(object),
      sep = "", fill = TRUE
    )
  } else {
    print(summary(object@fit$system_model))
  }
})


#' Model estimation.
#'
#' All models are estimated using full information maximum likelihood. The
#' \code{\linkS4class{equilibrium_model}} can also be estimated using two-stage
#' least squares. The maximum likelihood estimation is based on
#' \code{\link[stats]{optim}}. If no starting values are provided, the function uses
#' linear regression estimates as initializing values. The default optimization method is
#' BFGS. For other alternatives see \code{\link[stats]{optim}}. The implementation of
#' the two-stage least square estimation of the \code{\linkS4class{equilibrium_model}}
#' is based on \code{\link[systemfit]{systemfit}}.
#' @param object A model object.
#' @param ... Additional parameter used in the model's estimation. These are
#' passed further down to the estimation call. For the
#' \code{\linkS4class{equilibrium_model}} model, the parameters are passed to
#' \code{\link[systemfit]{systemfit}}, if the method is set to
#' \code{2SLS}, or to \code{\link[stats]{optim}} for any other method. For the rest of
#' the models, the parameters are passed to \code{\link[stats]{optim}}.
#' @return The object that holds the estimation result.
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
#' \code{"calculated"} Hessian for the model that expressions are
#' available and the \code{"numerical"} Hessian in other cases. Calculated
#' Hessian expressions are available for the basic and directional models.
#' @param standard_errors One of three potential options:
#' \code{"homoscedastic"}, \code{"heteroscedastic"}, or a vector with
#' variables names for which standard error clusters are to be created. The
#' default value is \code{"homoscedastic"}. If the option
#' \code{"heteroscedastic"} is passed, the variance-covariance matrix is
#' calculated using heteroscedasticity adjusted (Huber-White) standard errors.
#' If the vector is supplied, the variance-covariance matrix is calculated by
#' grouping the score matrix based on the passed variables.
setMethod(
  "estimate", signature(object = "market_model"),
  function(object, gradient = "calculated", hessian = "calculated",
           standard_errors = "homoscedastic", ...) {
    validate_gradient_option(object, gradient)
    validate_hessian_option(object, hessian)
    validate_standard_error_option(object, standard_errors)

    va_args <- list(...)

    if (hessian == "skip" ||
      ((object@model_type_string %in% c("Basic", "Directional")) &&
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

    va_args$fn <- function(...) minus_log_likelihood(object, ...)
    if (gradient == "calculated") {
      va_args$gr <- function(...) gradient(object, ...)
    }

    fit <- do.call(optim, va_args)
    fit$call <- call("optim", va_args)
    fit$start <- start
    fit$method <- va_args$method

    if (hessian == "calculated") {
      print_verbose(object@logger, "Calculating hessian and variance-covariance matrix.")
      fit$hessian <- hessian(object, fit$par)
    }

    if (length(standard_errors) == 1) {
      if (standard_errors == "heteroscedastic") {
        fit <- set_heteroscedasticity_consistent_errors(object, fit)
      } else if (standard_errors != "homoscedastic") {
        fit <- set_clustered_errors(object, fit, standard_errors)
      }
    } else {
      fit <- set_clustered_errors(object, fit, standard_errors)
    }

    if (!is.null(fit$hessian)) {
      tryCatch(
        fit$vcov <- MASS::ginv(fit$hessian),
        error = function(e) print_warning(object@logger, e$message)
      )
    }

    new("market_fit", object, fit)
  }
)

#' @describeIn estimate Equilibrium model estimation.
#' @param method A string specifying the estimation method. When the passed value is
#' among \code{Nelder-Mead}, \code{BFGS}, \code{CG}, \code{L-BFGS-B}, \code{SANN},
#' and \code{Brent}, the model is estimated using
#' full information maximum likelihood based on \code{\link[stats]{optim}} functionality.
#' When \code{2SLS} is supplied, the model is estimated using two-stage least squares
#' based on \code{\link[systemfit]{systemfit}}. In this case, the function returns a
#' list containing the first and second stage estimates. The default value is
#' \code{BFGS}.
setMethod(
  "estimate", signature(object = "equilibrium_model"),
  function(object, method = "BFGS", ...) {
    if (method != "2SLS") {
      return(callNextMethod(object, method = method, ...))
    }

    quantity_variable <- colnames(object@system@quantity_vector)
    price_variable <- colnames(object@system@price_vector)

    ## create fitted variable
    fitted_column <- paste0(price_variable, "_FITTED")

    ## estimate first stage
    first_stage_controls <- all.vars(terms(object@system@formula, lhs = 0))
    first_stage_controls <- first_stage_controls[
      first_stage_controls != price_variable
    ]
    first_stage_formula <- paste0(
      price_variable, " ~ ", paste0(first_stage_controls, collapse = " + ")
    )

    first_stage_model <- lm(first_stage_formula, object@model_tibble)
    object@model_tibble[, fitted_column] <- first_stage_model$fitted.values

    ## create demand formula
    independent <- all.vars(terms(object@system@formula, lhs = 0, rhs = 1))
    demand_formula <- formula(paste0(
      quantity_variable, " ~ ", paste0(independent, collapse = " + ")
    ))

    ## create supply formula
    independent <- all.vars(terms(object@system@formula, lhs = 0, rhs = 2))
    independent <- independent[independent != "CONST"]
    supply_formula <- formula(paste0(
      quantity_variable, " ~ ", paste0(independent, collapse = " + ")
    ))

    inst <- formula(paste0(" ~ ", paste0(first_stage_controls, collapse = " + ")))
    system_model <- systemfit::systemfit(
      list(demand = demand_formula, supply = supply_formula),
      method = "2SLS", inst = inst, data = object@model_tibble
    )

    new(
      "market_fit", object,
      list(
        method = method, first_stage_model = first_stage_model,
        system_model = system_model
      )
    )
  }
)

market_fit_coefficients <- function(object) {
  if (object@fit$method != "2SLS") {
    object@fit$par
  } else {
    demand <- object@fit$system_model$coefficients[
      grep("demand_", names(object@fit$system_model$coefficients))
    ]
    demand <- c(demand[2], demand[1], demand[-c(1, 2)])
    names(demand) <- gsub("demand_", "D_", names(demand))

    supply <- object@fit$system_model$coefficients[
      grep("supply_", names(object@fit$system_model$coefficients))
    ]
    supply <- c(supply[2], supply[1], supply[-c(1, 2)])
    names(supply) <- gsub("supply_", "S_", names(supply))

    var_d <- object@fit$system_model$residCov[[1, 1]]
    names(var_d) <- prefixed_variance_variable(object@system@demand)

    var_s <- object@fit$system_model$residCov[[2, 2]]
    names(var_s) <- prefixed_variance_variable(object@system@supply)

    coefs <- c(demand, supply, var_d, var_s)
    if (object@system@correlated_shocks) {
      rho <- object@fit$system_model$residCov[1, 2] / sqrt(var_d * var_s)
      names(rho) <- correlation_variable(object@system)
      coefs <- c(coefs, rho)
    }
    names(coefs) <- gsub("\\(Intercept\\)", "CONST", names(coefs))

    coefs
  }
}

#' Estimated coefficients of a fitted market model.
#'
#' Returns the coefficients of the fitted model.
#' @param object A fitted model object.
#' @return A vector of estimated model coefficients.
#' @rdname coef
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
setMethod("coef", signature(object = "market_fit"), market_fit_coefficients)

#' @describeIn coef Estimated coefficients alias.
#' @export
setMethod(
  "coefficients", signature(object = "market_fit"),
  market_fit_coefficients
)

#' Variance-covariance matrix for a fitted market model.
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
    if (object@fit$method != "2SLS") {
      colnames(object@fit$vcov) <- names(coef(object))
      rownames(object@fit$vcov) <- names(coef(object))
      object@fit$vcov
    } else {
      object@fit$system_model$coefCov
    }
  }
)


#' Log likelihood of a fitted market model.
#'
#' Specializes the \code{\link[stats]{logLik}} function for the market models
#' of the package estimated with full information minimum likelihood. It
#' returns \code{NULL} for the equilibrium model estimated with
#' \code{\link[systemfit]{systemfit}}.
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
    ll <- structure(-object@fit$value,
      df = length(coef(object)), class = "logLik"
    )
  }
  ll
}

#' @rdname logLik
#' @export
setMethod("logLik", signature(object = "market_fit"), logLik.market_fit)


try_coerce_market_fit <- function(object) {
  to_class <- class(object)[[1]]
  if (object@model_type_string == "Equilibrium") {
    to_class <- "equilibrium_model"
  } else if (object@model_type_string == "Basic") {
    to_class <- "diseq_basic"
  } else if (object@model_type_string == "Directional") {
    to_class <- "diseq_directional"
  } else if (object@model_type_string == "Deterministic Adjustment") {
    to_class <- "diseq_deterministic_adjustment"
  } else if (object@model_type_string == "Stochastic Adjustment") {
    to_class <- "diseq_stochastic_adjustment"
  }
  class(object)[[1]] <- to_class
  object
}

#' @rdname shortage_analysis
setMethod(
  "shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    shortages(model = try_coerce_market_fit(fit), parameters = coef(fit))
  }
)

#' @rdname shortage_analysis
setMethod(
  "normalized_shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    normalized_shortages(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname shortage_analysis
setMethod(
  "relative_shortages", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    relative_shortages(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_probabilities", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    shortage_probabilities(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_indicators", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    shortage_indicators(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(
    model = "missing", parameters = "missing",
    fit = "market_fit"
  ),
  function(fit) {
    shortage_standard_deviation(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_marginal", signature(
    fit = "market_fit",
    model = "missing", parameters = "missing"
  ),
  function(fit, variable) {
    shortage_marginal(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit), variable = variable
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
      model = try_coerce_market_fit(fit),
      parameters = coef(fit), variable = variable,
      aggregate = aggregate
    )
  }
)

#' @rdname scores
setMethod(
  "scores",
  signature(object = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) scores(try_coerce_market_fit(fit), coef(fit))
)

#' @rdname market_aggregation
setMethod(
  "aggregate_demand",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    aggregate_demand(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname market_aggregation
setMethod(
  "aggregate_supply",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    aggregate_supply(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname market_quantities
setMethod(
  "demanded_quantities",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    demanded_quantities(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' @rdname market_quantities
setMethod(
  "supplied_quantities",
  signature(model = "missing", parameters = "missing", fit = "market_fit"),
  function(fit) {
    supplied_quantities(
      model = try_coerce_market_fit(fit),
      parameters = coef(fit)
    )
  }
)

#' Plots the fitted model.
#'
#' Displays a graphical illustration of the passed fitted model object. The
#' function creates a scatter plot of quantity-price pairs for the records
#' corresponding to the given subject and time identifiers. Then, it plots
#' the average fitted demand and supply quantities for the same data subset
#' letting prices vary between the minimum and maximum price
#' points observed in the data subset.
#'
#' @param x A model object.
#' @param subject A vector of subject identifiers to be used in the
#' visualization.
#' @param time A vector of time identifiers to be used in the visualization.
#' @param ... Additional parameter to be used for styling the figure.
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
setMethod("plot", signature(x = "market_fit"), function(x, subject, time, ...) {
  if (missing(subject)) {
    subject <- x@model_tibble |>
      dplyr::distinct(!!as.symbol(x@subject_column)) |>
      dplyr::pull()
  }
  if (missing(time)) {
    time <- x@model_tibble |>
      dplyr::distinct(!!as.symbol(x@time_column)) |>
      dplyr::pull()
  }
  va_args <- list(...)
  if (is.null(va_args$xlab)) {
    xlab <- colnames(x@system@price_vector)[1]
  }
  if (is.null(va_args$ylab)) {
    ylab <- quantity_variable(x@system@demand)
  }
  if (is.null(va_args$main)) {
    main <- x@model_type_string
  }
  x@system <- set_parameters(x@system, coef(x))
  indices <- x@model_tibble |>
    dplyr::mutate(row = row_number()) |>
    dplyr::filter(!!as.symbol(x@subject_column) %in% subject &
      !!as.symbol(x@time_column) %in% time) |>
    dplyr::pull(row)
  a <- x@system@demand@control_matrix[indices, ] %*% x@system@demand@beta
  d <- function(p) mean(c(a) + x@system@demand@alpha * p)
  c <- x@system@supply@control_matrix[indices, ] %*% x@system@supply@beta
  s <- function(p) mean(c(c) + x@system@supply@alpha * p)
  prices <- x@system@price_vector[indices]
  bandwidth <- 0.01
  fprices <- min(prices) * (1 - bandwidth)
  tprices <- max(prices) * (1 + bandwidth)
  quantities <- x@system@quantity_vector[indices]
  fquantities <- min(quantities) * (1 - bandwidth)
  tquantities <- max(quantities) * (1 + bandwidth)
  dom <- seq(from = min(fprices), to = max(tprices), length.out = 100)
  plot(
    prices, quantities,
    pch = "o", col = "red",
    main = main, xlab = xlab, ylab = ylab,
    xlim = c(fprices, tprices), ylim = c(fquantities, tquantities)
  )
  lines(dom, sapply(dom, d), type = "l", lwd = 2.0, lty = 3, col = "blue")
  lines(dom, sapply(dom, s), type = "l", lwd = 2.0, lty = 2, col = "orange")
  legend("topleft",
    legend = c("avg demand", "avg supply", "data"),
    col = c("blue", "orange", "red"), lty = c(3, 2, NA),
    pch = c(NA, NA, "o")
  )
})
