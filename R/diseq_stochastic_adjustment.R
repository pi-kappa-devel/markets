#' @include disequilibrium_model.R

#' @rdname market_models
#' @description
#' \subsection{Disequilibrium model with stochastic price dynamics
#' (\code{diseq_stochastic_adjustment})}{
#' The disequilibrium model with stochastic price adjustment is described
#' by a system of four equations. Three of of them form a stochastic linear system of
#' market equations equations coupled with a stochastic price evolution equation. The
#' fourth equation is the short side rule. In contrast to the deterministic counterpart,
#' the model does not impose any separation rule on the sample. It is estimated using
#' full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\},}
#' \deqn{\Delta P_{nt} =
#'     \frac{1}{\gamma} \left( D_{nt} - S_{nt} \right) +  X_{p,nt}'\beta_{p} + u_{p,nt}.}
#' }
#' @export
setClass(
  "diseq_stochastic_adjustment",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn model_initialization Disequilibrium model with stochastic price
#'   adjustment constructor
#' @examples
#' simulated_data <- simulate_data(
#'   # model type, observed entities and time points
#'   "diseq_stochastic_adjustment", 500, 3,
#'   # demand coefficients
#'   -0.1, 9.8, c(0.3, -0.2), c(0.6, 0.1),
#'   # supply coefficients
#'   0.1, 7.1, c(0.9), c(-0.5, 0.2),
#'   # price adjustment coefficient
#'   1.4, 3.1, c(0.8)
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_stochastic_adjustment", # model type
#'   subject = id, time = date, quantity = Q, price = P,
#'   demand = P + Xd1 + Xd2 + X1 + X2, supply = P + Xs1 + X1 + X2,
#'   price_dynamics = Xp1,
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' show(model)
setMethod(
  "initialize", "diseq_stochastic_adjustment",
  function(.Object,
           quantity, price, demand, supply, price_dynamics, subject, time,
           data, correlated_shocks = TRUE, verbose = 0) {
    specification <- make_specification(
      substitute(quantity), substitute(price),
      substitute(demand), substitute(supply), substitute(subject), substitute(time),
      substitute(price_dynamics)
    )
    .Object <- callNextMethod(
      .Object, "Stochastic Adjustment", verbose,
      specification, correlated_shocks, data,
      function(...) new("system_stochastic_adjustment", ...)
    )

    .Object
  }
)

#' @describeIn single_call_estimation Disequilibrium model with stochastic
#' price adjustments.
#' @export
setGeneric(
  "diseq_stochastic_adjustment",
  function(specification, data,
           correlated_shocks = TRUE, verbose = 0,
           estimation_options = list()) {
    standardGeneric("diseq_stochastic_adjustment")
  }
)

#' @rdname single_call_estimation
setMethod(
  "diseq_stochastic_adjustment", signature(specification = "formula"),
  function(specification, data, correlated_shocks, verbose,
           estimation_options) {
    initialize_and_estimate(
      "diseq_stochastic_adjustment", specification, data,
      correlated_shocks, verbose, estimation_options
    )
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(
    model = "diseq_stochastic_adjustment", fit = "missing"
  ),
  function(model, parameters) {
    model@system <- set_parameters(model@system, parameters)
    result <- sqrt(
      model@system@demand@var + model@system@supply@var -
        2 * model@system@demand@sigma * model@system@supply@sigma * model@system@rho_ds
    )
    names(result) <- "shortage_standard_deviation"
    result
  }
)

setMethod(
  "calculate_initializing_values", signature(object = "diseq_stochastic_adjustment"),
  function(object) {
    start <- callNextMethod(object)

    lhs <- object@data[, price_differences_variable(object@system)]
    rhs <- cbind(
      object@system@quantity_vector,
      object@system@price_equation@independent_matrix
    )
    plm <- stats::lm(lhs ~ rhs - 1)
    gamma <- 1 / abs(plm$coefficients[1])
    coefficients <- plm$coefficients[-c(1)] / plm$coefficients[1]
    names(coefficients) <- colnames(
      object@system@price_equation@independent_matrix
    )

    len <- length(start)
    pos <- len - ifelse(object@system@correlated_shocks, 3, 2)
    start <- c(start[1:(pos - 1)], gamma, coefficients, start[(pos + 1):len])

    len <- length(start)
    if (object@system@correlated_shocks) {
      start <- c(start[1:(len - 1)], 1, start[len], 0, 0)
      names(start)[len:length(start)] <- c(
        prefixed_variance_variable(object@system@price_equation),
        paste0(correlation_variable(object@system), c("_DS", "_DP", "_SP"))
      )
    } else {
      start <- c(start, price_variance = 1)
      names(start)[len + 1] <- prefixed_variance_variable(object@system@price_equation)
    }

    start
  }
)

#' @rdname model_likelihoods
setMethod(
  "log_likelihood", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    sum(calculate_system_loglikelihood(object@system))
  }
)

#' @rdname model_likelihoods
setMethod(
  "gradient", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    colSums(calculate_system_scores(object@system))
  }
)

#' @rdname model_likelihoods
setMethod(
  "scores", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    calculate_system_scores(object@system)
  }
)

setMethod(
  "calculate_initializing_values",
  signature(object = "diseq_stochastic_adjustment"),
  function(object) {
    demand <- stats::lm(
      object@system@demand@dependent_vector ~
      object@system@demand@independent_matrix - 1
    )
    names(demand$coefficients) <- colnames(
      object@system@demand@independent_matrix
    )
    var_d <- var(demand$residuals)
    names(var_d) <- prefixed_variance_variable(object@system@demand)

    supply <- stats::lm(
      object@system@supply@dependent_vector ~
      object@system@supply@independent_matrix - 1
    )
    names(supply$coefficients) <- colnames(
      object@system@supply@independent_matrix
    )
    var_s <- var(supply$residuals)
    names(var_s) <- prefixed_variance_variable(object@system@supply)

    dp <- object@data[, price_differences_variable(object@system)]
    xd <- demand$fitted.values - supply$fitted.values
    rhs <- cbind(xd, object@system@price_equation@independent_matrix)
    prices <- stats::lm(dp ~ rhs - 1)
    names(prices$coefficients) <- c(
      price_differences_variable(object@system),
      colnames(object@system@price_equation@independent_matrix)
    )
    var_p <- var(prices$residuals)
    names(var_p) <- prefixed_variance_variable(object@system@price_equation)

    start <- c(
      demand$coefficients, supply$coefficients,
      prices$coefficients, var_d, var_s, var_p
    )

    if (object@system@correlated_shocks) {
      rho_ds <- 0.0
      names(rho_ds) <- paste0(correlation_variable(object@system), "_DS")
      rho_dp <- 0.0
      names(rho_dp) <- paste0(correlation_variable(object@system), "_DP")
      rho_sp <- 0.0
      names(rho_sp) <- paste0(correlation_variable(object@system), "_SP")

      start <- c(start, rho_ds, rho_dp, rho_sp)
    }

    start
  }
)
