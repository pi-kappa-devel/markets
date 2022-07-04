context("Equilibrium Model's Tests\n")

# Estimation setup
parameters <- list(
  nobs = 1000, tobs = 3,
  alpha_d = -1.7, beta_d0 = 14.9, beta_d = c(2.3, -1.2), eta_d = c(-1.3, -1.1),
  alpha_s = 1.6, beta_s0 = 10.2, beta_s = c(-1.3), eta_s = c(2.5, 2.2),
  sigma_d = 1.0, sigma_s = 1.0, rho_ds = 0.0
)

# Optimization setup
reltol <- 1e-8
optimization_method <- "BFGS"
optimization_options <- list(REPORT = 10, maxit = 50000, reltol = reltol)

# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("equilibrium_model", parameters)
  expect_is(mdl, "equilibrium_model")
})

est <- NULL
test_that(paste0(name(mdl), " can be estimated"), {
  est <<- equilibrium_model(
    formula(mdl), simulated_data,
    estimation_options = list(
      control = optimization_options, method = optimization_method
    )
  )
  expect_is(est@fit, "list")
})

test_that(paste0(
  name(mdl), " can be estimated using formulas",
  " with transformations"
), {
  est <- equilibrium_model(
    formula(update(
      Formula(formula(mdl)),
      . | log(5 + .) | . | . ~ . - P + log(5 + P) | . - P + log(5 + P)
    )),
    simulated_data,
    estimation_options = list(
      control = optimization_options, method = optimization_method,
      standard_errors = c("id")
    )
  )

  expect_is(est@fit, "list")
})

test_that(paste0(name(mdl), " fit can be summarized"), {
  test_summary(est, 41)
})

test_that(paste0(
  "Maximum likelihood estimates of '", name(mdl),
  "' are accurate"
), {
  test_estimation_accuracy(coef(est), unlist(parameters[-c(1, 2)]), 1e-0)
})


test_that(paste0("Aggregation can be calculated"), {
  test_aggregation(aggregate_demand, est)
  test_aggregation(aggregate_supply, est)
})

test_that(paste0("Scores can be calculated"), {
  test_scores(est)
})

reg <- NULL
test_that(paste0("First stage of '", name(mdl), "' can be estimated"), {
  reg <<- estimate(mdl, method = "2SLS")
  expect_is(reg@fit$first_stage_model, "lm")
})

test_that(paste0("Second stage of '", name(mdl), "' can be estimated"), {
  expect_is(reg@fit$demand_model, "lm")
  expect_is(reg@fit$supply_model, "lm")
})

test_that(paste0(name(mdl), " regressions can be summarized"), {
  test_summary(reg, 88)
})

test_that(paste0(
  "Two-stage least squares estimates of '", name(mdl),
  "' are accurate"
), {
  test_estimation_accuracy(coef(reg), unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0("Optimization of '", name(mdl), "' using GSL succeeds"), {
  mll <<- maximize_log_likelihood(mdl,
    start = NULL, step = 1e-2,
    objective_tolerance = 1e-4,
    gradient_tolerance = 1e-3,
    max_it = 1e+3
  )
  testthat::expect_length(mll, 8)
})

test_that(paste0(
  "Calculated gradient of '", name(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, coef(est), 1e-2)
})
