#' @include market_model.R

#' @describeIn market_models Base class for disequilibrium models
setClass(
  "disequilibrium_model",
  contains = "market_model",
  representation(),
  prototype()
)

setMethod(
  "initialize", "disequilibrium_model",
  function(.Object,
           model_name, verbose,
           specification,
           correlated_shocks,
           data,
           system_initializer) {
    .Object <- callNextMethod(
      .Object,
      model_name, verbose,
      specification,
      correlated_shocks,
      data,
      system_initializer
    )
    .Object@market_type <- "Disequilibrium"

    .Object
  }
)
