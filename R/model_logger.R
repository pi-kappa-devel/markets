#' Logger class
#'
#' @slot verbosity Controls the intensity of output messages. Errors are always printed.
#' Other than this, a value of
#' \describe{
#'  \item{1}{prints warnings,}
#'  \item{2}{prints basic information,}
#'  \item{3}{prints verbose information and,}
#'  \item{4}{prints debug information.}
#' }
#' @keywords internal
setClass(
  "model_logger",
  representation(
    verbosity = "numeric",
    attribute_print_mask = "character"
  ),
  prototype(
    verbosity = 0
  )
)

setMethod(
  "initialize", "model_logger",
  function(.Object, verbosity) {
    .Object@verbosity <- verbosity
    .Object
  }
)

setGeneric("print_error", function(object, ...) {
  standardGeneric("print_error")
})

setGeneric("print_warning", function(object, ...) {
  standardGeneric("print_warning")
})

setGeneric("print_info", function(object, ...) {
  standardGeneric("print_info")
})

setGeneric("print_verbose", function(object, ...) {
  standardGeneric("print_verbose")
})

setGeneric("print_debug", function(object, ...) {
  standardGeneric("print_debug")
})

setMethod("print_error", signature(object = "model_logger"), function(object, ...) {
  stop(...)
  object
})

setMethod("print_warning", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 0) {
    cat(strwrap(paste0("Warning: ", ...), exdent = 2), fill = TRUE, sep = "")
  }
  object
})

setMethod("print_info", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 1) {
    cat(strwrap(paste0("Info: ", ...), exdent = 2), fill = TRUE, sep = "")
  }
  object
})

setMethod("print_verbose", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 2) {
    cat(strwrap(paste0("Verbose: ", ...), exdent = 2), fill = TRUE, sep = "")
  }
  object
})

setMethod("print_debug", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 3) {
    cat(strwrap(paste0("Debug: ", ...), exdent = 2), fill = TRUE, sep = "")
  }
  object
})
