#' Error type
#' @param type an error type
#' @param message an error message associated with the type
#' @param ... passed as metadata on the error
#' @examples
#' e <- error('render_error', 'unable to render Rmd')
#' \dontrun{
#' stop(e)
#' }
error <- function(type, message, ...) {
  # Weird impl, but going ahead for now. May want to use real error types / rlang conditions,
  # but let's see how far we get with a simple list to test the api
  if (missing(type)) {
    type <- "generic_result_error"
  }
  if (missing(message)) {
    message <- type
  }
  stopifnot(is.character(type),
            is.character(message),
            length(type) == 1,
            length(message) == 1
            )
  structure(list(error_type = type,
                 error_message = message,
                 # TODO: this is maybe stupid
                 error_metadata = list(...)),
            # TODO: incomplete condition impl, see stop() source for accessors, etc.
            class = c("result_error", "condition")
            )
}

#' @export
conditionMessage.result_error <- function(c) {
  c$error_message
}
#' @export
conditionCall.result_error <- function(c) {
  NULL #TODO: undefined
}

#' @export
signalCondition.result_error <- function(cond) {
  signal(cond)
}

#' @export
print.result_error <- function(x, ...) {
  cat("Result<Error>\n")
  cat(paste0("  Error<", x$error_type, ">\n"))
  cat(paste0("    '", x$error_message, "'\n"))
  # TODO: metadata printing?
}

#
#result <- function(x) {
#}

