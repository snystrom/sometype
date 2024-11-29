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
            class = c("result", "condition"),
            result_enum = "error"
            )
}

result_enum <- function(x, is = c("ok", "error")) {
  # we're not match.arg'ing for speed since this is internal-only,
  # default `is` arg is just as a reminder
  attr(x, "result_enum") == is
}

#' @export
conditionMessage.result <- function(c) {
  if (!is_err(c)) {
    stop("unable to unpack conditionMessage from non-error Result (should never throw!).", call. = FALSE)
  }
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

ok <- function(x) {
  if (inherits(x, "condition") || is_err(x) || inherits(x, "try-error")) {
    stop("Cannot create Result from condition or error.")
  }
  result(x)
}

result_ok_t <- function(x) {
  attr(x, "result_ok_t", exact = TRUE)
}

result <- function(x, .err_type = "generic_result_error") {
  # TODO: polish
  if (inherits(x, "condition"))
    return(error(.err_type, message = conditionMessage(x)))

  structure(
    x,
    class = c("result"),
    result_ok_t = class(x),
    result_enum = "ok"
  )
}

is_result <- function(x) {
  inherits(x, "result")
}

is_ok <- function(x) {
  is_result(x) && result_enum(x, "ok")
}

is_err <- function(x) {
  is_result(x) && result_enum(x, "error")
}

# TODO: WARNING: UNCHECKED
result_ok_t <- function(x) {
  attr(x, "result_ok_t", exact = TRUE)
}

#' @export
unwrap.result <- function(x) {
  if (is_err(x)) {
    stop("Cannot unwrap Result<Error>", call. = FALSE)
  }

  if (is_ok(x)) {
    class(x) <- result_ok_t(x)
    attr(x, "result_enum") <- NULL
    attr(x, "result_ok_t") <- NULL
    return(x)
  }
}

#' @export
print.result <- function(x, ...) {
  if(is_ok(x)) {
    cat(paste0("Result<ok(", result_ok_t(x),")>\n"))
    print(unwrap(x))
    return(invisible(x))
  }

  if (is_err(x)) {
    cat("Result<Error>\n")
    cat(paste0("  Error<", x$error_type, ">\n"))
    cat(paste0("    '", x$error_message, "'\n"))
    # TODO: metadata printing?
    return(invisible(x))
  }
  stop("Unknown error printing Result. This should never throw.")
}

#' @export
`==.result` <- function(e1, e2) {
  if (is_err(e1) || is_err(e2)) {
    # TODO: this allows comparing Err to non-Result. Do we want that??
    # TODO: modify logic so we check if e1/e2 are results, then do identical or == if error or OK
    return(identical(e1, e2))
  }

  if (isFALSE(is_ok(e1) && is_ok(e2))) {
    stop("Cannot compare Result<Ok> to non-Result.")
  }

  unwrap(e1) == unwrap(e2)
}

#' Force expression to return a Result<T,E>
#'
#' Use in place of `TryCatch`.
#'
#' @param expr an expression
#' @param ... passed to `error()` if expr fails.
#' @export
#' @examples
#' result <- try_result({stop("oh no!")})
#' result_custom_err <- try_result({stop("oh no!")}, .err_type = "my_custom_error")
#' result_custom_err$error_type
try_result <- function(expr, ...) {
  # TODO: no unit tests
  result(
    tryCatch(expr,
            error = function(e) {
                return(e)
            }),
    ...
  )
}
