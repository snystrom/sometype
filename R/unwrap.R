#' @export
unwrap <- function(x) {
  UseMethod("unwrap")
}

#' @export
unwrap.default <- function(x) {
  stop("Cannot unwrap raw value", call. = FALSE)
}

#' Extract a contained value in an option or error
#' @param x a value. If `some()` will extract the contained value. If `none`
#'   will crash.
#' @return the unwrapped value in some(x)
#' @export
unwrap.option <- function(x) {
  if (is_none(x)) {
    stop("Cannot unwrap, got None", call.=FALSE)
  }

  if (is_some(x)) {
    class(x) <- attr(x, "option_t", exact = TRUE)
    attr(x, "enum") <- NULL
    attr(x, "option_t") <- NULL
    return(x)
  }

  stop("Error unwrapping unknown option. Should never throw.", call. = FALSE)
}

#' Extract the Ok value in a result or error
#' @param x a value. If `ok()` will extract the contained value. If `error`
#'   will crash.
#' @return the unwrapped value in ok(x)
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

#' Unwrap or return a default value
#' @param x a value
#' @param .default the default to return
#' @export
unwrap_or <- function(x, .default) {
  UseMethod("unwrap_or")
}

#' @export
unwrap_or.option <- function(x, .default) {
  if (is_some(x)) {
    return(unwrap(x))
  }
  return(.default)
}

#' @export
unwrap_or.result <- function(x, .default) {
  if (is_ok(x)) {
    return(unwrap(x))
  }
  return(.default)
}

#' @export
unwrap_or.default <- function(x, .default) {
  return(.default)
}

#' Unwrap or return a function result
#'
#' @param x a value
#' @param .fn the function to evaluate
#' @return unwrap(x) or the return of `.fn()`
#' @examples
#' unwrap_or_else(none, function() {"whoops!"})
#' unwrap_or_else(some(5), function() {"whoops!"})
#'
#' unwrap_or_else(ok(5), function() {"whoops!"})
#' unwrap_or_else(error(), function() {"whoops!"})
#' @export
unwrap_or_else <- function(x, .fn) {
  UseMethod("unwrap_or_else")
}

#' @export
unwrap_or_else.option <- function(x, .fn) {
  stopifnot(".fn must be a function" = is.function(.fn))
  if (is_some(x)) {
    return(unwrap(x))
  }
  .fn()
}

#' @export
unwrap_or_else.result <- function(x, .fn) {
  stopifnot(".fn must be a function" = is.function(.fn))
  if (is_ok(x)) {
    return(unwrap(x))
  }
  .fn()
}

#' @export
unwrap_or_else.default <- function(x, .fn) {
  .fn()
}

#' Unwrap an option or error with message
#' @param x a value
#' @param msg a custom error message. This should describe the reason you expect
#'   the option to be `some`, or the result to be `ok`.
#' @return the unwrapped value in some(x) or ok(x).
#' @examples
#' is_five <- some(5)
#' five <- expect(is_five, "This variable hold the number 5.")
#' \dontrun{
#' # A function we think should always return some()
#' # but for some mysterious reason, it does not.
#' should_always_work <- function(x) {none}
#
#' # This errors!
#' expect(should_always_work(1), "This shouldn't be none")
#' }
#' @export
expect <- function(x, msg) {
  UseMethod("expect")
}

#' @export
expect.option <- function(x, msg) {
  if (is_none(x) || !is_some(x)) {
    stop(msg, call. = FALSE)
  }
  unwrap(x)
}

#' @export
expect.result <- function(x, msg) {
  if (is_err(x) || !is_ok(x)) {
    stop(msg, call. = FALSE)
  }
  unwrap(x)
}

#' @export
expect.default <- function(x, msg) {
  stop(msg)
}
