#' Default predicates defining None values.
#'
#' Values that are missing, `NULL`, or `NA` will be considered `None`.
#'
#' @return list(missing, is.null, is.na)
#' @seealso missing is.null is.na
#' @export
default_none_predicates <- function() {
  list(
    missing,
    is.null,
    is.na
  )
}

#' Coerce value to an option
#'
#' @param x a value to convert to `some()` or `none` based on a set of predicates.
#' @param .none_predicates a list of predicate functions that evaluate to
#'   TRUE/FALSE. If a predicate evalutes to `TRUE`, we return `none`, else we
#'   return `some(x)`. Default: `default_none_predicates()`
#'
#' @return an option
#' @seealso default_none_predicates
option <- function(x, .none_predicates = default_none_predicates()) {
  stopifnot(".none_predicates must be a list" = is.list(.none_predicates))

  if (is.null(.none_predicates)) {
    stop(".none_predicates cannot be NULL. Use `list(is.null)` instead.", call. = FALSE)
  }

  # TODO: this doesn't work cause of lazy eval on .none_predicates...  It's
  # problematic to not check at this level because otherwise if we check in the
  # loop, a .none_predicate may be invalid and we would never catch it if we
  # never evaluate it before returning...
  #is_fun <- vapply(eval(.none_predicates), is.function, logical(length(.none_predicates)))
  #stopifnot("all none_predicates must be functions" = all(is_fun))

  for (.p in .none_predicates) {
    result <- .p(x)
    if (!is.logical(result) || length(result) > 1) {
      stop("Predicate functions must return TRUE/FALSE.", call. = FALSE)
    }
    if (result) {
      return(none)
    }
  }

  some(x)
}

#' Fetch the option_t of the Some variant of an Option
#' (means you can check type with out unwrapping)
#' @noRd
option_t <- function(x) {
  UseMethod("option_t")
}

#' @export
option_t.option <- function(x) {
  stopifnot("Cannot check Option<T> of None" = !is_none(x))
  attr(x, "option_t", exact = TRUE)
}

#' @export
some <- function(x) {
  stopifnot("Cannot create Some from None" = !is_none(x))

  if (is_some(x)) {
    return(x)
  }

  structure(x,
            class = "option",
            enum = "some",
            option_t = class(x)
            )
}

# TODO: IDK that I like this impl, but let's jam for now
# Another option could be structure("None")
# which has the added benefit of working with glue:: et. al.
#' @export
none <- structure("None",
                  class = "option",
                  enum = "none")
#lockBinding('none', .GlobalEnv)


#' @export
print.option <- function(x, ...) {
  if (is_some(x)) {
    cat(paste0("some(", option_t(x), ")\n"))
    print(unwrap(x))
  }

  if (is_none(x)) {
    cat("None\n")
  }

  return(invisible(x))
}

#' @export
is_option <- function(x) {
  inherits(x, "option")
}

option_enum <- function(x, is = c("some", "none")) {
  # we're not match.arg'ing for speed since this is internal-only,
  # default `is` arg is just as a reminder
  attr(x, "enum") == is
}

#' @export
is_some <- function(x) {
  is_option(x) && option_enum(x, "some")
}

#' @export
is_none <- function(x) {
  is_option(x) && option_enum(x, "none")
}

#' Extract a contained value in an option or error
#' @param x a value. If `some()` will extract the contained value. If `none`
#'   will crash.
#' @return the unwrapped value in some(x)
#' @export
unwrap <- function(x) {
  if (is_none(x)) {
    stop("Cannot unwrap, got None", call.=FALSE)
  }

  if (is_some(x)) {
    class(x) <- attr(x, "option_t", exact = TRUE)
    attr(x, "enum") <- NULL
    attr(x, "option_t") <- NULL
    return(x)
  }

  if (!is_some(x)) {
    # TODO: do we panic?
    stop("Cannot unwrap raw value", call. = FALSE)
  }
}

#' Unwrap or return a default value
#' @param x a value
#' @param .default the default to return
unwrap_or <- function(x, .default) {
  if (is_some(x)) {
    return(unwrap(x))
  }
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
#' @export
unwrap_or_else <- function(x, .fn) {
  stopifnot(".fn must be a function" = is.function(.fn))
  if (is_some(x)) {
    return(unwrap(x))
  }
  .fn()
}

#' Unwrap an option or error with message
#' @param x a value
#' @param msg a custom error message. This should describe the reason you expect
#'   the option to be `some`.
#' @return the unwrapped value in some(x)
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
  if (is_none(x) || !is_some(x)) {
    stop(msg, call. = FALSE)
  }
  unwrap(x)
}

#' @export
`==.option` <- function(e1, e2) {
  if (is_none(e1) || is_none(e2)) {
    return(is_none(e1) && is_none(e2))
  }

  unwrap(e1) == unwrap(e2)
}
