option <- function(x) {
  if (missing(x))
    return(none)

  if (is.null(x))
    return(none)

  some(x)
}

#' @export
some <- function(x) {
  stopifnot("Cannot create Some from None" = !is_none(x))
  structure(x,
            class = "option",
            enum = "some",
            option_t = class(x)
            )
}

# TODO: IDK that I like this impl, but let's jam for now
#
#' @export
none <- structure(list(),
                  class = "option",
                  enum = "none")

#' @export
print.option <- function(x, ...) {
  if (is_some(x)) {
    # TODO: this is not great for big stuff...
    cat(paste0("some(", class(unwrap(x)), ")\n"))
  }

  if (is_none(x)) {
    cat("None\n")
  }

  return(invisible(x))
}

is_option <- function(x) {
  inherits(x, "option")
}

option_enum <- function(x, is = c("some", "none")) {
  # we're not match.arg'ing for speed
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

#' @export
`==.option` <- function(e1, e2) {
  if (is_none(e1) || is_none(e2)) {
    return(is_none(e1) && is_none(e2))
  }

  unwrap(e1) == unwrap(e2)
}

