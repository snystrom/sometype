#' Prevent coercion of options to base classes
#' @noRd
block_as_type <- function(x, typestr, ...) {
  if (is_some(x)) {
    stop("Cannot convert Option<Some> to ", typestr, ".", call. = FALSE)
  }

  if (is_none(x)) {
    stop("Cannot convert Option<None> to ", typestr, ".", call. = FALSE)
  }

  stop("Error coercing Option. This should never throw.", call. = FALSE)
}

#' @export
as.numeric.option <- function(x) {
  block_as_type(x, "numeric")
}

#' @export
as.integer.option <- function(x) {
  block_as_type(x, "integer")
}

#' @export
as.complex.option <- function(x) {
  block_as_type(x, "complex")
}

#' @export
as.character.option <- function(x) {
  block_as_type(x, "character")
}

#' @export
as.logical.option <- function(x) {
  block_as_type(x, "logical")
}

#' @export
as.raw.option <- function(x) {
  block_as_type(x, "raw")
}

#' @export
as.double.option <- function(x) {
  block_as_type(x, "double")
}

#' @export
as.vector.option <- function(x, mode = "any") {
  block_as_type(x, mode)
}

#' @export
as.matrix.option <- function(x, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
  block_as_type(x, "matrix", nrow, ncol, byrow, dimnames)
}

#' @export
as.data.frame.option <- function(x, ...) {
  block_as_type(x, "data.frame", ...)
}

#' @export
as.list.option <- function(x) {
  block_as_type(x, "list")
}

#' @export
as.environment.option <- function(x) {
  block_as_type(x, "environment")
}

#' @export
as.function.option <- function(x) {
  block_as_type(x, "function")
}

#' @export
as.call.option <- function(x) {
  block_as_type(x, "call")
}

#' @export
as.expression.option <- function(x) {
  block_as_type(x, "expression")
}

#' @export
as.symbol.option <- function(x) {
  block_as_type(x, "symbol")
}

#' @export
as.name.option <- function(x) {
  block_as_type(x, "name")
}

#' @export
as.pairlist.option <- function(x) {
  block_as_type(x, "pairlist")
}

#' @export
as.language.option <- function(x) {
  block_as_type(x, "language")
}

#' @export
as.null.option <- function(x) {
  block_as_type(x, "NULL")
}
