#' Prevent coercion of options to base classes
#' @noRd
block_result_as_type <- function(x, typestr, ...) {
  if (is_ok(x)) {
    stop("Cannot convert Result<Ok> to ", typestr, ".", call. = FALSE)
  }

  if (is_err(x)) {
    stop("Cannot convert Result<Error> to ", typestr, ".", call. = FALSE)
  }

  stop("Error coercing Result. This should never throw.", call. = FALSE)
}

#' @export
as.numeric.result <- function(x) {
  block_result_as_type(x, "numeric")
}

#' @export
as.integer.result <- function(x) {
  block_result_as_type(x, "integer")
}

#' @export
as.complex.result <- function(x) {
  block_result_as_type(x, "complex")
}

#' @export
as.character.result <- function(x) {
  block_result_as_type(x, "character")
}

#' @export
as.logical.result <- function(x) {
  block_result_as_type(x, "logical")
}

#' @export
as.raw.result <- function(x) {
  block_result_as_type(x, "raw")
}

#' @export
as.double.result <- function(x) {
  block_result_as_type(x, "double")
}

#' @export
as.vector.result <- function(x, mode = "any") {
  block_result_as_type(x, mode)
}

#' @export
as.matrix.result <- function(x, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL) {
  block_result_as_type(x, "matrix", nrow, ncol, byrow, dimnames)
}

#' @export
as.data.frame.result <- function(x, ...) {
  block_result_as_type(x, "data.frame", ...)
}

#' @export
as.list.result <- function(x) {
  block_result_as_type(x, "list")
}

#' @export
as.environment.result <- function(x) {
  block_result_as_type(x, "environment")
}

#' @export
as.function.result <- function(x) {
  block_result_as_type(x, "function")
}

#' @export
as.call.result <- function(x) {
  block_result_as_type(x, "call")
}

#' @export
as.expression.result <- function(x) {
  block_result_as_type(x, "expression")
}

#' @export
as.symbol.result <- function(x) {
  block_result_as_type(x, "symbol")
}

#' @export
as.name.result <- function(x) {
  block_result_as_type(x, "name")
}

#' @export
as.pairlist.result <- function(x) {
  block_result_as_type(x, "pairlist")
}

#' @export
as.language.result <- function(x) {
  block_result_as_type(x, "language")
}

#' @export
as.null.result <- function(x) {
  block_result_as_type(x, "NULL")
}
