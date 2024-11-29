#' Prevent use of R operators with options
#' @noRd
block_operator_result <- function(e1, e2, opstr, ...) {
  if (is_ok(e1) || is_ok(e2)) {
    stop("Cannot use ", opstr, " on Result<Ok>", call. = FALSE)
  }

  if (is_err(e1) || is_err(e2)) {
    stop("Cannot use ", opstr, " on Result<Error>", call. = FALSE)
  }

  stop("Error coercing Result. This should never throw.", call. = FALSE)
}

#' @export
`+.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "+")
}

#' @export
`-.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "-")
}

#' @export
`*.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "*")
}

#' @export
`/.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "/")
}

#' @export
`^.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "^")
}

#' @export
`<.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "<")
}

#' @export
`>.result` <- function(e1, e2) {
  block_operator_result(e1, e2, ">")
}

#' @export
`<=.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "<=")
}

#' @export
`>=.result` <- function(e1, e2) {
  block_operator_result(e1, e2, ">=")
}

#' @export
`&.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "&")
}

#' @export
`|.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "|")
}


#' @export
`&&.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "&&")
}

#' @export
`||.result` <- function(e1, e2) {
  block_operator_result(e1, e2, "||")
}

#' @export
`[.result` <- function(x, i) {
  block_operator_result(x, i, "[")
}

#' @export
`[[.result` <- function(x, i) {
  block_operator_result(x, i, "[[")
}
