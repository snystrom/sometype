#' Prevent use of R operators with options
#' @noRd
block_operator <- function(e1, e2, opstr, ...) {
  if (is_some(e1) || is_some(e2)) {
    stop("Cannot use ", opstr, " on Option<Some>", call. = FALSE)
  }

  if (is_none(e1) || is_none(e2)) {
    stop("Cannot use ", opstr, " on Option<None>", call. = FALSE)
  }

  stop("Error coercing Option. This should never throw.", call. = FALSE)
}

#' @export
`+.option` <- function(e1, e2) {
  block_operator(e1, e2, "+")
}

#' @export
`-.option` <- function(e1, e2) {
  block_operator(e1, e2, "-")
}

#' @export
`*.option` <- function(e1, e2) {
  block_operator(e1, e2, "*")
}

#' @export
`/.option` <- function(e1, e2) {
  block_operator(e1, e2, "/")
}

#' @export
`^.option` <- function(e1, e2) {
  block_operator(e1, e2, "^")
}

#' @export
`<.option` <- function(e1, e2) {
  block_operator(e1, e2, "<")
}

#' @export
`>.option` <- function(e1, e2) {
  block_operator(e1, e2, ">")
}

#' @export
`<=.option` <- function(e1, e2) {
  block_operator(e1, e2, "<=")
}

#' @export
`>=.option` <- function(e1, e2) {
  block_operator(e1, e2, ">=")
}

#' @export
`&.option` <- function(e1, e2) {
  block_operator(e1, e2, "&")
}

#' @export
`|.option` <- function(e1, e2) {
  block_operator(e1, e2, "|")
}


#' @export
`&&.option` <- function(e1, e2) {
  block_operator(e1, e2, "&&")
}

#' @export
`||.option` <- function(e1, e2) {
  block_operator(e1, e2, "||")
}

#' @export
`[.option` <- function(x, i) {
  block_operator(x, i, "[")
}

#' @export
`[[.option` <- function(x, i) {
  block_operator(x, i, "[[")
}
