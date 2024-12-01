% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unwrap.R
\name{expect}
\alias{expect}
\title{Unwrap an option or error with message}
\usage{
expect(x, msg)
}
\arguments{
\item{x}{a value}

\item{msg}{a custom error message. This should describe the reason you expect
the option to be \code{some}, or the result to be \code{ok}.}
}
\value{
the unwrapped value in some(x) or ok(x).
}
\description{
Unwrap an option or error with message
}
\examples{
is_five <- some(5)
five <- expect(is_five, "This variable hold the number 5.")
\dontrun{
# A function we think should always return some()
# but for some mysterious reason, it does not.
should_always_work <- function(x) {none}
# This errors!
expect(should_always_work(1), "This shouldn't be none")
}
}
