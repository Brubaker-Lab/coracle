% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_numeric_length}
\alias{check_numeric_length}
\title{(Internal) Argument checking for \code{coracle::statistic_column} value.}
\usage{
check_numeric_length(x, arg = caller_arg(x), call = caller_env())
}
\arguments{
\item{x}{A numeric of length 1 or NA.}

\item{arg}{The calling function's argument corresponding to \code{x}.}

\item{call}{The calling function's environment.}
}
\value{
Error if argument conditions are not met, NULL otherwise.
}
\description{
(Internal) Argument checking for \code{coracle::statistic_column} value.
}
