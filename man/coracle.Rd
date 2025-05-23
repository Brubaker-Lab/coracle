% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coracle.R
\name{coracle}
\alias{coracle}
\title{Correlation of Tidy Data}
\usage{
coracle(x, y = NULL, z = NULL, ..., method = "spearman")
}
\arguments{
\item{x}{A \code{coracle_data} object for correlation.}

\item{y}{(Optional) A \code{coracle_data} object for correlation.}

\item{z}{(Optional) A \code{coracle_data} object for correlation.}

\item{...}{Unused}

\item{method}{The correlation method ("spearman", "pearson", or "kendall")}
}
\value{
A \code{coracle_data} object with correlation results.
}
\description{
Correlation of Tidy Data
}
