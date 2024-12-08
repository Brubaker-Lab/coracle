% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_col.R
\name{corr_col}
\alias{corr_col}
\title{Correlation of columns for tidy data.}
\usage{
corr_col(
  x,
  y = NULL,
  ...,
  xy_join = NULL,
  x_name = "x",
  y_name = "y",
  method = "spearman"
)
}
\arguments{
\item{x}{A tidy data.frame containing numeric data columns.}

\item{y}{(Optional) A tidy data frame containing numeric data columns for pairwise correlation with \code{x}.}

\item{...}{Unused.}

\item{xy_join}{(Optional) A \code{\link[dplyr]{join_by}} join specification for \code{x} and \code{y}.}

\item{x_name}{(Optional) A character of length 1 to be used as the first column name.}

\item{y_name}{(Optional) A character of length 1 to be used as the second column name.}

\item{method}{(Optional) A character of length 1 indicating which correlation coefficient is to be used: \code{"spearman"} (the default), \code{"pearson"}, or \code{"kendall"}.}
}
\value{
A data frame of correlation results.
}
\description{
Correlation of columns for tidy data.
}
\examples{

# Single data frame
x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
corr_col(x)

# Data frames with a shared column, "i"
x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
corr_col(x, y)

# Data frames without a shared column
x <- data.frame(ix= as.character(1:5), ux = 1:5, dx = 5:1)
y <- data.frame(iy = as.character(1:5), uy = 1:5, dy = 5:1)
corr_col(x, y, xy_join = dplyr::join_by(ix == iy))

# Renaming the outputs
x <- data.frame(i= as.character(1:5), ux = 1:5, dx = 5:1)
y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
corr_col(x,y, x_name = "first", y_name = "second")

}
