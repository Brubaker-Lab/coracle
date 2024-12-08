% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_col.R
\name{corr}
\alias{corr}
\title{(Internal)Mapped correlation function for \code{corr_col()}}
\usage{
corr(var_x, var_y, corr_data, method)
}
\arguments{
\item{var_x}{A column name of \code{corr_data} as a character of length 1}

\item{var_y}{A column name of \code{corr_data} as a character of length 1}

\item{corr_data}{The data frame prepared for correlation}

\item{method}{(Optional) A character of length 1 indicating which correlation coefficient is to be used: \code{"spearman"} (the default), \code{"pearson"}, or \code{"kendall"}.}
}
\value{
The results of a single correlation as a data frame row.
}
\description{
(Internal)Mapped correlation function for \code{corr_col()}
}
