% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcor_col.R
\name{pcor}
\alias{pcor}
\title{(Internal) Mapped partial correlation function for \code{pcor_col()}}
\usage{
pcor(var_x, var_y, var_z, pcor_data, method)
}
\arguments{
\item{var_x}{A column name of \code{pcor_data} as a character of length 1.}

\item{var_y}{A column name of \code{pcor_data} as a character of length 1.}

\item{var_z}{A list of column names of \code{pcor_data}.}

\item{pcor_data}{The data prepare for partial correlation.}

\item{method}{(Optional) A character of length 1 indicating which correlation coefficient is to be used: \code{"spearman"} (the default), \code{"pearson"}, or \code{"kendall"}.}
}
\value{
The results of a single partial correlation as a data frame row.
}
\description{
(Internal) Mapped partial correlation function for \code{pcor_col()}
}
