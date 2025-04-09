#' (Internal) Utility function for comparing values, inclusive of NAs
#'
#' @param v1 An atomic value.
#' @param v2 An atomic value.
#'
#' @returns `TRUE` where values are the same (including NAs), otherwise `FALSE`
`%same_as%` <- function(v1, v2)
{
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}


