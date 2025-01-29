#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param ...
#' @param x_name
#' @param x_join
#' @param x_vals
#' @param x_labl
#' @param y_name
#' @param y_join
#' @param y_vals
#' @param y_labl
#' @param z_name
#' @param z_join
#' @param z_vals
#' @param z_labl
#' @param method
#'
#' @returns
#' @export
#'
#' @examples
coracle <- function(x,
                 y = NULL,
                 z = NULL,
                 ...,

                 x_name = NULL, # Column with the variable of interest
                 x_join = NULL, # Column to use for joining
                 x_vals = NULL, # Column(s) of values for correlation
                 x_labl = NULL, #

                 y_name = NULL,
                 y_join = NULL,
                 y_vals = NULL,
                 y_labl = NULL,

                 z_name = NULL,
                 z_join = NULL,
                 z_vals = NULL,
                 z_labl = NULL,

                 method = "spearman") {

  x <- validate_inputs(x, x_name, x_join, x_vals, x_labl)

  if (!is.null(y)) {
    y <- validate_inputs(y, y_name, y_join, y_vals, y_labl)
  }

  if (!is.null(z)) {
    z <- validate_inputs(z, z_name, z_join, z_vals, z_labl)
  }

  arg_match(method, c("pearson", "kendall", "spearman"))

  if(!is.null(y) && !is.null(z)){
    # Partial
    cli_abort(c("x" = "Partial correlation has not been implemented yet!"))

    corr_xyz(x, y, z, method)

  } else if (!is.null(y)) {
    # Paired
    corr_xy(x, y, method)

  } else {
    # Autocorrelation
    cli_abort(c("x" = "Autocorrelation has not been implemented yet!"))
    corr_x(x, method)
  }

}

corr_xyz <- function(x, y, z, method){

}

corr_xy <- function(x, y, method){


  future_map()

  # future_map(x$data,
  #      \(a) map(y$data,
  #                \(b) {
  #                  full_join(a,
  #                            b,
  #                            by = join_by(
  #                                !!sym(x$join_col) == !!sym(y$join_col)
  #                              ),
  #                            keep = T
  #                            ) |> print()}))

}

corr_x <- function(x, method){

}

corr_data <- function(a, b){



}
