pcor_col <- function(x,
                     y = NULL,
                     z = NULL,
                     ...,
                     x_name = "x",
                     y_name = "y",
                     z_name = "z",
                     xy_join_vars = NULL,
                     xz_join_vars = NULL,
                     method = "spearman") {
  # Two cases: x, xyz

  cli_abort(c("{.fn coracle::pcor_col}` is still under development."))

}

pcor_col_x <- function(x,
                       x_name = "x",
                       ...,
                       method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
}


pcor_col_xyz <- function(x,
                         y,
                         z,
                         ...,
                         x_name = "x",
                         y_name = "y",
                         z_name = "z",
                         xy_join_vars = NULL,
                         xz_join_vars = NULL,
                         method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))

}
