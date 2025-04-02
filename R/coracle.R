validate_coracle_input <- function(input,
                                   arg = caller_arg(input),
                                   call = caller_env()) {
  if (!is.null(input) && !("coracle_data" %in% class(input))) {
    cli_abort(c("x" = "{.arg {arg}} requires {.cls coracle_obj}."))
  }

}

corr_result <- function(a_grps,
                        b_grps = NA_character_,
                        c_grps = NA_character_,
                        stat_value = NA,
                        p = NA,
                        n = NA,
                        message = NA_character_,
                        method
){

  stat_name <- list(spearman = "rho",
                    kendall = "tau",
                    pearson = "cor")

  result_front <- list(a = name_a,
                 b = name_b)

  if(!is.na(name_c)){
    result_front <- c(result_front, c = name_c)
  }

  statistic <- stat_value
  names(statistic) <- stat_name[[method]]

  result_back <- list(
    p = p,
    n = n,
    message = message
  )

  c(result_front, statistic, result_back)

}

#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param ...
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
                    method = "spearman") {

  validate_coracle_input(x)
  validate_coracle_input(y)
  validate_coracle_input(z)

  arg_match(method, c("spearman", "pearson", "kendall"))

  result <- NULL

  if (!is.null(x) && !is.null(y) && !is.null(z)) {
    cli_abort(c("x" = "Pairwise partial correlation has not been implemented yet"))
    result <- pairwise_partial_correlation(x, y, z, method)
  } else if (!is.null(x) && !is.null(y)) {
    result <- pairwise_correlation(x, y, method)
  } else if (!is.null(x)) {
    cli_abort(c("x" = "Autocorrelation has not been implemented yet"))
    result <- autocorrelation(x, method)
  } else {
    cli_abort(c("x" = "This case should be unreachable!"))
  }

}

pairwise_correlation <- function(x,y, method){

  if(!is_joins_valid(x,y))
    cli_abort(c("x" = "Not enough overlap in join values to run correlation."))

  leaf_pairs <- expand_grid(a = x$leaves, b = y$leaves) |> mutate(method = method) |> slice_sample(n=100)

  future_pmap(leaf_pairs,
              correlate)
}

is_joins_valid <- function(a,b){
  length(intersect(a$join_vals, b$join_vals)) >= 3 # Min. 3 for correlation
}



is_values_constant <- function(a_data, a){

  a_data |>
    pluck(a$vals_col) |>
    unique() |>
    length() == 1

}

correlate <- function(a, b, method) {

  if(!is_joins_valid(a,b))
    return()

  # avoids `join_by`'s input validation
  a_join <- a$join_col
  b_join <- b$join_col

  a_suffix <- paste0("_", a$id)
  b_suffix <- paste0("_", b$id)

  corr_data <- full_join(a$data,
                         b$data,
                         by = join_by(!!sym(a_join) == !!sym(b_join)),
                         suffix = c(a_suffix, b_suffix)) |>
    drop_na()

  corr_data

}
