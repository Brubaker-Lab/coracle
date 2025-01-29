validate_inputs <- function(data,
                            name_col,
                            join_col,
                            val_cols,
                            call = caller_env(),
                            arg_data = caller_arg(data),
                            arg_name = caller_arg(name_col),
                            arg_join = caller_arg(join_col),
                            arg_vals = caller_arg(val_cols)) {

  # Calling to avoid laziness

  arg_name
  arg_join
  arg_vals

  # Immediate Data Check ------------

  if (!is.data.frame(data)) {
    cli_abort(c("x" = "{.arg {arg_data}} requires a {.cls data.frame}."), call = call)
  }



  # Join Column ------------

  if (is.null(join_col)) {
    join_col <- data |>
      select(where(is_join)) |>
      names()
  }

  if (!is_scalar_character(join_col)) {
    cli_abort(c("x" = "{.arg {arg_join}} requires a {.cls character} scalar."),
              call = call)
  }

  if (!(join_col %in% names(data))) {
    cli_abort(c("x" = "{.arg {arg_join}} must be a column in {.arg {arg_data}}"),
              call = call)
  }

  # Value Column(s) ------------

  if (is.null(val_cols)) {
    val_cols <- data |>
      select(where(is_vals)) |>
      names()
  }

  if (is_empty(val_cols)) {
    val_cols <- data |>
      select(-all_of(c(name_col, join_col))) |>
      select(where(is.numeric)) |>
      names()
  }

  if (!is_character(val_cols)) {
    cli_abort(c("x" = "{.arg {arg_vals}} requires a {.cls character} vector."),
              call = call)
  }

  if (!all(val_cols %in% names(data))) {
    cli_abort(c("x" = "{.arg {arg_vals}} must be a column in {.arg {arg_data}}"),
              call = call)
  }

  # Name column ------------

  if (length(val_cols) == 1) {

    if (is.null(name_col)) {
      name_col <- data |>
        select(where(is_name)) |>
        names()
    }

    if (!is_scalar_character(name_col)) {
      cli_abort(c("x" = "{.arg {arg_name}} requires a {.cls character} scalar."),
                call = call)
    }

    if (!(name_col %in% names(data))) {
      cli_abort(c("x" = "{.arg {arg_name}} must be a column in {.arg {arg_data}}"),
                call = call)
    }
  }

  # Prepare "wide" Data ------------

  if (length(val_cols) > 1){

    data <- data |>
      mutate(across(all_of(val_cols), un_vals)) |>
      pivot_longer(cols = all_of(val_cols),
                   names_to = "name",
                   values_to = "value")

    name_col <- "name"
    val_cols <- "value"

  }

  data <- data |>
    mutate(across(everything(),un_coracle)) |>
    select(any_of(c(name_col,join_col, val_cols))) |>
    group_by(!!sym(name_col)) |>
    group_split()

    return(
      tibble(data = data,
             name_col = name_col,
             join_col = join_col,
             vals_col = val_cols)
    )

}
