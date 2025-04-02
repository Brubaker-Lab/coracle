#' R6 Class Representing Data for Correlation
#'
#' @description
#' A `coracle_data` nodes stores data for correlation in a tree.
#'
#' @details
#' Branches are based on grouping variables. Data is stored as chunks in the leaf nodes of the tree.
#'

# Object definition ------------

coracle_data <- R6::R6Class(
  "coracle_data",
  public = list(
    ## Fields ------------

    #' @field version Package version used to generate node.
    version = NULL,

    #' @field id Unique hash ID for node.
    id = NULL,

    #' @field chunk Atomic unit of data stored in leaves.
    chunk = NULL,

    #' @field children Child `coracle_data` node(s).
    children = NULL,

    #' @field grps_cols Grouping column(s) in data for this node and it's children.
    grps_cols = NULL,

    #' @field join_col Joining column in data.
    join_col = NULL,

    #' @field join_vals Joining column values.
    join_vals = NULL,

    #' @field vals_col Values column in data.
    vals_col = NULL,

    #' @field other_cols Other column(s) present in data.
    other_cols = NULL,

    debug = NULL,

    ## Initalization ------------

    #' @description
    #' Create a new `coracle_data` node
    #'
    #'
    #' @param data
    #' @param call
    #' @param grps
    #' @param join
    #' @param vals
    #'
    #' @returns
    #'
    #' @examples
    initialize = function(data = NULL,
                          grps = NULL,
                          join = NULL,
                          vals = NULL,
                          ...,
                          labl_cols = NULL,
                          labl_vals = NULL,
                          call = caller_env()) {
      ### Initial data check ------------

      if (!is.data.frame(data))
        cli_abort(c("x" = "{.arg data} requires a {.cls data.frame}."), call = call)

      ### Tidyselect Group Column(s) ------------

      grps_expr <- rlang::enquo(grps)
      grps_pos <- tidyselect::eval_select(grps_expr, data)

      if (length(grps_pos) < 1)
        cli_abort(c("x" = "{.arg grps} requires a selection."), call = call)

      self$grps_cols <- names(grps_pos)

      ### Tidyselect Join Column ------------

      join_expr <- rlang::enquo(join)
      join_pos <- tidyselect::eval_select(join_expr, data)

      if (length(join_pos) != 1)
        cli_abort(c("x" = "{.arg join} must select exactly one column."), call = call)

      self$join_col <- names(join_pos)
      self$join_vals <- sort(unique(data[[self$join_col]]))

      ### Tidyselect Variable Column(s) ------------

      vals_expr <- rlang::enquo(vals)
      vals_pos <- tidyselect::eval_select(vals_expr, data)

      if (length(vals_pos) < 1)
        cli_abort(c("x" = "{.arg vals} requires a selection."), call = call)

      ### Ensure no overlapping selections ------------

      if (length(intersect(grps_pos, join_pos)) != 0)
        cli_abort(c("x" = "{.arg grps} and {.arg join} may not share any column(s)."),
                  call = call)

      if (length(intersect(grps_pos, vals_pos)) != 0)
        cli_abort(c("x" = "{.arg grps} and {.arg vals} may not share any column(s)."),
                  call = call)

      if (length(intersect(join_pos, vals_pos)) != 0)
        cli_abort(c("x" = "{.arg join} and {.arg vals} may not share any column(s)."),
                  call = call)

      ### Other Column(s)

      self$other_cols <- subset(data, select = -c(join_pos, vals_pos, grps_pos)) |>
        names()

      ### Reassemble data ------------

      data <- data |>
        dplyr::relocate(!!grps_expr, !!join_expr, !!vals_expr)

      ### Transform data to long format, if necessary ------------

      if (length(vals_pos) > 1) {

        if (!is.null(labl_cols) && !is_scalar_character(labl_cols))
            cli_abort(c("x" = "{.arg labl_cols} requires a {.cls character} scalar."),
                      call = call)

        if (!is.null(labl_vals)) {
          if (!is_scalar_character(labl_vals)) {
            cli_abort(c("x" = "{.arg labl_vals} requires a {.cls character} scalar."),
                      call = call)
          }
        }

        name_col <- labl_cols %||% "names"
        values_col <- labl_vals %||% "values"

        data <- data |> tidyr::pivot_longer(
          cols = all_of(names(vals_pos)),
          names_to = name_col,
          values_to = values_col
        )

        self$grps_cols <- c(self$grps_cols, name_col)
        self$vals_col <- values_col

      } else {
        # If not necessary
        self$vals_col <- names(vals_pos)
      }

      ### Reorder columns -> grps, join, vals, other ------------

      data <- data |>
        dplyr::relocate(all_of(self$grps_cols), self$join_col, self$vals_col, all_of(self$other_cols))

      ### Chunk by grps, creating tree ------------

      for(col in self$grps_cols)
      {

        col_values <- data[[col]]
        col_values <- col_values[!is.na(col_values)]

        if(length(unique(col_values)) > 1){

          self$children <- sort(unique(col_values)) |>
            purrr::set_names() |>
            map(\(x)
                 coracle_data$new(data = data[data[[col]] == x,],
                                  grps = self$grps_cols,
                                  join = self$join_col,
                                  vals = self$vals_col,
                                  labl_cols = labl_cols,
                                  labl_vals = labl_vals
                                  )
                 )

          break
        }
      }

      if(is.null(self$children)){
        self$chunk <- data
      }

      self$id <- rlang::hash(as.numeric(Sys.time())) |> stringr::str_sub(-7)
      self$version <- as.character(packageVersion("coracle"))
    }
  ),

  ## Active Bindings ------------

  active = list(
    #' @description
    #' Get list of data chunks.
    #'
    #'
    #' @param node
    #'
    #' @returns Stored data as a list of `data.frames` by chunk.
    #'
    #' @examples
    chunks = function(node = self) {

      map(self$leaves,
          \(x) x$chunk)

    },

    #' @description
    #' Get data as a data frame.
    #'
    #'
    #' @param node
    #'
    #' @returns Stored data as `data.frame`.
    #'
    #' @examples
    data = function(node = self) {

      self$chunks |>
        purrr::list_rbind()

    },

    #' @description
    #' Get list of leaf nodes with data.
    #'
    #' @param node
    #'
    #' @returns List of `coracle_data` nodes storing data.
    #'
    #' @examples
    leaves = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node)
      } else {
        map(self$children, \(x) x$leaves) |> purrr::list_flatten()
      }
    }
  )
)
