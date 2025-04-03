#' R6 Class Representing Data for Correlation with the `coracle` package
#'
#' @description
#' A `coracle_data` nodes stores data for correlation in a tree.
#'
#' @details
#' Branches are based on grouping variables. Data is stored as chunks in the leaf nodes of the tree.
#'
#' @export
coracle_data <- R6::R6Class(
  "coracle_data",
  public = list(
    ## Fields ------------

    #' @field version Scalar character of `coracle` package version used to generate this node.
    version = NULL,

    #' @field id Scalar character hash ID for node.
    id = NULL,

    #' @field chunk `data.frame` of data for correlation.
    chunk = NULL,

    #' @field children List of child `coracle_data` node(s).
    children = NULL,

    #' @field grps_cols Character vector names of grouping column(s).
    grps_cols = NULL,

    #' @field grps_vals Named list of grouping column(s) and chunk value(s).
    grps_vals = NULL,

    #' @field join_col Character scalar name of joining column.
    join_col = NULL,

    #' @field join_vals Vector of values in joining column.
    join_vals = NULL,

    #' @field vals_col Character scalar name of values column.
    vals_col = NULL,

    #' @field other_cols Character vector of names of other column(s).
    other_cols = NULL,

    #' @field chunk_flags Boolean indication of whether this node is valid for correlation.
    chunk_flags = NULL,

    ## Initalization ------------

    #' @description
    #' Create a new `coracle_data` node
    #'
    #'
    #' @param data A `data.frame` of data to prepare for correlation.
    #' @param grps <[`tidy-select`][dplyr_tidy_select]> expressions identifying grouping column(s)
    #' @param join A <[`tidy-select`][dplyr_tidy_select]> expression identifying one joining column.
    #' @param vals <[`tidy-select`][dplyr_tidy_select]> expressions identifying value column(s).
    #' @param ... Unused
    #' @param labl_cols (Optional) Character scalar column name for long-format groups.
    #' @param labl_vals (Optional) Character scalar column name for long-format values.
    #' @param call Caller environment for error reporting.
    #'
    #' @returns A `coracle_data` root node.
    initialize = function(data = NULL,
                          grps = NULL,
                          join = NULL,
                          vals = NULL,
                          ...,
                          labl_cols = NULL,
                          labl_vals = NULL,
                          call = caller_env()) {


      self$id <- hash(as.numeric(Sys.time())) |> str_sub(-7)
      self$version <- as.character(packageVersion("coracle"))

      ### Initial data check ------------

      if (!is.data.frame(data))
        cli_abort(c("x" = "{.arg data} requires a {.cls data.frame}."), call = call)

      ### Tidyselect Group Column(s) ------------

      grps_expr <- enquo(grps)
      grps_pos <- eval_select(grps_expr, data)
      self$grps_cols <- names(grps_pos)

      ### Tidyselect Join Column ------------

      join_expr <- enquo(join)
      join_pos <- eval_select(join_expr, data)

      if (length(join_pos) != 1)
        cli_abort(c("x" = "{.arg join} must select exactly one column."), call = call)

      self$join_col <- names(join_pos)
      self$join_vals <- sort(unique(data[[self$join_col]]))

      ### Tidyselect Variable Column(s) ------------

      vals_expr <- enquo(vals)
      vals_pos <- eval_select(vals_expr, data)

      if (length(vals_pos) < 1)
        cli_abort(c("x" = "{.arg vals} requires a selection."), call = call)

      ### title ------------

      if (length(grps_pos) < 1 && length(vals_pos) == 1)
        cli_abort(c("x" = "{.arg grps} requires a selection."), call = call)

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
        relocate(!!grps_expr, !!join_expr, !!vals_expr)

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

        name_col <- labl_cols %||% paste0("names_", self$id)
        values_col <- labl_vals %||% "values"

        data <- data |> pivot_longer(cols = all_of(names(vals_pos)),
                                     names_to = name_col,
                                     values_to = values_col)

        self$grps_cols <- c(self$grps_cols, name_col)
        self$vals_col <- values_col

      } else {
        # If not necessary
        self$vals_col <- names(vals_pos)
      }

      ### Reorder columns -> grps, join, vals, other ------------

      data <- data |>
        relocate(all_of(self$grps_cols),
                 self$join_col,
                 self$vals_col,
                 all_of(self$other_cols))

      ### Chunk by grps, creating tree ------------

      for (col in self$grps_cols)
      {
        col_values <- data[[col]]
        # col_values <- col_values[!is.na(col_values)]

        if (length(unique(col_values)) > 1) {
          self$children <- sort(unique(col_values)) |>
            set_names() |>
            map(
              \(x)
              coracle_data$new(
                data = data[data[[col]] == x, ],
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

      if (is.null(self$children)) {
        flags <- list()

        if (any(duplicated(data[[self$join_col]])))
          flags <- append(flags, "Duplicate join values.")

        if (length(data[[self$join_col]]) < 3)
          flags <- append(flags, "Less than 3 join values.")

        if (length(unique(data[[self$vals_col]])) <= 1)
          flags <- append(flags, "Constant values.")

        if (length(flags) > 0)
          self$chunk_flags <- flags

        self$grps_vals <- self$grps_cols |>
          set_names() |>
          map(\(x) unique(data[[x]]))

        self$chunk <- data
      }

    }
  ),

  ## Active Bindings ------------

  active = list(
    #' @description
    #' Active binding for chunks of stored data as a list of `data.frame`s.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Chunks of stored data as a list of `data.frames`.
    chunks = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node$chunk)
      } else {
        map(self$children, \(x) x$chunks) |> list_flatten()
      }
    },

    #' @description
    #' Active binding for all stored data as a `data.frame`.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Data as a `data.frame`.
    data = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node$chunk)
      } else {
        temp <- self$chunks

        if (!is.data.frame(temp))
          temp <- temp |> list_rbind()

        return(temp)
      }
    },

    #' @description
    #' Active binding for data prepared for correlation as a `data.frame`.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Data prepared for correlation as a `data.frame`.
    corr_data = function(node = self) {

      node$data |>
        select(all_of(c(self$grps_cols, self$join_col, self$vals_col))) |>
        rename_with(~ paste0(.x, "_", self$id))

    },

    #' @description
    #' Active binding for joining column name in data prepared for correlation as a character scalar.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Character scalar of joining column name in data prepared for correlation.
    corr_join = function(){
      paste0(self$join_col, "_", self$id)
    },

    #' @description
    #' Active binding for grouping column(s) name(s) in data prepared for correlation as a character scalar.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Character vector of grouping column(s) name(s) in data prepared for correlation.
    corr_grps = function(){
      paste0(self$grps_cols, "_", self$id)
    },

    #' @description
    #' Active binding for value column name in data prepared for correlation as a character scalar.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Character scalar of value column name in data prepared for correlation.
    corr_vals = function(){
      paste0(self$vals_col, "_", self$id)
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes.
    leaves = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node)
      } else {
        map(self$children, \(x) x$leaves) |> list_flatten()
      }
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes with data valid for correlation.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes with data valid for correlation.
    leaves_valid = function(node = self) {
      if (!is.null(node$chunk)) {
        if (length(node$chunk_flags) == 0) {
          return(node)
        } else {
          return(NULL)
        }
      } else {
        map(self$children, \(x) x$leaves_valid) |> list_flatten() |> compact()
      }
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes with data invalid for correlation.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes with data invalid for correlation.
    leaves_invalid = function(node = self) {
      if (!is.null(node$chunk)) {
        if (length(node$chunk_flags) != 0) {
          return(node)
        } else {
          return(NULL)
        }
      } else {
        map(self$children, \(x) x$leaves_invalid) |> list_flatten() |> compact()
      }
    }
  )
)
