
.select_tracked_df <- rlang::new_environment()
peek_tracked_df <- function(fn = NULL) {
  context <- .select_tracked_df$current
  if (is.null(context)) {
    cli::cli_abort(
      sprintf("%s must be used inside a crumbs stage selection.",
              context %||% "This helper")
    )
  }
  return(context)
}
as.list(.select_tracked_df)
eval_stage_select <- function(expr, data) {
  expr <- rlang::enquo(expr)
  # restore initial context on exit
  old_context <- .select_tracked_df$current
  on.exit(.select_tracked_df$current <- old_context,
          add = TRUE)
  # add current tracked_df as complement to tidyselect vars and data
  .select_tracked_df$current <- data$parents
  locs <- tidyselect::eval_select(
    expr = expr, data = data$data, allow_rename = FALSE
  )
  return(locs)
}


#' Select stage(s) using tidyselect-like syntax
#'
#' @param .data tracked dataframe
#' @param ... Columns to select, unquoted, separated by commas. You can use
#'   helpers as described here.
#' @export
select_stages <- function(.data, ...) {
  stopifnot( is_tracked_df(x) )
  selections <- rlang::enquos(...)
  data_s <- .data$data
  for (i in seq_along(selections)) {
    s <- selections[[i]]
    vars <- tidyselect::eval_select(s, data = .data$data)
    data_s <- data_s[vars]
  }
  return(data_s)
}

#' Select the latest stage
#'
#' @inheritParams tidyselect::everything vars
#' @export
latest <- function(vars = NULL) {
  vars <- vars %||% tidyselect::peek_vars(fn = "latest")
  length(vars)
}

eval_stage_select( descendants(), .data)

#' Select descendants of a stage
#'
#' @inheritParams tidyselect::everything vars
#' @export
descendants <- function(vars = NULL) {
  browser()
  vars <- vars %||% peek_tracked_df(fn = "descendants")
}

