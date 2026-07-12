
#' Overlay one dataframe onto another
#'
#' @param x dataframe to be edited
#' @param y dataframe containing edits
#' @param by vector of columns to join by
#' @inheritParams rlang::args_dots_empty
#' @param perl use perl-compatible regex
#' @export
overlay <- function(x, y, by, ..., perl = TRUE) {
  if (missing(by) && is_snapshot(x)) by <- get_keys(x)
  # ensure correct col order
  x_cols <- c(by, setdiff(names(x), by)) #x cols in correct order
  cols_to_overlay <- setdiff(intersect(names(x), names(y)), by) #x non-key cols
  y_cols <- c(by, cols_to_overlay) #probably same as x_cols?
  x <- x[x_cols]
  y <- y[y_cols]
  y_overlay <- y[cols_to_overlay]

  match_cache <- list()
  for (key in by) {
    rgx_u <- unique(y[[key]])
    match_cache[[key]] <- lapply(rgx_u ,
       \(rgx) grep(rgx, x[[key]], perl = perl) )
    names( match_cache[[key]] ) <- rgx_u
  }
  out <- x
  key_1 <- by[1]
  for (r in seq_len( nrow(y) ) ) {
    first_pattern <- y[[key_1]][[r]]
    idx_to_replace <- match_cache[[key_1]][[first_pattern]]
    for (key in by[-1]) {
      pattern <- y[[key]][[r]]
      idx_to_replace <- intersect(idx_to_replace,
                                  match_cache[[key]][[pattern]])
    }
  }
  out[idx_to_replace, cols_to_overlay] <- y_overlay[r, cols_to_overlay]

  return(out)
}


pgrepl <- function(x, data) {
  out <- c()
  for (i in seq_along(x)) {
    out <- c(out, grepl(x[i], data[i]))
  }
  return( all(out) )
}

#' Apply edits to a dataframe
#' @examples
#' print(5 + 5)
#' @param data a snapshot or a dataframe
#' @param edits a list or dataframe of edits to be applied
#' @param na_alias an optional alias to use for NAs
#' @param ... arguments passsed on to methods, primarily keys if data is a dataframe
#' @export
edit_data <- function(data, edits, ... ) {
  keys <- get_keys(data, ...)
  apply_to <- names(edits)[!names(edits) %in% keys]
  apply_to <- tidyselect::eval_select(apply_to, data)
  edits_filter <- purrr::map(keys, \(key) {
    filter <- sprintf("grepl(edits$%s[i], data$%s)", key, key )
    rlang::parse_expr(filter)
  })

  data_edited <- data

  for (i in seq_len(nrow(edits)) ) {
    edit <- edits[i, keys]
    new_value <- edits[i, apply_to]
    matches <- c()
    for (j in seq_len(nrow(data_edited)) ) {
      matches[j] <- pgrepl( edit, data_edited[j, keys] )
    }
    data_edited[matches, apply_to] <- new_value
  }

  return(data_edited)
}


#' Aggregate data
#'
#' The main additions compared to dplyr::mutate(!!!exprs) are
#' * Specify axis of aggregation
#' * Specify records to exclude
#' @param data a snapshot
#' @param exprs list of expressions to add
#' @param col key column containing IDs to aggregate
#' @param value column containing values to aggregate
#' @param exclude an exclude() object - to be added
#' @export
aggregate_data <- function(data, exprs, col, value, exclude) {
  keys <- get_keys(data)
  col <- rlang::ensym(col)
  value <- rlang::ensym(value)
  keys <- keys[keys != col]
  agg_df <- track(A0 = tidyr::pivot_wider(data,
                                     names_from = tidyselect::all_of(col),
                                     values_from = tidyselect::all_of(value)),
                  keys = keys)
  for (agg_i in seq_along(exprs)) {
    old_df_name <- rlang::sym(paste0("A",agg_i-1))
    new_df_name <- paste0("A",agg_i)
    agg_df <- agg_df %>%
      evolve(!!new_df_name := dplyr::mutate(!!old_df_name, !!!exprs[[agg_i]]) )
  }
  cols_created <- purrr::list_flatten(exprs, name_spec = "{inner}") %>% names
  cols_to_pivot <- unique(data[[col]]) %>%
    union( cols_created )
  # pivot back
  agg_df$data[[length(agg_df$data)]] %>%
    tidyr::pivot_longer(cols = all_of(cols_to_pivot),
                        names_to = as.character( col ),
                        values_to = as.character( value ))
}


