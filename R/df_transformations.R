
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
edit_data <- function(data, edits, na_alias = NA, ... ) {
  keys <- get_keys(data, ...)
  apply_to <- names(edits)[!names(edits) %in% keys]
  apply_to <- tidyselect::eval_select(apply_to, data)
  edits_filter <- purrr::map(keys, \(key) {
    filter <- sprintf("grepl(edits$%s[i], data$%s)", key, key )
    rlang::parse_expr(filter)
  })

  data_edited <- data
  if (missing(na_alias)) na_alias <- NULL
  if (!is.null(na_alias)) {
    edits[apply_to] <- replace(edits[apply_to], edits[apply_to] == na_alias, NA)
  }
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


aggregate_data <- function(data, exprs, exclude) {

}


