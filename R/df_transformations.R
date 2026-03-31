
#' Apply edits to a dataframe
#' @examples
#' print(5 + 5)
#'
#' @export
edit_data <- function(data, edits, na_alias, ... ) {
  keys <- get_keys(data, ...)
  apply_to <- names(edits)[!names(edits) %in% keys]
  apply_to <- tidyselect::eval_select(apply_to, data)
  edits_filter <- purrr::map(keys, \(key) {
    filter <- sprintf("grepl(edits$%s[i], data$%s)", key, key )
    rlang::parse_expr(filter)
  })

  # bring together
  edits_fn <- rlang::new_function(args = pairlist2(i = )
    ,rlang::call2("*", !!!edits_filter) )

  if (missing(na_alias)) na_alias <- NULL
  if (!is.null(na_alias)) {
    edits[apply_to] <- replace(edits[apply_to], edits[apply_to] == na_alias, NA)
  }
  data_edited <- data
  for (i in seq_len(nrow(edits)) ) {
    edit_ind <- which( edits_fn(i) == 1 )
    data_edited[edit_ind, apply_to] <- edits[i, apply_to]
  }
  return(data_edited)
}


aggregate_data <- function(data, exprs, exclude) {

}


