
#' Plot snapshot tree as a graph
#'
#' @param x a snapshot tree
#' @param record optionally a named vector of key-value pairs, to show values for a specific key in the graph
#' @param ... arguments to be passed to methods
#' @export
plot.snapshot_tree <- function(x, record = NULL, ...) {
  edges <- purrr::imap( x$parents, \(parents, child) {
    tibble::tibble(from = parents, to = child)
  } ) %>%
    # to will not be NA as the name of an element would not be NA.
    # however, from (the parent(s)) may be NA
    purrr::list_rbind() %>%
    dplyr::filter(!is.na(from))
  keys <- get_keys(tree)

  if ( 1 == 2) {
  #if (!is.null(record)) {
    record_filter_expr <- purrr::imap( record, \(value, key) {
      rlang::call2(`==`, rlang::sym(key), value)
      }) %>% unname
    x$data %>% purrr::imap( \(df, name) {

    })
    x$data$init_1 %>% dplyr::filter(!!!record_filter_expr) %>% dplyr::pull(value)
  }
  graph <- igraph::graph_from_data_frame(edges)
  base::plot(graph,
       layout = igraph::layout_as_tree(graph),
       edge.arrow.size = 0.5,
       vertex.label = igraph::V(graph)$name,
       vertex.size = 30,
       vertex.color = "lightblue")
}

#' Compare snapshots
#' @param tree a snapshot tree
#' @param values list of value columns
#' @param ... snapshots to select
#' @param comp_names glue specification using {.col} and/or {.name}
#' @export
history <- function(tree, values, ..., comp_names = "{.col}_{.name}" ) {
  values <- rlang::ensyms(values)
  values <- as.character(values)
  snaps <- rlang::ensyms(...)
  keys <- get_keys(tree)
  rename_fn <- function(.col, .name, string = comp_names ) {
    glue::glue( string )
  }

  tree$data[ as.character(snaps) ] %>%
    purrr::imap( \(snap, .name) {
      selection <- dplyr::select( snap, dplyr::all_of(keys), !!!values )
      dplyr::rename_with( selection, .cols = dplyr::all_of(values), ~rename_fn(., .name) )
      }) %>%
    purrr::reduce( \(a, b) dplyr::full_join(a, b, by = keys) )
}

#' Compare all snapshots within a tree
#' This is a special case of history()
#' @param tree a snapshot tree
#' @param values list of value columns
#' @param comp_names glue specification using {.col} and/or {.name}
#' @export
history_all <- function(tree, values, comp_names = "{.col}_{.name}") {
  values <- rlang::ensym(values)
  all_snapshot_names <- rlang::syms( names(tree$data) )
  history(tree, !!values, !!!all_snapshot_names, comp_names = comp_names)
}
