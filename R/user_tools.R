
#' Plot snapshot tree as a graph
#'
#' @param x a snapshot tree
#' @param record optionally a named vector of key-value pairs, to show values for a specific key in the graph
#' @param value_col name of a column to draw values from, if record is specified
#' @param plot_function a function with arguement graph producing the output of the plot. can map
#'   value, .in (int), .out (int), .type (normal/init/merge/final) and .label
#' @param ... arguments to be passed to methods
#' @method plot snapshot_tree
plot.snapshot_tree <- function(x, record = NULL, value_col = NULL,
                               plot_function, ...) {
  keys <- get_keys(x)
  # if there's only one value column assume that should be plotted
  if (!is.null(record)) {
    if (is.null(names(record))) {
      names(record) <- keys
    }
    if (is.null(value_col)) {
      non_key_names <- unique(lapply(x$data,names))[[1]]
      non_key_names <- non_key_names[ !non_key_names %in% names(record) ]
      if (length(non_key_names) == 1) value_col <- rlang::sym(non_key_names)
    }
  }
  validate_plot <- rlang::exprs(is.null(record), is.null(value_col))
  if( rlang::inject( sum(!!!validate_plot) ) == 1 ) {
    stop("Must provide both record and value_col")
  }

  edges <- purrr::imap( x$parents, \(parents, child) {
    tibble::tibble(from = parents, to = child)
  } ) %>%
    # to will not be NA as the name of an element would not be NA.
    # however, from (the parent(s)) may be NA
    purrr::list_rbind()

  # if record is provided, add value column with which to populate data
  if ( !is.null(record) ) {
    value_col <- rlang::enquo(value_col)
    record_filter_expr <- purrr::imap( record, \(value, key) {
      rlang::call2(`==`, rlang::sym(key), value)
      }) %>% unname

    edge_data <- x$data %>% names %>% tibble::tibble(to = .) %>%
      dplyr::mutate(value = purrr::map_chr(to, \(table) {
        x$data[[table]] %>%
          dplyr::filter(!!!record_filter_expr) %>%
          dplyr::select(!!value_col) %>%
          {ifelse( nrow(.),  as.character(.), "NULL") }
        } ))
    edges <- dplyr::left_join(edges, edge_data, by = "to")
  }
  graph_in <- list()
  graph_in$edges <- edges %>%
    dplyr::filter(!is.na(from)) %>%
    dplyr::select(from, to)
  # calculate in/out
  graph_in$graph <- igraph::graph_from_data_frame(graph_in$edges)
  graph_in$inout <- purrr::map( c("in", "out"), \(mode)
                igraph::degree(graph_in$graph, mode = mode) )

  graph_in$values <- edges %>%
    dplyr::select( to, !!value_col ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(.in = purrr::map_dbl(to, \(id)
                                      lookup(id, names(graph_in$inout[[1]]), graph_in$inout[[1]] ) ),
                  .out = purrr::map_dbl(to, \(id)
                                       lookup(id, names(graph_in$inout[[2]]), graph_in$inout[[2]]) ),
                  .type = dplyr::case_when(.in == 0 ~ "init",
                                           .in > 1 ~ "merge",
                                           .out == 0 ~ "final",
                                           TRUE ~ "normal"),
                  .label = dplyr::case_when(is.null(value_col) ~ to,
                                            TRUE ~ rlang::inject( paste0(to, "\n", !!value_col) ))
    )

  graph <- igraph::graph_from_data_frame(graph_in$edges, vertices = graph_in$values, directed = TRUE)

  if (missing(plot_function)) plot_function <- quibble_plot_default
  plot_function(graph = graph)
}

quibble_plot_default <- function(graph) {
  ggraph::ggraph(graph, layout = "sugiyama") +
    ggraph::geom_edge_link(
      arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm")),
      end_cap = ggraph::circle(3, "mm"),
      colour = "black"
    ) +
    ggraph::geom_node_point(ggplot2::aes(colour = .type), size = 8) +
    ggraph::geom_node_text(
      aes(label = .label),
      nudge_x = 0.15, nudge_y = 0,
      size = 4
    ) +
    ggplot2::scale_colour_manual(values = c(init = "#cb5252",
                                            merge = "#218760",
                                            final = "#2d1b37",
                                            normal = "#d8d8d5"),
                                 name = "Node type") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = margin(0, 25, 30, 0),
      legend.position = "right"
    )
}

#' Compare snapshots
#' @param tree a snapshot tree
#' @param values list of value columns
#' @param ... snapshots to select
#' @param comp_names glue specification using {.col} and/or {.name}
#' @param diffs if TRUE, will display TRUE/FALSE
#' @export
history <- function(tree, values, ..., comp_names = "{.col}_{.name}", diffs = FALSE ) {
  values <- rlang::ensyms(values)
  values <- as.character(values)
  snaps <- rlang::ensyms(...)
  keys <- get_keys(tree)
  rename_fn <- function(.col, .name, string = comp_names ) {
    glue::glue( string )
  }
  # filter for requested snapshots and select relevant columns
  filtered_data <- tree$data[ as.character(snaps) ] %>%
    purrr::imap( \(snap, .name) {
      selection <- dplyr::select( snap, dplyr::all_of(keys), !!!values )
      dplyr::rename_with( selection, .cols = dplyr::all_of(values), ~rename_fn(., .name) )
      }) %>%
    purrr::reduce( \(a, b) dplyr::full_join(a, b, by = keys) )

  if (diffs) {
    filtered_data <- filtered_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.is_different = dplyr::n_distinct(
        dplyr::c_across(-dplyr::all_of(keys)) ) != 1) %>%
      dplyr::ungroup()
  }
  return(filtered_data)
}

#' Compare all snapshots within a tree
#' This is a special case of history()
#' @param tree a snapshot tree
#' @param values list of value columns
#' @param comp_names glue specification using {.col} and/or {.name}
#' @param ... arguments to pass to history()
#' @export
history_all <- function(tree, values, comp_names = "{.col}_{.name}", ...) {
  values <- rlang::ensym(values)
  all_snapshot_names <- rlang::syms( names(tree$data) )
  history(tree, !!values, !!!all_snapshot_names, comp_names = comp_names, ...)
}
