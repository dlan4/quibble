#' Plot tracked dataframe as a graph
#'
#' @param x a tracked_df object
#' @param record optionally a named vector of key-value pairs, to show values for a specific key in the graph
#' @param value_col name of a column to draw values from, if record is specified
#' @param value_format a function with single argument to format the value_col
#' @param metric metric to show: either value, row_n, or a summary function with argument table, which returns
#'   a character of length 1 for each stage. Defaults to 'value' if record is provided
#' @param plot_function a function with arguement graph producing the output of the plot. can map
#'   value, .in (int), .out (int), .type (normal/init/merge/final) and .label
#' @param ... arguments to be passed to methods
#' @method plot tracked_df
#' @export
plot.tracked_df <- function(x, record = NULL, value_col = NULL,
                            value_format = function(v) v,
                       metric = c("value", "row_n"), plot_function, ...) {
  keys <- get_keys(x)
  # if there's only one value column assume that should be plotted
  if (!is.null(record)) {
    if (is.null(names(record))) {
      names(record) <- keys
    }
  }
  if (is.null(value_col)) {
    non_key_names <- unique(lapply(x$data,names))[[1]]
    non_key_names <- non_key_names[ !non_key_names %in% keys ]
    if (length(non_key_names) == 1) {value_col <- rlang::sym(non_key_names)
    } else value_col <- rlang::sym(non_key_names[1])
  }

  #validate_plot <- rlang::exprs(is.null(record), is.null(value_col))
  #if( rlang::inject( sum(!!!validate_plot) ) == 1 ) {
  #  stop("Must provide both record and value_col")
  #}

  # Just the edges not the metadata to go with them
  edges <- purrr::imap( x$parents, \(parents, child) {
    tibble::tibble(from = parents, to = child)
  } ) %>%
    # to will not be NA as the name of an element would not be NA.
    # however, from (the parent(s)) may be NA
    purrr::list_rbind()

  # if record is provided, add value metric with which to populate data
  if (!missing(record) & missing(metric)) metric <- "value"
  missing_metric <- missing(metric)
  value_capture_fns <- list()
  value_capture_fns$value <- function(table) {
    record_filter_expr <- purrr::imap( record, \(value, key) {
      rlang::call2(`==`, rlang::sym(key), value)
    }) %>% unname

    x$data[[table]] %>%
      dplyr::filter(!!!record_filter_expr) %>%
      dplyr::select(!!value_col) %>%
      dplyr::mutate(!!value_col := value_format(!!value_col)) %>%
      {ifelse( nrow(.),  as.character(.), "NULL") }
  }
  value_capture_fns$row_n <- function(table) {
    x$data[[table]] %>% nrow %>% as.character
  }
  if ( !missing_metric ) {
    value_col <- if (!inherits(value_col, "name")) rlang::ensym(value_col) else value_col
    fn <- if(rlang::is_function(metric)) metric else value_capture_fns[[metric]]

    edge_data <- x$data %>% names %>%
      tibble::tibble(to = .) %>%
      dplyr::mutate(!!value_col := purrr::map_chr(to, fn) )
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

  if ( is.null( edges[[value_col]] )) edges[[value_col]] <- NA
  graph_in$values <- edges %>%
    dplyr::select( tidyselect::any_of(c("to", as.character(value_col))) ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(.in = purrr::map_dbl(to, \(id)
                                      lookup(id, names(graph_in$inout[[1]]), graph_in$inout[[1]] ) ),
                  .out = purrr::map_dbl(to, \(id)
                                       lookup(id, names(graph_in$inout[[2]]), graph_in$inout[[2]]) ),
                  # need this regardless of if it's the value_col
                  .num_rows = purrr::map_dbl(to, ~as.numeric(value_capture_fns$row_n(.)) ),
                  .type = dplyr::case_when(.in == 0 ~ "init",
                                           .in > 1 ~ "merge",
                                           .num_rows < cummax(.num_rows) ~ "subset",
                                           .out == 0 ~ "final",
                                           TRUE ~ "normal"),
                  .label = dplyr::case_when(missing_metric ~ to,
                                            TRUE ~ rlang::inject( paste0(to, "\n", !!value_col) ))
    )

  graph <- igraph::graph_from_data_frame(graph_in$edges, vertices = graph_in$values, directed = TRUE)

  if (missing(plot_function)) plot_function <- crumbs_plot_default
  plot_function(graph = graph)
}

crumbs_plot_default <- function(graph) {
  ggraph::ggraph(graph, layout = "sugiyama") +
    ggraph::geom_edge_link(
      arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm")),
      end_cap = ggraph::circle(3, "mm"),
      colour = "black"
    ) +
    ggraph::geom_node_point(ggplot2::aes(colour = .type), size = 8) +
    ggraph::geom_node_text(
      ggplot2::aes(label = .label),
      nudge_x = 0.15, nudge_y = 0,
      size = 4
    ) +
    ggplot2::scale_colour_manual(values = c(init = "#cb5252",
                                            merge = "#218760",
                                            final = "#2d1b37",
                                            normal = "#d8d8d5",
                                            subset = "#4c78a8"),
                                 name = "Node type") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 25, 30, 0),
      legend.position = "right"
    )
}

#' Compare stages
#' @param tree a tracked_df object
#' @param value name of value column to compare
#' @param ... stages to select
#' @param comp_names glue specification using {.col} and/or {.name}
#' @param diffs if TRUE, will display TRUE/FALSE
#' @export
history <- function(tree, value, ..., comp_names = "{.col}_{.name}", diffs = FALSE ) {
  stages <- rlang::expr(c(...))
  keys <- get_keys(tree)
  rename_fn <- function(.col, .name, string = comp_names ) {
    glue::glue( string )
  }
  stage_pos <- tidyselect::eval_select(stages, tree$data)
  # filter for requested stages and select relevant columns
  filtered_data_unjoined <- tree$data[ stage_pos ] %>%
    purrr::imap( \(stage, .name) {
      selection <- dplyr::select( stage, dplyr::all_of(keys), all_of(value) )
      dplyr::rename_with( selection, .cols = dplyr::all_of(value), ~rename_fn(., .name) )
      })
  filtered_data <- filtered_data_unjoined %>%
    purrr::reduce( \(a, b) dplyr::full_join(a, b, by = keys) )

  if (diffs) {
    value_cols <- setdiff(names(filtered_data), keys )
    filtered_matrix <- as.matrix(filtered_data[value_cols])
    filtered_data[[".is_different"]] <- !(
      rowSums(is.na(filtered_matrix)) %in% c(0, length(filtered_matrix)) &
      rowSums(filtered_matrix != filtered_matrix[, 1], na.rm = TRUE) == 0
    )
  }
  return(filtered_data)
}


#' Restore key combinations to a stages
#'
#' One use case for this is restoring original keys
#' @param data stage
#' @param to stage which contains the key combinations you want to restore
#' @param which keys to restore - not in use yet
#' @export
restore_keys <- function(data, to, which = "all") {
  keys <- get_keys(to)
  keys_in_data <- keys[keys %in% names(data)]
  col_order <- keys %>% union(names(data)) %>% rlang::syms()
  to[keys] %>%
    dplyr::left_join(data, by = keys_in_data) %>%
    dplyr::select(!!!col_order) %>%
    # preserve stage order
    new_stage(keys = keys)
}




#' Replace names and symbols in an expression
#'
#' @param expr Expression to modify
#' @param replace symbol/name to replace
#' @param replacement character replacement value
#' @export
replace_expr <- function(expr, replace, replacement) {

  if (is.symbol(expr) && identical(expr, as.symbol(replace))) {
    return(rlang::sym(replacement))
  }
  if (is.call(expr)) {
    nms <- names(expr)
    if (!is.null(nms)) {
      nms[nms == replace] <- replacement
    }
    expr[-1] <- lapply(expr[-1], replace_expr, replace, replacement)
    names(expr) <- nms
  }

  expr
}

