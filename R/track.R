
#' Track a dataframe through stages
#' @param data dataframe to track
#' @param keys a vector of keys used to identify the data
#' @param name name of the dataframe within the tracked_df
#' @details
#' This function creates a tracked_df object.
#' @export
track <- function(data, keys, name = "init") {

  t_df <- new_tracked_df(data, keys, name)
  validate_tracked_df(t_df)
  return(t_df)
}


#' Add new snapshots to a tree
#' @param tree a snapshot tree
#' @param ... Name-value pairs of snapshots to add to the tree
#' @param from Optional: the snapshot which is being used. If not provided, this
#'  is automatically deduced from the expressions supplied.
#' @export
evolve <- function(.x, ..., .from) {
  snaps <- rlang::enquos(...)
  keys <- get_keys(.x)
  for (i in seq_along(snaps)) {
    # evaluate
    tracked_mask <- rlang::as_data_mask(.x$data)
    tracked_mask$.tracked <- .x$data
    snapshot_name <- names(snaps)[i]

    # find parent
    if (!missing(.from)) {
      parents <- .from
    } else {
      parents <- find_parents( expr=rlang::get_expr( snaps[[i]] ),
                              tracked = .x )
    }
    # ensure parent is not the same as child
    .x$parents[[snapshot_name]] <- parents[parents != snapshot_name]
    # eval
    .x$data[[snapshot_name]] <- rlang::eval_tidy(snaps[[i]], tracked_mask) %>%
      new_snapshot(., keys = keys, from = NULL)
  }
  return (.x)
}

#' Combine snapshots
#'
#' Completes a full join of the data, then coalesces where there are columns with the same name.
#'
#' NOT IN USE
#' @param ... snapshots to merge
#' @param resolve if prefer_first (the default), the first named snapshot will
#'   be preferred when coalescing.
merge_branches <- function(..., resolve = c("prefer_first", "prefer_last")) {
  branches <- list(...)
  stopifnot ( all( purrr::map_lgl(branches, is_snapshot) ) )
  keys <- get_keys(branches[[1]])

  if (missing(resolve)) resolve <- "prefer_first"
  merged <- branches[[1]]

  for (i in 2:length(branches) ) {
    value_columns <- purrr::map( list( merged, branches[[i]] ),
                                 \(x) names(x)[!names(x) %in% keys] )
    # columns which must be coalesced
    merge_cols <- rlang::inject( intersect(!!!value_columns) )
    all_columns <- purrr::list_c(value_columns) %>% c(keys)
    # join data
    joined <- dplyr::full_join( merged, branches[[i]], by = keys )
    # cols to coalesce
    cols_to_coalesce <- which(!names(joined) %in% all_columns)
    cols_as_syms <- rlang::syms(names(joined))
    # column numbers split
    coalesce_exprs <- split(cols_to_coalesce, ceiling(seq_along(cols_to_coalesce) / 2) ) %>%
      setNames(merge_cols) %>%
      purrr::map( \(coalesce_inds) {
        if(resolve == "prefer_last") coalesce_inds <- rev(coalesce_inds)
        rlang::call2( dplyr::coalesce, !!!cols_as_syms[coalesce_inds] )
        })
    merged <- joined %>% dplyr::mutate( !!!coalesce_exprs , .keep = "unused" )
  }
  return(merged)
}

#' Check if object is a snapshot
#' @param x object to check
#' @export
is_snapshot <- function(x) "snapshot" %in% class(x)

#' Check if object is a snapshot tree
#' @param x object to check
#' @export
is_tracked_df <- function(x) "tracked_df" %in% class(x)

new_tracked_df <- function(x, keys, name) {
  snap <- new_snapshot(x, keys = keys)
  snap <- rlang::list2(!!name := snap)
  parents <- rep(NA, length(x)) %>%
    as.list %>%
    rlang::set_names(names(x))
  structure( list(data = snap, parents = parents),
             class = c("tracked_df", "list"), keys = keys )
}

validate_tracked_df <- function(x) {

}

latest <- function(t_df) {
  t_df[[length(t_df)]]
}

#' Print
#'
#' Prints a tracked DF showing the most recent snapshot
#' @param x Object to format or print
#' @param ... arguments to pass to methods
#' @method print tracked_df
#' @export
print.tracked_df <- function(x, ...) {
  s <- x$data
  cat(sep="","Tracked dataframe with snapshots: ", paste0( names(s), collapse = ", "),
      ". Showing ",names(s)[length(s)],":\nKeys = ",paste0(get_keys(x), collapse=", "),"\n")
  print( s[[length(s)]], ... )
}

new_snapshot <- function(data, keys = NULL, from = NULL) {
  structure( tibble::tibble(data), class = c("snapshot", "tbl_df", "tbl", "data.frame"),
             from = from, keys = keys )
}

#' Get keys from an object
#'
#' This is a helper function which calls attr(x, "keys").
#' @param x Either a snapshot or snapshot_tree
#' @export
get_keys <- function(x, ...) {
  UseMethod("get_keys")
}
#' Get keys from an object
#' @method get_keys snapshot
#' @export
get_keys.snapshot <- function(x, ...) {
  return(attr(x, "keys"))
}
#' Get keys from an object
#' @method get_keys tracked_df
#' @export
get_keys.tracked_df <- function(x, ...) {
  return(attr(x, "keys"))
}
#' Get keys from an object
#' @method get_keys data.frame
#' @export
get_keys.data.frame <- function(x, keys, ...) {
  if (missing(keys)) {
    stop("argument \"keys\" is missing and must be provided for dataframes")
  }
}

#' Return all combinations of keys
#' @param tracked tracked dataframe
#' @param ... snapshot names
get_key_combs <- function(tracked, ...) {
  snaps <- rlang::ensyms(...)
  snaps <- as.character(snaps)
  keys <- get_keys(tree)
  keys <- rlang::syms(keys)
  tree$data[snaps] %>%
    purrr::list_rbind() %>%
    dplyr::distinct( !!!keys )
}


