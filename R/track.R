
`%>%` <- magrittr::`%>%`

#' @export
track <- function(..., keys) {
  x <- list(...)
  tree <- new_snapshot_tree(x, keys)
  validate_snapshot_tree(tree)
  return(tree)
}

#' @export
evolve <- function(tree, ..., from) {
  snaps <- rlang::enquos(...)
  keys <- get_keys(tree)
  for (i in seq_along(snaps)) {
    # evaluate
    tree_mask <- rlang::as_data_mask(tree$data)
    tree_mask$.tree <- tree$data
    snapshot_name <- names(snaps)[i]
    tree$data[[snapshot_name]] <- rlang::eval_tidy(snaps[[i]], tree_mask)

    # find parent
    if (!missing(from)) {
      parents <- from
    } else {
      parents <- find_parents( expr=rlang::get_expr( snaps[[i]] ),
                              tree = tree )
    }
    tree$parents[[snapshot_name]] <- parents
  }
  return (tree)
}

#' @export
merge_branches <- function(..., resolve = c("prefer_first", "prefer_last")) {
  branches <<- list(...)
  stopifnot ( all( purrr::map_lgl(branches, is_snapshot) ) )
  keys <- get_keys(branches[[1]])

  if (missing(resolve)) resolve <- "prefer_first"
  merged <- branches[[1]]
  purrr::map( branches, \(branch) names(branch)[!names(branch) %in% keys] )
  for (i in 2:length(branches) ) {
    value_columns <- purrr::map( list( merged, branches[[i]] ),
                                 \(x) names(x)[!names(x) %in% keys] )
    # columns which must be coalesced
    merge_cols <- rlang::inject( intersect(!!!common_values) )
    all_columns <- purrr::list_c(value_columns) %>% c(keys)
    # join data
    joined <- dplyr::full_join( merged, branches[[i]], by = keys )
    # cols to coalesce
    cols_to_coalesce <- which(!names(joined) %in% all_columns)
    cols_as_syms <- rlang::syms(names(joined))
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

is_snapshot <- function(x) "snapshot" %in% class(x)
is_snapshot_tree <- function(x) "snapshot_tree" %in% class(x)

new_snapshot_tree <- function(x, keys) {
  init_count <- 1
  for (i in seq_along(x) ) {
    # handle missing name
    if ( is.na( names(x)[i] %||% NA ) ) {
      names(x)[i] <- sprintf("init_%i", init_count)
      init_count <- init_count + 1
    }
    # convert dataframe to snapshot
    if ( !is_snapshot( x[[i]] ) ) {
      x[[i]] <- new_snapshot( x[[i]], keys = keys )
    }
  }
  parents <- rep(NA, length(x)) %>% as.list %>% setNames(names(x))
  structure( list(data = x, parents = parents),
             class = c("snapshot_tree", "list"), keys = keys )
}

validate_snapshot_tree <- function(x) {

}
#' @export
print.snapshot_tree <- function(x, ...) {
  s <- x$data
  cat(sep="","Snapshot tree with frames: ", paste0( names(s), collapse = ", "),
      ". Showing ",names(s)[length(s)],":\nKeys = ",paste0(get_keys(x), collapse=", "),"\n")
  print( s[[length(s)]] )
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
#' @export
get_keys.snapshot <- function(x, ...) {
  return(attr(x, "keys"))
}
#' @export
get_keys.snapshot_tree <- function(x, ...) {
  return(attr(x, "keys"))
}
#' @export
get_keys.data.frame <- function(x, keys, ...) {
  if (missing(keys)) {
    stop("argument \"keys\" is missing and must be provided for dataframes")
  }
}


