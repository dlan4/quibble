

snapshot_tree <- function(data, keys, name = "init") {
  cube <- new_snapshot_tree(.data, .keys, .frame1)
  validate_snapshot_tree()
  return(cube)
}
new_snapshot <- function(.data, .from = NULL) {
  structure( .data, class = c("snapshot", "tbl_df", "tbl", "data.frame"), from = .from )
}
is_snapshot <- function(x) "snapshot" %in% class(x)
is_snapshot_tree <- function(x) "snapshot_tree" %in% class(x)

new_snapshot_tree <- function(.keys, ...) {
  snaps <<- rlang::list2(...)
  if (length(snaps) == 1 & !is_snapshot(snaps[[1]])) {
    snaps[[1]] <- new_snapshot(snaps[[1]])
    names(snaps) <- "init"
  }
  snaps <- purrr::imap(snaps, \(snap, name) {
    if (!is_snapshot(snap)) stop(sprintf("%s is not a snapshot", name))
    attr(snap, "keys") <- .keys
    snap
    })
  structure( rlang::list2( !!!snaps ), class = c("snapshot_tree", "list"), keys = .keys )
}
#' Get keys from an object
#'
#' This is a helper function which calls attr(x, "keys").
#' @param x Either a snapshot or snapshot_tree
get_keys <- function(x, ...) {
  UseMethod("get_keys")
}
get_keys.snapshot <- function(x, ...) {
  return(attr(x, "keys"))
}
get_keys.snapshot_tree <- function(x, ...) {
  return(attr(x, "keys"))
}
get_keys.data.frame <- function(x, keys, ...) {
  if (missing(keys)) {
    stop("argument \"keys\" is missing and must be provided for dataframes")
  }
}

validate_snapshot_tree <- function() {

}
print.snapshot_tree <- function(x, ...) {
  cat(sep="","Snapshot tree with frames: ", paste0( names(x), collapse = ", "),
      ". Showing ",names(x)[length(x)],":\nKeys = ",paste0(attr(x, "keys"), collapse=", "),"\n")
  print(x[[length(x)]])
}

init <- tibble(code = c("HAHA","HAHA","HAHA"),
               data_id = c("code","data_id","value"),
               value=c("Hong Kong",1760,4.5) ) %>%
  new_snapshot_tree(.keys = c("code", "data_id")) %>% .[["init"]]
.from = "init"
frames <- quos(edited1 = edit_data(.data$init, edits, "NULL") )

add_snapshot <- function(.x, ..., .from) {
  snaps <- rlang::enquos(...)
  keys <- get_keys(.x)
  for (i in seq_along(snaps)) {
    tree_mask <- rlang::as_data_mask(.x)
    snapshot_name <- names(snaps)[i]
    .x[[snapshot_name]] <- eval_tidy(snaps[[i]], tree_mask)
  }
  return (.x)
}


library(sloop)
s3_dispatch( print(new_snapshot_tree(c("code","data_id"), chl_i1 ) ) )

