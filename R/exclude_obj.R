
#' Helper to exclude key combinations from a transformation
#' @param ... list specifying key combinations to exclude
exclude <- function(...) {

  structure(, class = "exclude")
}

#' Print exclude object
#' @param x exclude object
#' @param ... pass to method
#' @export
print.exclude <- function(x, ...) {

}

