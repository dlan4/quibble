
lookup <- function(x, vec1, vec2) {
  ind <- match(x, vec1)
  return( vec2[ind] )
}
