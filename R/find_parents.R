
find_parents <- function(expr, tracked) {
  tree_nodes <- names(tracked$data)
  expr_names <- all.names(expr)
  nodes_referenced <- intersect(expr_names, tree_nodes)
  if (!length(nodes_referenced)) return(NA)
  return(nodes_referenced)
}
