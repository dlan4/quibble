
find_parents <- function(expr, tree) {
  tree_nodes <- names(tree$data)
  expr_names <- all.names(expr)
  nodes_referenced <- intersect(expr_names, tree_nodes)
  if (!length(nodes_referenced)) return(NA)
  return(nodes_referenced)
}
