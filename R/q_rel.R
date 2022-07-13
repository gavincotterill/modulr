#' Calculate the assortativity coefficient on a weighted, undirected network graph
#' @param graph_object an `igraph` graph object with 'membership' attribute
#' @export
q_rel <- function(graph_object){
  adj_ind <- as.matrix(igraph::get.adjacency(graph_object, type = "upper", attr = "weight"))
  mem_ind <- igraph::V(graph_object)$membership
  assortnet::assortment.discrete(adj_ind, types = mem_ind, weighted = T)$r
}
