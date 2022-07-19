#' Calculate the Newman's Q
#'
#' Wrapper around assortnet function that takes a weighted, undirected network graph and returns the assortativity coefficient.
#'
#' @param graph_object an `igraph` graph object with 'membership' attribute
#' @export
#' @examples
#' \donttest{
#' g <- simulate_graph(n_animals = 25,
#'                     n_groups = 4,
#'                     time_to_leave = 5,
#'                     time_to_return = 2,
#'                     travel_time = c(0.01, 0.2),
#'                     samples_per_day = 1,
#'                     sampling_duration = 7)
#' q_rel(g)
#' }
q_rel <- function(graph_object){
  adj_ind <- as.matrix(igraph::get.adjacency(graph_object, type = "upper", attr = "weight"))
  mem_ind <- igraph::V(graph_object)$membership
  assortnet::assortment.discrete(adj_ind, types = mem_ind, weighted = T)$r
}
