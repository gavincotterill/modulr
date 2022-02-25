#' Plot a graph, color-coded by `rnetcarto` modules
#'
#' @param x, a named square adjacency matrix or named igraph graph. Intended for
#'   weighted, undirected graphs.
#' @param alg, the community detection algorithm to use, either from igraph:
#' "fast_greedy", "leading_eigen", "louvain", or "walktrap"
#' or from rnetcarto: "netcarto"
#' default is "walktrap"
#'
#' @return An igraph plot using the Fruchterman-Reingold layout and displaying
#'   color-coded node membership as determined by `rnetcarto`.
#' @export
#'
#' @examples
#' adjmat <- matrix(sample(seq(0, 1, .01), 16), nrow = 4)
#' diag(adjmat) <- 0
#' adjmat[lower.tri(adjmat)] <- 0
#' rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:4)
#' assortr_plot(adjmat)
assortr_plot <- function(x, alg = "walktrap"){
  if (!requireNamespace(c("igraph", "assortnet", "rnetcarto"), quietly = TRUE)) {
    stop(
      "Packages \"igraph\", \"igraph\", and \"rnetcarto\" must be installed to use this function.",
      call. = FALSE
    )
  }
  possible_algorithms <- c("netcarto", "fast_greedy", "leading_eigen", "louvain", "walktrap") # there are others
  if(!alg %in% possible_algorithms){
    stop(
      "alg must take one of the following values: \"netcarto\", \"fast_greedy\", \"leading_eigen\", \"louvain\", or \"walktrap\""
    )
  }

  if(is.matrix(x)){

    g_obs <- igraph::graph_from_adjacency_matrix(x, weighted = T, mode = "undirected", diag = F)
    g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
    am_obs <- as.matrix(igraph::get.adjacency(g_obs, type = "upper", attr = "weight"))

  } else if(class(x) == "igraph"){

    g_obs <- x
    g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
    am_obs <- as.matrix(igraph::get.adjacency(g_obs, type = "upper", attr = "weight"))

  } else {
    stop("Object must either be a square adjacency matrix of class matrix or an igraph object.")
  }

  if( length( igraph::E(g_obs) ) >= 2 & alg == "netcarto"){

    df <- rnetcarto::netcarto(am_obs)[[1]]
    nms <- igraph::V(g_obs)$name
    nmsNC <- df$name
    if(all(nms %in% nmsNC)){
      (colorOrder <- df$module[match(nms, nmsNC)])
    }else{
      colorOrder <- df$module[match(nms, nmsNC)]
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # use [1] not [j]
      }
    }
    igraph::V(g_obs)$membership <- colorOrder
    qrel <- assortnet::assortment.discrete(graph = am_obs, types = colorOrder, weighted = T)$r
    r <- ifelse(qrel %in% "NaN", 0, qrel)

  } else if(length( igraph::E(g_obs) ) >= 2 & alg %in% possible_algorithms[2:5]){

    foo <- eval(parse(text = paste0("igraph::cluster_", alg)))

    community_object <- foo(g_obs, weights = igraph::E(g_obs)$weight)

    nms <- igraph::V(g_obs)$name
    nmsCL <- community_object$names
    if(all(nms %in% nmsCL)){
      (colorOrder <- community_object$membership[match(nms, nmsCL)])
    }else{
      colorOrder <- community_object$membership[match(nms, nmsCL)]
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # use [1] not [j]
      }
    }
    igraph::V(g_obs)$membership <- colorOrder
    qrel <- assortnet::assortment.discrete(graph = am_obs, types = colorOrder, weighted = T)$r
    r <- ifelse(qrel %in% "NaN", 0, qrel)
  }else if(length(igraph::E(g_obs)) == 1){

    head <- names(which(rowSums(am_obs) > 0))
    tail <- names(which(colSums(am_obs) > 0))
    colorOrder <- ifelse(igraph::V(g_obs)$name %in% c(head, tail), 0, NA)
    for(j in 1:sum(is.na(colorOrder))){
      colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # use [1] not [j]
    }
    igraph::V(g_obs)$membership <- colorOrder
    r <- 0

  }else if(length(igraph::E(g_obs)) == 0){

    igraph::V(g_obs)$membership <- 1:length(igraph::V(g_obs))
    r <- NA_real_
  }
# no way to make invisible with base r plotting?
  invisible(igraph::plot.igraph(g_obs, layout = igraph::layout.fruchterman.reingold(g_obs),
                 edge.width = igraph::E(g_obs)$weight*4,
                 vertex.color = igraph::V(g_obs)$membership,
                 main = paste0("Assort. Coef. = ", round(r, 2)))
            )

}
