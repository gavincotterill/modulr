#' Takes a square adjacency matrix or igraph graph and returns a plot of the graph, color-coded by modules detected by `rnetcarto`
#'
#' @param x, a named square adjacency matrix or named igraph graph. Intended for weighted, undirected graphs.
#'
#' @return a plot
#' @export
#'
#' @examples # add at a later date
assortr_plot <- function(x){
  if (!requireNamespace(c("igraph", "dplyr", "assortnet", "rnetcarto"), quietly = TRUE)) {
    stop(
      "Packages \"igraph\", \"dplyr\", \"igraph\", and \"rnetcarto\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if(is.matrix(x)){

    g_obs <- igraph::graph_from_adjacency_matrix(x, weighted = T, mode = "undirected", diag = F)
    g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
    am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "weight") %>%
      as.matrix()

  } else if(class(x) == "igraph"){

    g_obs <- x
    g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
    am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "weight") %>%
      as.matrix()
  } else {
    stop("Object must either be a square adjacency matrix of class matrix or an igraph object.")
  }

  if( length( igraph::E(g_obs) ) >= 2 ){

    df <- rnetcarto::netcarto(am_obs)[[1]]
    nms <- igraph::V(g_obs)$name
    nmsNC <- df$name
    if(all(nms %in% nmsNC)){
      (colorOrder <- df$module[match(nms, nmsNC)])
    }else{
      colorOrder <- df$module[match(nms, nmsNC)]
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # this is counterintuitive... but use a [1] instead of [j]
      }
    }
    igraph::V(g_obs)$membership <- colorOrder

  }else if(length(igraph::E(g_obs)) == 1){

    head <- names(which(rowSums(am_obs) > 0))
    tail <- names(which(colSums(am_obs) > 0))
    colorOrder <- ifelse(igraph::V(g_obs)$name %in% c(head, tail), 0, NA)
    for(j in 1:sum(is.na(colorOrder))){
      colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # this is counterintuitive... but use a [1] instead of [j]
    }
    igraph::V(g_obs)$membership <- colorOrder

  }else if(length(igraph::E(g_obs)) == 0){

    igraph::V(g_obs)$membership <- 1:length(igraph::V(g_obs))

  }

  return(plot(g_obs, layout = igraph::layout.fruchterman.reingold(g_obs),
              edge.width = igraph::E(g_obs)$weight*4,
              vertex.color = igraph::V(g_obs)$membership))

}
