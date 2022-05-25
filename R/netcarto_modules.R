#' Return vector of group membership
#'
#' @inheritParams modulr_plot
#'
#' @return a named vector of module memberships matching the named adjacency
#'   matrix or igraph graph object (igraph::V(graph)$name)
#' @export
#' @importFrom rlang .data
#' @examples
#' adjmat <- matrix(sample(seq(0, 1, .01), 16), nrow = 4)
#' diag(adjmat) <- 0
#' adjmat[lower.tri(adjmat)] <- 0
#' rownames(adjmat) <- colnames(adjmat) <- paste0("Animal_", 1:4)
#' netcarto_modules(adjmat)
netcarto_modules <- function(x) {
    if (!requireNamespace(c("igraph","rnetcarto"), quietly = TRUE)) {
      stop(
        "Packages \"igraph\" and \"rnetcarto\" must be installed to use this function.",
        call. = FALSE
      )
    }

    if(is.matrix(x)){

      g_obs <- igraph::graph_from_adjacency_matrix(x, weighted = T, mode = "undirected", diag = F)
      g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
      am_obs <- as.matrix(igraph::get.adjacency(g_obs, type = "upper", attr = "weight"))

    } else if(class(x) == "igraph"){

      g_obs <- x

      if(!is.null(igraph::E(g_obs)$sim_weight)){
        g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$sim_weight==0))
        am_obs <- as.matrix(igraph::get.adjacency(g_obs, type = "upper", attr = "sim_weight"))
      }else{
        g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
        am_obs <- as.matrix(igraph::get.adjacency(g_obs, type = "upper", attr = "weight"))
      }

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
          colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1
        }
      }
      igraph::V(g_obs)$membership <- colorOrder

    }else if(length(igraph::E(g_obs)) == 1){

      head <- names(which(rowSums(am_obs) > 0))
      tail <- names(which(colSums(am_obs) > 0))
      colorOrder <- ifelse(igraph::V(g_obs)$name %in% c(head, tail), 0, NA)
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1
      }
      igraph::V(g_obs)$membership <- colorOrder

    }else if(length(igraph::E(g_obs)) == 0){

      igraph::V(g_obs)$membership <- 1:length(igraph::V(g_obs))
    }

  y <- stats::setNames(igraph::V(g_obs)$membership, c(igraph::V(g_obs)$name))

  return(y[order(factor(names(y)))])

  }
