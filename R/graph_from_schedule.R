#' Create undirected, weighted 'igraph' graph from a continuous-time description of animal movements.
#'
#' @param schedule object from simulate_schedule()
#' @return an undirected, weighted 'igraph' graph where weights are the proportion
#' of time dyads spent together.
#' @export
#' @examples
#' \donttest{
#' g <- graph_from_schedule(schedule = obj)
#'}
graph_from_schedule <- function(schedule) {

  ids <- names(schedule)
  grp_mem <- stringr::str_extract(ids, "\\d{1,}(?=_)")
  mem_df <- data.frame(ids = ids,
                       membership = grp_mem)

  vals <- unique(c(ids, ids))
  dyads <- data.frame(t(combn(vals, 2)))
  names(dyads) <- c("Var1", "Var2")
  dyads$ew <- NA

  n_animals <- length(schedule)

  adj_mat <- matrix(NA, nrow = n_animals, ncol = n_animals)
  row.names(adj_mat) <- colnames(adj_mat) <- ids

  for(d in 1:nrow(dyads)){

    t1 <- schedule[[dyads[d,"Var2"]]]
    t2 <- schedule[[dyads[d,"Var1"]]]

    intervals <- data.table::foverlaps(t1, t2) %>%
      dplyr::mutate(start_max = pmax(.data$start, .data$i.start),
                    end_min = pmin(.data$end, .data$i.end),
                    together = ifelse(.data$state == .data$i.state, 1, 0)) %>%
      na.omit() %>%
      dplyr::arrange(end_min) # make sure the last row has the max value for end_min

    g_int <- data.frame(intervals) %>%
      dplyr::select(state, i.state,start_max, end_min, together)

    together <- g_int[,c(1,3:5)] %>%
      dplyr::mutate(id = "A") %>%
      rbind(g_int[,2:5]  %>% 'names<-'(names(g_int)[c(1,3:5)]) %>% dplyr::mutate(id = "B"))

    time_overlap <- together %>%
      dplyr::filter(.data$id == "A" & .data$together == 1) %>%
      dplyr::mutate(time = .data$end_min - .data$start_max)

    numer <- sum(time_overlap$time)
    denom <- intervals[[nrow(intervals), "end_min"]]

    if(is.na(numer)){
      edge_weight <- 0
    }else{
      edge_weight <- numer / denom
    }
    dyads$ew[d] <- edge_weight
    adj_mat[dyads[d, 1], dyads[d, 2]] <- dyads$ew[d]
  }

  diag(adj_mat) <- 0
  adj_mat[lower.tri(adj_mat)] <- 0
  adj_mat[is.na(adj_mat)] <- 0

  # convert to igraph object and plot with f-r layout
  sim_igraph <- igraph::graph_from_adjacency_matrix(adj_mat, weighted = TRUE, mode = "undirected")
  sim_igraph <- igraph::delete.edges(sim_igraph, which(igraph::E(sim_igraph)$weight == 0))

  nms <- igraph::V(sim_igraph)$name
  nmsNC <- mem_df$ids
  membership <- mem_df$membership[match(nms, nmsNC)]

  igraph::V(sim_igraph)$membership <- as.numeric(membership)

  # # n_groups <- animal_list[[1]]$inputs$n_groups
  # n_groups <- length(unique(mem_df$membership))
  #
  # if(length(unique(membership)) == n_groups){
  #   igraph::V(sim_igraph)$membership <- membership
  # }else{warning(paste0(length(unique(membership)), " group(s) in simulated network, not ", n_groups, " as requested.
  # Individuals are randomly assigned to groups.
  # At small average group sizes it is possible that fewer groups are returned than desired."), call. = FALSE)}

  return(sim_igraph)

}


