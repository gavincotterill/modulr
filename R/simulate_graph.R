#' Simulate the true simple ratio index values for each dyad in a network.
#'
#' @param n_animals The number of nodes to include in the network.
#' @param n_groups The number of modules in the network.
#' @param time_to_leave The average number of days spent in the home group.
#' @param time_to_return The average number of days spent away from the home
#'   group.
#' @param samples_per_day The number of samples per day.
#' @param sampling_duration The number of days in the sampling period.
#'
#' @return sim_igraph, an igraph graph object.
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' g <- simulate_graph(n_animals = 25,
#'                     n_groups = 4,
#'                     time_to_leave = 5,
#'                     time_to_return = 2,
#'                     samples_per_day = 1,
#'                     sampling_duration = 7)
#' @seealso \code{\link{plot_simulated_graph}}
simulate_graph <- function(n_animals,
                           n_groups,
                           time_to_leave,
                           time_to_return,
                           samples_per_day,
                           sampling_duration) {

  if (!requireNamespace(c("igraph"), quietly = TRUE)) {
    stop(
      "Package \"igraph\"  must be installed to use this function.",
      call. = FALSE
    )
  }

  animal_list <- animal_sample_df <- vector("list", length = n_animals)  # build storage objects

  if( length(time_to_leave) == length(time_to_return) & length(time_to_return) == 1 ) {
    for(a in 1:n_animals){  # for-loop over animals.
      animal_list[[a]] <- simulate_animal(time_to_leave_group = time_to_leave,
                                          time_to_return_to_group = time_to_return,
                                          n_groups = n_groups,
                                          samples_per_day = samples_per_day,
                                          sampling_duration = sampling_duration)
      animal_sample_df[[a]] <- cbind(animal_list[[a]]$samples,
                                     id = rep(paste("Animal_", a, sep = ""), nrow(animal_list[[a]]$samples)))
      names(animal_sample_df)[a] <- (paste0("Animal_", a))
    }

  } else if(length(time_to_leave) == length(time_to_return) & length(time_to_return) == n_animals){
    for(a in 1:n_animals){  # for-loop over animals.
      animal_list[[a]] <- simulate_animal(time_to_leave_group = time_to_leave[a],
                                          time_to_return_to_group = time_to_return[a],
                                          n_groups = n_groups,
                                          samples_per_day = samples_per_day,
                                          sampling_duration = sampling_duration)
      animal_sample_df[[a]] <- cbind(animal_list[[a]]$samples,
                                     id = rep(paste("Animal_", a, sep = ""), nrow(animal_list[[a]]$samples)))
      names(animal_sample_df)[a] <- (paste0("Animal_", a))
    }
  } else if( length(time_to_leave) != length(time_to_return) | !length(time_to_leave) %in% c(1, n_animals) | !length(time_to_return) %in% c(1, n_animals)){
    stop(
      "\'time_to_leave\' and \'time_to_return\' need to have the same length. That length needs to either be one or equal to \'n_animals\'.",
      call. = FALSE
    )
  }



  mem_df <- data.frame(ids = names(animal_sample_df),
                       membership = unlist(lapply(animal_list, function(x) x[['animals_home']])))

  # collect output into a common GPS-like dataframe.
  samples_out <- as.data.frame(do.call("rbind", animal_sample_df))
  names(samples_out) <- c("time", "location", "id")

  # build SRI adjacency matrix -----
  dyads <- expand.grid(1:n_animals, 1:n_animals)
  dyads <- dyads[which(dyads[, 1] != dyads[, 2]), ] # this still includes eg A--B and B--A, but that shouldn't add much time

  adj_mat <- matrix(NA, nrow = n_animals,ncol = n_animals)

  for(d in 1:nrow(dyads)){
    anim1 <- subset(samples_out, id == levels(factor(samples_out$id))[dyads[d, 1]])
    anim2 <- subset(samples_out, id == levels(factor(samples_out$id))[dyads[d, 2]])
    ### Currently SRI is not continuous-- based on sample frequency, not instantaneous:
    together <- length(which(anim1$location == anim2$location))
    adj_mat[dyads[d, 1], dyads[d, 2]] <- together / nrow(anim1)
  }

  rownames(adj_mat) <- colnames(adj_mat) <- levels(factor(samples_out$id))
  diag(adj_mat) <- rep(0, n_animals) # zero diagonal and lower tri
  adj_mat[lower.tri(adj_mat)] <- 0

  # convert to igraph object and plot with f-r layout
  sim_igraph <- igraph::graph_from_adjacency_matrix(adj_mat, weighted = T, mode = "undirected")
  sim_igraph <- igraph::delete.edges(sim_igraph, which(igraph::E(sim_igraph)$weight == 0))

  nms <- igraph::V(sim_igraph)$name
  nmsNC <- mem_df$ids
  membership <- mem_df$membership[match(nms, nmsNC)]

  igraph::V(sim_igraph)$membership <- membership

  return(sim_igraph)

}
