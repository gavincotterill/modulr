#' Simulate Independent Switching Graph
#'
#' Useful for those interested primarily in generating modular network graphs.
#' Equivalent to using simulate_schedule with the independent simulator and then
#' graph_from_schedule, but also includes a discrete-time approximation that is
#' faster for large networks.
#'
#' @param n_animals The number of nodes to include in the network.
#' @param sampler Takes one of c("discrete", "continuous"). Whether group
#' composition should be monitored instantaneously (truth), or sampled in
#' discrete time.
#' @inheritParams simulate_animal
#'
#' @return sim_igraph, an igraph graph object.
#' @export
#'
#' @examples
#' g <- simulate_graph(n_animals = 25,
#'                     n_groups = 4,
#'                     time_to_leave = 5,
#'                     time_to_return = 2,
#'                     travel_time = c(0.001,0.2),
#'                     sampling_duration = 7,
#'                     sampler = "discrete",
#'                     samples_per_day = 1
#'                     )
#' @seealso \code{\link{plot_simulated_graph}}
simulate_graph <- function(n_animals,
                           n_groups,
                           time_to_leave,
                           time_to_return,
                           travel_time,
                           sampling_duration,
                           sampler = "discrete",
                           samples_per_day = 1) {

  if (!requireNamespace(c("igraph"), quietly = TRUE)) { stop("Package \"igraph\"  must be installed to use this function.",call. = FALSE)}
  if (n_groups == 1) {stop( "single module networks are currently not supported -- n_groups must be >= 2", call. = FALSE)}
  if (!sampler %in% c("discrete", "continuous")) {stop("sampler must be either \"discrete\" or \"continuous\".",call. = FALSE) }
  animal_list <- animal_sample_df <- vector("list", length = n_animals)  # build storage objects

  if( length(time_to_leave) == length(time_to_return) & length(time_to_return) == 1 ) {
    for(a in 1:n_animals){  # for-loop over animals.
      animal_list[[a]] <- simulate_animal(time_to_leave = time_to_leave,
                                          time_to_return = time_to_return,
                                          travel_time = travel_time,
                                          n_groups = n_groups,
                                          samples_per_day = samples_per_day,
                                          sampling_duration = sampling_duration)
      animal_sample_df[[a]] <- cbind(animal_list[[a]]$samples,
                                     id = rep(paste(animal_list[[a]]$animals_home, a, sep = "_"),
                                              nrow(animal_list[[a]]$samples)))
      names(animal_sample_df)[a] <- paste(animal_list[[a]]$animals_home, a, sep = "_")
    }

  } else if(length(time_to_leave) == length(time_to_return) & length(time_to_return) == n_animals){
    for(a in 1:n_animals){  # for-loop over animals.
      animal_list[[a]] <- simulate_animal(time_to_leave = time_to_leave[a],
                                          time_to_return = time_to_return[a],
                                          travel_time = travel_time,
                                          n_groups = n_groups,
                                          samples_per_day = samples_per_day,
                                          sampling_duration = sampling_duration)
      animal_sample_df[[a]] <- cbind(animal_list[[a]]$samples,
                                     id = rep(paste(animal_list[[a]]$animals_home, a, sep = "_"),
                                              nrow(animal_list[[a]]$samples)))
      names(animal_sample_df)[a] <- paste(animal_list[[a]]$animals_home, a, sep = "_")

    }
  } else if( length(time_to_leave) != length(time_to_return) | !length(time_to_leave) %in% c(1, n_animals) | !length(time_to_return) %in% c(1, n_animals)){
    stop( "\'time_to_leave\' and \'time_to_return\' need to have the same length. That length needs to either be one or equal to \'n_animals\'.",call. = FALSE)
  }

  if(sampler == "discrete"){
    mem_df <- data.frame(ids = names(animal_sample_df),
                         membership = unlist(lapply(animal_list, function(x) x[['animals_home']])))
    # collect output into a common GPS-like dataframe.
    samples_out <- as.data.frame(do.call("rbind", animal_sample_df))
    names(samples_out) <- c("time", "location", "id")

    # build SRI adjacency matrix -----
    ids <- names(animal_sample_df)
    vals <- unique(c(ids, ids))
    dyads <- data.frame(t(combn(vals, 2)))
    names(dyads) <- c("Var1", "Var2")

    adj_mat <- matrix(NA, nrow = n_animals, ncol = n_animals)
    row.names(adj_mat) <- colnames(adj_mat) <- ids

    for(d in 1:nrow(dyads)){
      anim1 <- samples_out %>% dplyr::filter(id == dyads[d, 1])
      anim2 <- samples_out %>% dplyr::filter(id == dyads[d, 2])
      # anim1 <- subset(samples_out, id == levels(factor(samples_out$id))[dyads[d, 1]])
      # anim2 <- subset(samples_out, id == levels(factor(samples_out$id))[dyads[d, 2]])
      together <- length(which(anim1$location == anim2$location))
      adj_mat[dyads[d, 1], dyads[d, 2]] <- together / nrow(anim1)
    }

    # rownames(adj_mat) <- colnames(adj_mat) <- levels(factor(samples_out$id))
    diag(adj_mat) <- rep(0, n_animals) # zero diagonal and lower tri
    adj_mat[lower.tri(adj_mat)] <- 0
  }


  if(sampler == "continuous"){
    ids <- names(animal_sample_df)
    names(animal_list) <- ids
    # names(animal_list) <- 1:length(animal_list)

    mem_df <- data.frame(ids = names(animal_list),
                         membership = unlist(lapply(animal_list, function(x) x[['animals_home']])))

    vals <- unique(c(ids, ids))
    dyads <- data.frame(t(combn(vals, 2)))
    names(dyads) <- c("Var1", "Var2")
    dyads$ew <- NA

    animals_transformed <- lapply(animal_list, dt_fxn)

    adj_mat <- matrix(NA, nrow = n_animals, ncol = n_animals)
    row.names(adj_mat) <- colnames(adj_mat) <- ids

    for(d in 1:nrow(dyads)){

      t1 <- animals_transformed[[dyads[d,"Var2"]]]
      t2 <- animals_transformed[[dyads[d,"Var1"]]]

      intervals <- data.table::foverlaps(t1, t2) %>%
        dplyr::mutate(start_max = pmax(.data$start, .data$i.start),
                      end_min = pmin(.data$end, .data$i.end),
                      together = ifelse(.data$state == .data$i.state, 1, 0)) %>%
        na.omit()

      g_int <- data.frame(intervals) %>%
        dplyr::select(1,4,7:9)

      together <- g_int[,c(1,3:5)] %>%
        dplyr::mutate(id = "A") %>%
        rbind(g_int[,2:5]  %>% 'names<-'(names(g_int)[c(1,3:5)]) %>% dplyr::mutate(id = "B"))

      time_overlap <- together %>%
        dplyr::filter(.data$id == "A" & .data$together == 1) %>%
        dplyr::mutate(time = .data$end_min - .data$start_max)

      numer <- sum(time_overlap$time)
      # denom <- intervals[nrow(intervals), "end_min"]$end_min #
      # denom <- intervals[nrow(intervals), "end_min"]["end_min"] # throwing an error after travel time incorporated
      # denom <- max(intervals$end_min)
      denom <- intervals[[nrow(intervals), "end_min"]]

      if(is.na(numer)){
        edge_weight <- 0
      }else{
        edge_weight <- numer / denom

      }

      dyads$ew[d] <- edge_weight
      adj_mat[dyads[d, 1], dyads[d, 2]] <- dyads$ew[d]
    }

    # rownames(adj_mat) <- colnames(adj_mat) <- names(animal_list)
    # diag(adj_mat) <- rep(0, n_animals) # zero diagonal and lower tri
    diag(adj_mat) <- 0
    adj_mat[lower.tri(adj_mat)] <- 0
    adj_mat[is.na(adj_mat)] <- 0
  }

  # convert to igraph object and plot with f-r layout
  sim_igraph <- igraph::graph_from_adjacency_matrix(adj_mat, weighted = TRUE, mode = "undirected")
  sim_igraph <- igraph::delete.edges(sim_igraph, which(igraph::E(sim_igraph)$weight == 0))

  nms <- igraph::V(sim_igraph)$name
  nmsNC <- mem_df$ids
  membership <- mem_df$membership[match(nms, nmsNC)]

  if(length(unique(membership)) == n_groups){
    igraph::V(sim_igraph)$membership <- membership
  }else{warning(paste0(length(unique(membership)), " group(s) in simulated network, not ", n_groups, " as requested.
Individuals are randomly assigned to groups.
At very small network sizes it is possible that fewer groups are returned than desired."), call. = FALSE)}

  return(sim_igraph)

}


