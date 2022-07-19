#' Simulate network sampling, monitoring and perform community detection based on the simple ratio index.
#'
#' @param graph The true network to sample.
#' @param sample_nNodes The number of individuals to sample.
#' @param sampling_duration The number of days to monitor sampled individuals.
#' @param prop_hi_res The proportion of sampled nodes to monitor with high resolution.
#' @param hi_res The observation frequency of the high resolution individuals, expressed as the number of observations per day.
#' @param lo_res The observation frequency of the low resolution individuals, expressed as the number of observations per day.
#' @param regime The sampling regime to use: options include "random", "even", "grab-two".
#' @param alg The community detection algorithm to use, one of: "netcarto", "fast_greedy", "label_prop", "leading_eigen", "louvain", "spinglass", "walktrap".
#'
#' @return An `igraph` graph of the sampled network where edge weights ('attr' = "weight") is the simple ratio index.
#' @export
#'
#' @examples
#' \donttest{
#' library(igraph)
#' data("real_networks")
#' data("study_design")
#' g_obs <- sample_graph(
#'   graph = real_networks[[1]],
#'   sample_nNodes = ceiling(0.5 * length(igraph::V(real_networks[[1]]))),
#'   prop_hi_res = 1,
#'   regime = "grab-two",
#'   alg = "fast_greedy")
#'
#'}
sample_graph <- function(graph, sample_nNodes, sampling_duration, prop_hi_res = 1, hi_res = 1, lo_res = 1/7, regime = "grab-two", alg = "fast_greedy"){
  if (!requireNamespace(c("igraph", "dplyr"), quietly = TRUE)) {stop("Packages \"igraph\" and \"dplyr\" must be installed to use this function.",call. = FALSE)}
  if(!regime %in% c("grab-two", "random", "even")){ stop("regime must take one of the following values: \"grab-two\", \"even\",\"random\".")}

  possible_algorithms <- c("netcarto", "fast_greedy", "label_prop", "leading_eigen", "louvain", "spinglass", "walktrap", "optimal", "leiden")
  if(!alg %in% possible_algorithms){stop("alg must take one of the following values: \"netcarto\", \"fast_greedy\",\"label_prop\", \"leading_eigen\", \"louvain\", \"spinglass\", or \"walktrap\"")}
  if(alg %in% c("netcarto") & !requireNamespace(c("rnetcarto"), quietly = TRUE)){stop( "Package \"rnetcarto\" must be installed to use the netcarto community detection algorithm.")}
  if(sample_nNodes <= 2){stop("sample_nNodes must be greater than 2 in order to make a graph")}
  if(class(graph) != "igraph"){ stop("graph needs to be an igraph object.", call. = FALSE) }

  adjmat = igraph::as_adjacency_matrix(graph, attr = "weight", type = "both", sparse = FALSE)
  netSize <- ncol(adjmat)

  if(regime == "grab-two"){
    membership <- igraph::V(graph)$membership    # pull the stored membership vector
    id_df <- data.frame(ids = igraph::V(graph)$name, group = membership)

    grouped_by_size <- id_df %>%
      dplyr::group_by(.data$group) %>%
      dplyr::add_tally() %>%
      dplyr::ungroup() %>%
      dplyr::arrange(-.data$n, .data$group)

    ngroups = length(unique(membership)) # number of modules

    # condition 1: if you need fewer animals than there are ngroups * 2
    if(sample_nNodes <= (ngroups * 2) & sample_nNodes %% 2 == 0){ # and is even,
      initial_sampling <- grouped_by_size %>%
        dplyr::group_by(.data$group) %>%
        dplyr::slice(1:2) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(-.data$n, .data$group) %>%
        dplyr::slice(1:sample_nNodes) %>% # you won't sample every group. # this is the only difference from the second condition
        dplyr::select(-.data$n)
      remainder <- sample_nNodes - nrow(initial_sampling)

      if(remainder == 0){
        out <- initial_sampling
      }else if(remainder == 1){
        unsampled_df <- id_df %>%
          dplyr::filter(!.data$ids %in% initial_sampling$ids & .data$group %in% initial_sampling$group)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        unsampled_df[random_vec,]
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      }else if(remainder > 1){
        unsampled_df <- id_df %>%
          dplyr::filter(!.data$ids %in% initial_sampling$ids)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        unsampled_df[random_vec,]
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      }
      # condition 2:
    } else if(sample_nNodes < (ngroups * 2) & sample_nNodes %% 2 != 0){ # fewer animals then groups*2 and odd
      initial_sampling <- grouped_by_size %>%
        dplyr::group_by(.data$group) %>%
        dplyr::slice(1:2) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(-.data$n, .data$group) %>%
        dplyr::slice(1:(sample_nNodes - 1)) %>%
        dplyr::select(-.data$n)
      remainder <- sample_nNodes - nrow(initial_sampling)
      if(remainder == 0){
        out <- initial_sampling
      }else if(remainder == 1){
        # first try to add on to existing groups
        unsampled_df <- id_df %>%
          dplyr::filter(!.data$ids %in% initial_sampling$ids & .data$group %in% initial_sampling$group)
        # if that's not possible, draw from any other group
        if(nrow(unsampled_df) == 0){
          unsampled_df <- id_df %>%
            dplyr::filter(!.data$ids %in% initial_sampling$ids)
        }
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      }else if(remainder > 1){
        unsampled_df <- id_df %>%
          dplyr::filter(!.data$ids %in% initial_sampling$ids)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      }
      # condition 3:
    }else if(sample_nNodes > (ngroups * 2)){ # if you need more animals than twice the number of groups
      initial_sampling <- grouped_by_size %>%
        dplyr::group_by(.data$group) %>%
        dplyr::slice(1:2) %>%
        dplyr::ungroup()
      remainder <- sample_nNodes - nrow(initial_sampling)
      if(remainder > 0){
        unsampled_df <- grouped_by_size %>%
          dplyr::filter(!.data$ids %in% initial_sampling$ids)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      } else { out <- initial_sampling }
    }# end conditions
    grab <- out$ids
  }# end grab-two

  if(regime == "even"){
    membership <- igraph::V(graph)$membership
    id_df <- data.frame(ids = igraph::V(graph)$name, group = membership) %>%
      dplyr::group_by(.data$group) %>%
      dplyr::mutate(count = dplyr::n()) %>%
      dplyr::arrange(desc(.data$count)) %>%
      dplyr::ungroup()

    nGroups <- length(unique(membership))

    if(floor(sample_nNodes / nGroups) == 0){ # if this is true you can only sample one individual per group, not all groups represented
      groups_to_sample <- id_df %>%
        dplyr::group_by(.data$group) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::slice(1:sample_nNodes) %>%
        dplyr::select(.data$group)

      sample <- id_df %>%
        dplyr::filter(group %in% groups_to_sample$group) %>%
        dplyr::group_by(.data$group) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$ids)

    }else if(floor(sample_nNodes / nGroups) > 0){ # if this is true you can at least sample one individual per group

      initial_preference <- ceiling(sample_nNodes / nGroups)

      initial_sample <- id_df %>%
        dplyr::group_by(.data$group) %>%
        dplyr::slice_sample(n = initial_preference) %>%
        dplyr::ungroup()

      number_to_remove <- nrow(initial_sample) - sample_nNodes

      if(number_to_remove > 0){
        rows_to_remove <- initial_sample %>%
          dplyr::group_by(.data$group) %>%
          dplyr::slice_sample(n = 1) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(desc(.data$count)) %>%
          dplyr::slice(1:number_to_remove)

        sample <- initial_sample %>%
          dplyr::filter(!.data$ids %in% rows_to_remove$ids)

      }else if(number_to_remove == 0){
        sample <- initial_sample

      }else if(number_to_remove < 0){
        remaining_ids <- id_df %>%
          dplyr::filter(!.data$ids %in% initial_sample$ids)

        rows_to_add <- remaining_ids[sample(nrow(remaining_ids), abs(number_to_remove)),]
        sample <- rbind(initial_sample, rows_to_add)
      }
    }
    grab <- sample$ids
  } # end even

  if(regime == "random"){
    grab <- sample(1:netSize, size = sample_nNodes, replace = F)
  }

  if(length(grab) != sample_nNodes){
    stop(paste0("sampled ", length(grab)," not ", sample_nNodes))
  }

  am <- adjmat[grab, grab]
  g2 <- igraph::graph_from_adjacency_matrix(am, weighted = T, mode = "undirected", diag = F)
  ed <- igraph::get.data.frame(g2) # nodes can get silently dropped,

  ids <- igraph::V(g2)$name # so make sure that doesn't happen
  dyads <- data.frame(t(combn(ids, 2)))
  names(dyads) <- c("from", "to")
  ed <- dplyr::left_join(dyads, ed, by = c("from", "to"))
  ed[is.na(ed)] <- 0

  # allocate hi vs lo res:
  nGPS <- ceiling(sample_nNodes * prop_hi_res)
  if(nGPS < sample_nNodes){
    nVHF <- sample_nNodes - nGPS
  }else if(nGPS == sample_nNodes){
    nVHF <- 0
  }else if(nGPS > sample_nNodes){
    print("error line 262")
  }

  # calculate SRIs
  ofd <- data.frame(id = row.names(am)) %>%
    dplyr::mutate(obs_freq = ifelse(as.numeric(row.names(.)) <= nGPS, hi_res, lo_res),
           n_obs = ceiling(.data$obs_freq * sampling_duration))
  ed$fromObs <- ofd$n_obs[match(ed$from, ofd$id)]
  ed$toObs <- ofd$n_obs[match(ed$to, ofd$id)]
  ed$minObs <- pmin(ed$fromObs, ed$toObs)
  ed$sim_weight <- stats::rbinom(n = nrow(ed), size = ed$minObs, p = ed$weight) / ed$minObs
  ed$weight <- ed$sim_weight # work around to rename sim_weight to weight

  # build graph and adjmat from sampled SRI network
  g_obs <- igraph::graph_from_data_frame(ed[,c("from", "to", "weight")], directed = FALSE)
  g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$weight==0))
  am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "weight") %>% as.matrix()

  # perform community detection
  # condition 1: 2 or more edges with netcarto
  if( length( igraph::E(g_obs) ) >= 2 & alg == "netcarto"){
    df <- rnetcarto::netcarto(am_obs)[[1]]
    nms <- igraph::V(g_obs)$name
    nmsNC <- df$name
    # netcarto will silently drop disconnected (solo) nodes
    if(all(nms %in% nmsNC)){
      (colorOrder <- df$module[match(nms, nmsNC)])
    }else{ # if anyone is unaccounted for, give them the next available group number
      colorOrder <- df$module[match(nms, nmsNC)]
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # note, is supposed to use [1] not [j]
      }
    }
    igraph::V(g_obs)$membership <- colorOrder
  # condition 2: 2 or more edges using a different algorithm
  } else if(length( igraph::E(g_obs) ) >= 2 & alg %in% possible_algorithms[2:length(possible_algorithms)]){
    foo <- eval(parse(text = paste0("igraph::cluster_", alg))) # try adding igraph:: in here to avoid error
    community_object <- foo(g_obs, weights = igraph::E(g_obs)$weight)
    nms <- igraph::V(g_obs)$name
    nmsCL <- community_object$names
    # make sure single node modules get a group number
    if(all(nms %in% nmsCL)){
      (colorOrder <- community_object$membership[match(nms, nmsCL)])
    }else{
      colorOrder <- community_object$membership[match(nms, nmsCL)]
      for(j in 1:sum(is.na(colorOrder))){
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1 # use [1] not [j]
      }
    }
    igraph::V(g_obs)$membership <- colorOrder
  # condition 3: if there's only one edge, put the dyad in a group and everyone else in their own
  }else if(length(igraph::E(g_obs)) == 1){
    head <- names(which(rowSums(am_obs) > 0))
    tail <- names(which(colSums(am_obs) > 0))
    colorOrder <- ifelse(igraph::V(g_obs)$name %in% c(head, tail), 0, NA)
    for(j in 1:sum(is.na(colorOrder))){
      colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rm = T) + 1
    }
    igraph::V(g_obs)$membership <- colorOrder
  # condition 4: if all nodes are disconnected
  }else if(length(igraph::E(g_obs)) == 0){
    # then they're each in their own group by default
    igraph::V(g_obs)$membership <- 1:length(igraph::V(g_obs))
  }
  return(g_obs)
}
