#' Simulate a sampling regime and calculate network modularity.
#'
#' @param graph The true network to sample
#' @param sample_nNodes the number of individuals to sample
#' @param prop_hi_res The proportion of high resolution data to use
#' @param hi_res The observation frequency (observations per sampling period)
#'   of the high res data
#' @param lo_res Observations per sampling period of the low res data
#' @param regime Whether to sample modules randomly or impose 'evenness'. options include "random", "even", "better"
#' @param alg the clustering algorithm to use, one of: "fast_greedy", "louvain", "leading_eigen", "walktrap", or "netcarto"
#'
#' @return g_obs, the graph of the observed network
#' @export
#'
#' @examples
#' data("real_networks")
#' data("study_design")
#'
#' g_obs <- sample_graph(
#'   graph = real_networks[[1]],
#'   sample_nNodes = ceiling(0.5 * length(igraph::V(real_networks[[1]]))),
#'   prop_hi_res = 1,
#'   regime = "better")
#'
#' am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "sim_weight") %>%
#'   as.matrix()
#'
#' study_design[1,] %>%
#'   dplyr::mutate(prop_hi_res = 1,
#'                 nNodes_sim = ncol(am_obs),
#'                 nModules_sim = length(unique(igraph::V(g_obs)$membership)),
#'                 qrel_sim = assortnet::assortment.discrete(am_obs, types = igraph::V(g_obs)$membership, weighted = T)$r)
#'
sample_graph <- function(graph, sample_nNodes, prop_hi_res = 1, hi_res = 30/365, lo_res = 5/365, regime = "better", alg = "netcarto"){
  if (!requireNamespace(c("igraph", "dplyr", "rnetcarto"), quietly = TRUE)) {
    stop(
      "Packages \"igraph\", \"dplyr\", and \"rnetcarto\" must be installed to use this function.",
      call. = FALSE
    )
  }
  possible_algorithms <- c("netcarto", "fast_greedy", "leading_eigen", "louvain", "walktrap")
  if(!alg %in% possible_algorithms){
    stop(
      "alg must take one of the following values: \"netcarto\", \"fast_greedy\", \"leading_eigen\", \"louvain\", or \"walktrap\""
    )
  }
  if(sample_nNodes <= 2){
    stop(
      "sample_nNodes must be greater than 2 in order to make a graph"
    )
  }
  if(class(graph) != "igraph"){ stop("graph needs to be an igraph object.", call. = FALSE) }

  # sample_nNodes <- sample_nNodes # what's this for?
  adjmat = igraph::as_adjacency_matrix(graph, attr = "weight", type = "both", sparse = F)
  netSize <- ncol(adjmat)

  if(regime == "better"){
    membership <- igraph::V(graph)$membership    # pull the stored membership vector
    id_df <- data.frame(ids = igraph::V(graph)$name, group = membership)

    grouped_by_size <- id_df %>%
      dplyr::group_by(group) %>%
      dplyr::add_tally() %>%
      dplyr::ungroup() %>%
      dplyr::arrange(-n) %>%
      dplyr::filter(n != 1) # drop single node modules

    min_sample <- id_df %>% # smallest module size
      dplyr::group_by(group) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::select(n) %>%
      min()

    ngroups = length(unique(membership)) # number of modules
    size = sample_nNodes # the number of animals to return (number needed)
    even_sampler <- floor(size/ngroups)

    # if you need fewer animals than there are ngroups * 2
    if(size <= (ngroups * 2) & size %% 2 == 0){ # and is even,

      out <- grouped_by_size %>%
        dplyr::group_by(group) %>%
        dplyr::sample_n(2) %>%
        dplyr::ungroup() %>%
        dplyr::slice(1:size) %>% # you won't sample every group.
        dplyr::select(-n)

    } else if(size < (ngroups * 2) & size %% 2 != 0){ # fewer animals then groups*2 and odd

      initial_sampling <- grouped_by_size %>%
        dplyr::group_by(group) %>%
        dplyr::sample_n(2) %>%
        dplyr::ungroup() %>%
        dplyr::slice(1:(size - 1)) %>%
        dplyr::select(-n)

      remainder <- size - nrow(initial_sampling)

      if(remainder > 0){ # start inner
        unsampled_df <- id_df %>%
          dplyr::filter(!ids %in% initial_sampling$ids)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      } else { out <- initial_sampling } # end inner

    }else if(size > (ngroups * 2)){ # if you need more animals than twice the number of groups
      initial_sampling <- id_df %>%
        dplyr::group_by(group) %>%
        dplyr::sample_n(ifelse(min_sample < even_sampler, min_sample, even_sampler))

      remainder <- size - nrow(initial_sampling)

      if(remainder > 0){ # begin inner
        unsampled_df <- id_df %>%
          dplyr::filter(!ids %in% initial_sampling$ids)
        random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
        out <- rbind(initial_sampling, unsampled_df[random_vec,])
      } else { out <- initial_sampling } # end inner
    }
    grab <- out$ids
  }

  if(regime == "even"){
    membership <- igraph::V(graph)$membership    # pull the stored membership vector
    id_df <- data.frame(ids = igraph::V(graph)$name, group = membership)

    min_sample <- id_df %>%
      dplyr::group_by(group) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::select(n) %>%
      min()     # return smallest number of nodes per group

    ngroups = length(unique(membership))
    size = sample_nNodes # the number of animals to return
    even_sampler <- floor(size/ngroups)

      if(size > ngroups & even_sampler >= min_sample){ # start sampling if

        initial_sampling <- id_df %>%
          dplyr::group_by(group) %>%
          dplyr::sample_n(min_sample)
        remainder <- size - nrow(initial_sampling)

        if(remainder > 0){ # inner
          unsampled_df <- id_df %>%
            dplyr::filter(!ids %in% initial_sampling$ids)
          random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
          out <- rbind(initial_sampling, unsampled_df[random_vec,])
        } else { out <- initial_sampling } # end inner

      } else if(size > ngroups & even_sampler < min_sample){ # outer contd
        initial_sampling <- id_df %>%
          dplyr::group_by(group) %>%
          dplyr::sample_n(even_sampler)
        remainder <- size - nrow(initial_sampling)

        if(remainder > 0){ # inner
          unsampled_df <- id_df %>%
            dplyr::filter(!ids %in% initial_sampling$ids)
          random_vec <- sample(1:nrow(unsampled_df), remainder, replace = F)
          out <- rbind(initial_sampling, unsampled_df[random_vec,])
        } else { out <- initial_sampling } # end inner

      } else if(size <= ngroups){ # outer contd
        sampling <- id_df %>%
          dplyr::group_by(group) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() # these are randomly sorted by group
        out <- sampling[1:size,]
      } # end sampling if()

    grab <- out$ids

    }

  if(regime == "random"){
    grab <- sample(1:netSize, size = sample_nNodes, replace = F)
    }

  # if(any(grab > netSize)){print("this is the problem")}

  if(length(grab) > 2){
    am <- adjmat[grab, grab]
    g2 <- igraph::graph_from_adjacency_matrix(am, weighted = T, mode = "undirected", diag = F)
    ed <- igraph::get.data.frame(g2)

    # nNodes_sim <- length(grab) # redundant, replace with sample_nNodes
    # nGPS <- ceiling(nNodes_sim * prop_hi_res)
    # nVHF <- nNodes_sim - nGPS

    ## is the problem here?
    nGPS <- ceiling(sample_nNodes * prop_hi_res)
    if(nGPS < sample_nNodes){
      nVHF <- sample_nNodes - nGPS
    }else if(nGPS == sample_nNodes){
      nVHF <- 0
    }else if(nGPS > sample_nNodes){
      print("error line 209")
    }

    ofd <- data.frame(id = row.names(am)) %>%
      dplyr::mutate(obs_freq = ifelse(as.numeric(row.names(.)) <= nGPS, hi_res, lo_res),
             n_obs = obs_freq * 365)
    ed$fromObs <- ofd$n_obs[match(ed$from, ofd$id)]
    ed$toObs <- ofd$n_obs[match(ed$to, ofd$id)]
    ed$minObs <- pmin(ed$fromObs, ed$toObs)
    ed$sim_weight <- stats::rbinom(n = nrow(ed), size = ed$minObs, p = ed$weight) / ed$minObs

    g_obs <- igraph::graph_from_data_frame(ed[,c("from", "to", "sim_weight")], directed = FALSE)
    g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$sim_weight==0))
    am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "sim_weight") %>% as.matrix()

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

      foo <- eval(parse(text = paste0("cluster_", alg)))

      community_object <- foo(g_obs, weights = E(g_obs)$weight)

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
        colorOrder[is.na(colorOrder)][1] <- max(colorOrder, na.rmuse_ = T) + 1 # this is counterintuitive... but use a [1] instead of [j]
      }
      igraph::V(g_obs)$membership <- colorOrder

    }else if(length(igraph::E(g_obs)) == 0){

      igraph::V(g_obs)$membership <- 1:length(igraph::V(g_obs))

    }
    return(g_obs)
  }

}
