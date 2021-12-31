#' Simulate a sampling regime and calculate network modularity.
#'
#' @param adjmat I'm not sure why this is here... should just build it from the
#'   graph
#' @param graph The true network to sample
#' @param missingness The proportion of the nodes in the real network that will
#'   be missed in sampling
#' @param propGPS The proportion of high resolution data to use
#' @param gps_freq The observation frequency (observations per sampling period)
#'   of the high res data
#' @param vhf_freq Observations per sampling period of the low res data
#' @param regime Whether to sample modules randomly or impose 'evenness'
#'
#' @return g_obs, the graph of the observed network
#' @export
#'
#' @examples # nothing yet
sample_graph <- function(graph, missingness, propGPS, gps_freq, vhf_freq, regime = "random"){
  if (!requireNamespace(c("igraph", "dplyr", "rnetcarto"), quietly = TRUE)) {
    stop(
      "Packages \"igraph\", \"dplyr\", and \"rnetcarto\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if(class(graph) != "igraph"){ stop("graph needs to be an igraph object.", call. = FALSE) }

  m <- missingness
  adjmat = as_adjacency_matrix(graph, attr = "weight", type = "both", sparse = F)
  netSize <- ncol(adjmat)

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
    size = ceiling( (1 - m) * netSize ) # the number of animals to return
    even_sampler <- floor(size/ngroups)

    tryCatch({
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
        initial_sampling <- id_df %>% dplyr::group_by(group) %>%
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

      if(exists("out")){ # start exists out
        grab <- out$ids
        if(length(grab) > 2){ # start grab if()
          am <- adjmat[which(rownames(adjmat) %in% grab), which(colnames(adjmat) %in% grab)]
          g2 <- igraph::graph_from_adjacency_matrix(am, weighted = T, mode = "undirected", diag = F)
          ed <- igraph::get.data.frame(g2) # weights are all super small when they're each in a different module

          if(nrow(ed) > 1){
            nNodes_sim <- length(grab)
            nGPS <- ceiling(nNodes_sim * propGPS)
            nVHF <- nNodes_sim - nGPS

            ofd <- data.frame(id = row.names(am)) %>%
              dplyr::mutate(obs_freq = ifelse(as.numeric(row.names(.)) <= nGPS, gps_freq, vhf_freq),
                     n_obs = obs_freq * 365)
            ed$fromObs <- ofd$n_obs[match(ed$from, ofd$id)]
            ed$toObs <- ofd$n_obs[match(ed$to, ofd$id)]
            ed$minObs <- pmin(ed$fromObs, ed$toObs)
            ed$sim_weight <- stats::rbinom(n = nrow(ed), size = ed$minObs, p = ed$weight) / ed$minObs # when they're all in diff mods, with low weights, simweight has a lot of zeros

            g_obs <- igraph::graph_from_data_frame(ed[,c("from", "to", "sim_weight")], directed = FALSE)
            g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$sim_weight==0))
            am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "sim_weight") %>% as.matrix()

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
            return(g_obs)
          }
        }
      }
    }, error = function(e){print(paste("i, j, k = ", i, j, k, sep = "  "))} )
  }

  if(regime == "random"){
    grab <- sample(1:netSize, size = ceiling( (1 - m) * netSize ), replace = F)
    if(length(grab) > 2){
      am <- adjmat[grab, grab]
      g2 <- igraph::graph_from_adjacency_matrix(am, weighted = T, mode = "undirected", diag = F)
      ed <- igraph::get.data.frame(g2)

      nNodes_sim <- length(grab)
      nGPS <- ceiling(nNodes_sim * propGPS)
      nVHF <- nNodes_sim - nGPS

      ofd <- data.frame(id = row.names(am)) %>%
        dplyr::mutate(obs_freq = ifelse(as.numeric(row.names(.)) <= nGPS, gps_freq, vhf_freq),
               n_obs = obs_freq * 365)
      ed$fromObs <- ofd$n_obs[match(ed$from, ofd$id)]
      ed$toObs <- ofd$n_obs[match(ed$to, ofd$id)]
      ed$minObs <- pmin(ed$fromObs, ed$toObs)
      ed$sim_weight <- stats::rbinom(n = nrow(ed), size = ed$minObs, p = ed$weight) / ed$minObs

      g_obs <- igraph::graph_from_data_frame(ed[,c("from", "to", "sim_weight")], directed = FALSE)
      g_obs <- igraph::delete_edges(g_obs, which(igraph::E(g_obs)$sim_weight==0))
      am_obs <- igraph::get.adjacency(g_obs, type = "upper", attr = "sim_weight") %>% as.matrix()

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
      return(g_obs)
    }
  }

}
