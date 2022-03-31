#' ff_fwd
#' @keywords internal
ff_forward <- function(t2, curr_vec, n, mbrs_list, i){
  fwd_inds <- list()
  next_actions <- list()
  for(k in seq_along(curr_vec)){
    fwd_index <- index_forward(t2, "vector", curr_vec[[k]], i)
    fwd_inds[[k]] <- fwd_index
    next_actions[[k]] <- t2[fwd_index, "action"]
    names(next_actions)[k] <- curr_vec[[k]]
  }
  next_locs <- unique(unlist(fwd_inds))
  n_next_locs <- length(next_locs)

  if(any(c("fission", "fission-fusion") %in% next_actions) & n_next_locs == 1){
    already_there <- t2$holding[next_locs] # if there are already groups there, try to make sure they get ordered ascending to match vector column
    if(is.na(already_there)){
      t2$members[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$members[next_locs], unlist(mbrs_list)[[1]])) %>%
        gsub(" ", "/", .)
      t2$holding[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$holding[next_locs], names(mbrs_list))) %>%
        gsub(" ", "-", .)
    }else{
      t2$members[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$members[next_locs], unlist(mbrs_list)[[1]])) %>%
        gsub(" ", "/", .)
      t2$holding[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$holding[next_locs], names(mbrs_list))) %>%
        gsub(" ", "-", .)

      new_order <- stringr::str_split(t2$holding[next_locs], "-")[[1]] %>% as.numeric() %>% base::order()
      t2$holding[next_locs] <- paste(stringr::str_split(t2$holding[next_locs], "-")[[1]][new_order], collapse = "-")
      t2$members[next_locs] <- paste(stringr::str_split(t2$members[next_locs], "/")[[1]][new_order], collapse = "/")
    }
  }

  if(any(c("fission", "fission-fusion") %in% next_actions) & n_next_locs > 1){
    curr_mem_split <- stringr::str_split(t2$members[i], "/")[[1]] # who was there with blank space divider and length = n groups
    curr_mem_vec <- stringr::str_split(paste(curr_mem_split, collapse = "-"), "-")[[1]] # separated individuals for sampling
    groups_in <- mbrs_list
    # use mbrs_list
    split_list <- list()
    for(j in 1:length(groups_in)){
      group_id <- names(groups_in)[[j]]
      home_group <- stringr::str_split(groups_in[[group_id]], "-")[[1]][stringr::str_split(groups_in[[group_id]], "-")[[1]] %in% curr_mem_vec]
      other_group <- unlist(stringr::str_split(unlist(groups_in[!names(groups_in) %in% group_id]), "-"))[unlist(stringr::str_split(unlist(groups_in[!names(groups_in) %in% group_id]), "-")) %in% curr_mem_vec]
      l_m <- length(home_group)
      l_n_m <- length(other_group)
      if(j < n){ # is it safe to use n here?
        split_list[[j]] <- paste( c(sample(home_group, round(l_m * cohesion, 0)),
                                    sample(other_group, round(l_n_m * (1 - cohesion), 0))),
                                  collapse = "-" )
        names(split_list)[j] <- group_id
        curr_mem_vec <- curr_mem_vec[!curr_mem_vec %in% stringr::str_split(paste(unlist(split_list), collapse = "-"), "-")[[1]]]
        # }else if(j == n_grp_prev){
      }else if(j == n){
        split_list[[j]] <- paste(curr_mem_vec, collapse = "-")
        names(split_list)[j] <- group_id
      }
    }
    for(k in 1:length(split_list)){
      un_split_list <- unlist(split_list[k])[[1]]
      # function here goes forward to find where to put these based on vector and time interval
      fwd_index <- index_forward(t2, "vector", value = names(split_list)[k], i)

      already_there <- t2$holding[fwd_index] # if there are already groups there, try to make sure they get ordered ascending to match vector column
      if(is.na(already_there)){
        t2$members[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$members[fwd_index], un_split_list)) %>%
          gsub(" ", "/", .)
        t2$holding[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$holding[fwd_index], names(split_list)[k])) %>%
          gsub(" ", "-", .)
      }else{
        t2$members[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$members[fwd_index], un_split_list)) %>%
          gsub(" ", "/", .)
        t2$holding[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$holding[fwd_index], names(split_list)[k])) %>%
          gsub(" ", "-", .)

        new_order <- stringr::str_split(t2$holding[fwd_index], "-")[[1]] %>% as.numeric() %>% base::order()
        t2$holding[fwd_index] <- paste(stringr::str_split(t2$holding[fwd_index], "-")[[1]][new_order], collapse = "-")
        t2$members[fwd_index] <- paste(stringr::str_split(t2$members[fwd_index], "/")[[1]][new_order], collapse = "/")
      }
    }
  }
  return(t2)
}
