#' ff_fwd2 relates group-switching rates to average time interval lengths to let
#' individuals transition between home and non-home groups that they, in some cases
#' 'magically' find (i.e., they aren't sent to home base, they're sent to their home
#' group as if they knew where to find them with perfect accuracy.)
#' @keywords internal
ff_forward2 <- function(t2, curr_vec, mbrs_list, i, time_to_leave, time_to_return){

  # based on the number of groups currently here, how many destinations are there in the next time step?
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

  # if there is any fission in the next time step and more than one destination-- you could have a single group going to fission-fusion, in which case it's just a fusion for this group
  if(any(c("fission", "fission-fusion") %in% next_actions) & length(curr_vec) > 1){
    (t <- str_split(mbrs_list, "-") %>% `names<-`(names(mbrs_list)))
    (grps <- names(mbrs_list))
    (id_tags <- paste0(grps, "_"))

    # this wouldn't need to happen every time, could be stored outside
    # but it currently looks like these switching probabilities are too low
    # possibly because they only occur at fission events and aren't possible at every time step
    diff <- t2 %>% group_by(start) %>% summarise(diff = end - start) %>% slice(1)
    avg_int <- mean(diff$diff)
    (lh_prob <- (1/time_to_leave) * avg_int) # prob of leaving home
    (gh_prob <- (1/time_to_return) * avg_int) # avg int / ttr is the prob of being sent to home group

    # who is with their home group?
    (at_home <- purrr::map2(t, id_tags, ~ stringr::str_subset(., .y) ))
    (at_home <- at_home[lengths(at_home) > 0])
    (ah_lengths <- lapply(at_home, length))
    if(length(ah_lengths) > 1){
      (f <- map(ah_lengths, function(x) rbinom(x, 1, lh_prob) %>% sum(.)))
      (leaves_home <- map2(at_home, f, ~ sample(., size=.y)))
      (leaves_home <- leaves_home[lengths(leaves_home) > 0])
    }else if(length(ah_lengths) == 1){
       (f <- rbinom(ah_lengths[[1]], 1, lh_prob) %>% sum(.))
       (leaves_home <- map2(at_home, f, ~ sample(., size=.y)))
       leaves_home <- leaves_home[lengths(leaves_home) > 0]
    }

    # who is not with their home group?
    (not_at_home <- purrr::map2(t, id_tags, ~ stringr::str_subset(., .y, negate = TRUE) ))

    # who has the opportunity to rejoin their home group here?
    any_id_tags <- paste(unlist(id_tags), collapse = "|")

    (can_rejoin <- purrr::map(not_at_home, ~ stringr::str_subset(., any_id_tags) ))
    (can_rejoin <- can_rejoin[lengths(can_rejoin) > 0])
    (crj_lengths <- lapply(can_rejoin, length))
    if(length(crj_lengths) > 1){
      (f <- map(crj_lengths, function(x) rbinom(x, 1, gh_prob) %>% sum(.)))
      (does_rejoin <- map2(can_rejoin, f, ~ sample(., size=.y)))
      does_rejoin <- does_rejoin[lengths(does_rejoin) > 0]
    }else if(length(crj_lengths) == 1){
      (f <- rbinom(crj_lengths[[1]], 1, gh_prob) %>% sum(.)) # try this
      (does_rejoin <- map2(can_rejoin, f, ~ sample(., size=.y)))
      does_rejoin <- does_rejoin[lengths(does_rejoin) > 0]
    }

    # whoever isn't the three other categories could be sent to their home group
    (can_go_home <- purrr::map(not_at_home, ~ stringr::str_subset(., any_id_tags, negate = TRUE) ))
    (can_go_home <- can_go_home[lengths(can_go_home) > 0])
    (cgh_lengths <- lapply(can_go_home, length))
    if(length(cgh_lengths) > 1){
      (f <- map(cgh_lengths, function(x) rbinom(x, 1, gh_prob) %>% sum(.)))
      (does_go_home <- map2(can_go_home, f, ~ sample(., size=.y)))
      does_go_home <- does_go_home[lengths(does_go_home) > 0]
    }else if(length(cgh_lengths) == 1){
      (f <- rbinom(cgh_lengths[[1]], 1, gh_prob) %>% sum(.)) # try this
      (does_go_home <- map2(can_go_home, f, ~ sample(., size=.y)))
      does_go_home <- does_go_home[lengths(does_go_home) > 0]
    }

    # using custom functions to move animals around
    mbrs_list2 <- str_split(mbrs_list, "-") %>% `names<-`(names(mbrs_list)) # structural bits here that could mess things up down the line
    if(exists("does_rejoin")){
      if(length(does_rejoin) > 0){
        mbrs_list2 <- rejoin_animals(mbrs_list2, does_rejoin)
      }
    }
    if(exists("leaves_home")){
      if(length(leaves_home) > 0){
        mbrs_list2 <- switch_animals(mbrs_list2, leaves_home)
      }
    }
    if(exists("does_go_home")){
      if(length(does_go_home) > 0){
        mbrs_list2 <- relocate_animals(mbrs_list2, does_go_home)
      }
    }
    mbrs_list2 <- purrr::map(mbrs_list2, ~ paste(., collapse = "-"))
    curr_vec2 <- names(mbrs_list2)

    fwd_inds <- list()
    next_actions <- list()
    for(k in seq_along(curr_vec2)){
      fwd_index <- index_forward(t2, "vector", curr_vec2[[k]], i)
      fwd_inds[[k]] <- fwd_index
      next_actions[[k]] <- t2[fwd_index, "action"]
      names(next_actions)[k] <- curr_vec2[[k]]
    }
    next_locs <- unique(unlist(fwd_inds))
    n_next_locs <- length(next_locs)
  }

  if(any(c("fission", "fission-fusion") %in% next_actions) & n_next_locs == 1){
    already_there <- t2$holding[next_locs] # if there are already groups there, try to make sure they get ordered ascending to match vector column
    if(is.na(already_there)){
      t2$members[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$members[next_locs], unlist(mbrs_list)[[1]])) %>% gsub(" ", "/", .)
      t2$holding[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$holding[next_locs], names(mbrs_list))) %>% gsub(" ", "-", .)
    }else{
      t2$members[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$members[next_locs], unlist(mbrs_list)[[1]])) %>% gsub(" ", "/", .)
      t2$holding[next_locs] <- gsub("NA| NA|NA ", "", paste(t2$holding[next_locs], names(mbrs_list))) %>% gsub(" ", "-", .)

      new_order <- stringr::str_split(t2$holding[next_locs], "-")[[1]]%>% as.numeric() %>% base::order()
      t2$holding[next_locs] <- paste(stringr::str_split(t2$holding[next_locs], "-")[[1]][new_order], collapse = "-")
      t2$members[next_locs] <- paste(stringr::str_split(t2$members[next_locs], "/")[[1]][new_order], collapse = "/")

      # check for duplicate groups
      x <- str_split(t2$holding[next_locs], "-")[[1]]
      if( any(duplicated(x)) ){
        out_n <- length(unique(x))
        out_list <- list()
        for(p in 1:out_n){
          grab_index <- which(x == unique(x)[p])
          mems <- str_split(t2$members[next_locs], "/")[[1]]
          out_list[[p]] <- paste(mems[grab_index], collapse = "-")
        }
        t2$members[next_locs] <- paste(out_list, collapse = "/")
        t2$holding[next_locs] <- paste(unique(x), collapse = "-")
      }


    }
  }

  # need a condition handler if curr_vec2 is longer than curr_vec
  # and if the names(mbrs_list2)[k] is already at t2$holding[fwd_index], combine the groups
  if(any(c("fission", "fission-fusion") %in% next_actions) & n_next_locs > 1){
    for(k in 1:length(mbrs_list2)){
      un_mbrs_list2 <- unlist(mbrs_list2[k])[[1]]
      # function here goes forward to find where to put these based on vector and time interval
      fwd_index <- index_forward(t2, "vector", value = names(mbrs_list2)[k], i)

      already_there <- t2$holding[fwd_index] # if there are already groups there, try to make sure they get ordered ascending to match vector column
      if(is.na(already_there)){
        t2$members[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$members[fwd_index], un_mbrs_list2)) %>% gsub(" ", "/", .)
        t2$holding[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$holding[fwd_index], names(mbrs_list2)[k])) %>% gsub(" ", "-", .)
      }else{
        t2$members[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$members[fwd_index], un_mbrs_list2)) %>% gsub(" ", "/", .)
        t2$holding[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$holding[fwd_index], names(mbrs_list2)[k])) %>% gsub(" ", "-", .)

        new_order <- stringr::str_split(t2$holding[fwd_index], "-")[[1]] %>% as.numeric() %>% base::order()
        t2$holding[fwd_index] <- paste(stringr::str_split(t2$holding[fwd_index], "-")[[1]][new_order], collapse = "-")
        t2$members[fwd_index] <- paste(stringr::str_split(t2$members[fwd_index], "/")[[1]][new_order], collapse = "/")

        # add a regular expression for holding, if there's a duplicate, somehow drop one
        # add a regular expression for members...
        x <- str_split(t2$holding[fwd_index], "-")[[1]]
        if( any(duplicated(x)) ){
          out_n <- length(unique(x))
          out_list <- list()
          for(p in 1:out_n){
            grab_index <- which(x == unique(x)[p])
            mems <- str_split(t2$members[fwd_index], "/")[[1]]
            out_list[[p]] <- paste(mems[grab_index], collapse = "-")
          }
          t2$members[fwd_index] <- paste(out_list, collapse = "/")
          t2$holding[fwd_index] <- paste(unique(x), collapse = "-")
        }

      }
    }
  }

  return(t2)
}
