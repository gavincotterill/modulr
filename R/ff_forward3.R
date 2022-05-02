#' @title ff_fwd3
#' @description like ff2, but trying to incorporate a multinomial draw for animals 'not-at-home'
#'
#' @param t2 the dataframe of group and individual switching schedules over matched intervals
#' @param curr_vec the group tags present in vector at i
#' @param mbrs_list the list of groups and animals within groups at i
#' @param i the loop index for t2
#' @inheritParams simulate_animal
#'
#' @keywords internal
#'
ff_forward3 <- function(t2, curr_vec, mbrs_list, i, time_to_leave, time_to_return){

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

  if(any(c("fission", "fission-fusion") %in% next_actions) & length(curr_vec) > 1){

    (t <- stringr::str_split(mbrs_list, "-") %>% `names<-`(names(mbrs_list)))
    (grps <- names(mbrs_list))
    (id_tags <- paste0(grps, "_"))

    # diff <- t2 %>% dplyr::group_by(start) %>% dplyr::summarise(diff = end - start) %>% dplyr::slice(1)
    # avg_int <- mean(diff$diff)
#
#     lh_prob <- ifelse((1/time_to_leave) * avg_int >= 1, 0.99, (1/time_to_leave) * avg_int) # prob of leaving home
#     gh_prob <- ifelse((1/time_to_return) * avg_int >= 1, 0.99, (1/time_to_return) * avg_int) #### THIS IS AN ISSUE
#
#     lh_prob <- ifelse(lh_prob <= 0, 0.01, lh_prob)
#     gh_prob <- ifelse(gh_prob <= 0, 0.01, gh_prob)

    (lh_prob <- (1/time_to_leave) / ((1/time_to_leave) + (1/time_to_return)))
    (gh_prob <- (1/time_to_return) /  ((1/time_to_leave) + (1/time_to_return)))

    lh_prob <- ifelse(lh_prob >= 1, 0.99, lh_prob)
    gh_prob <- ifelse(gh_prob >= 1, 0.99, gh_prob)

    lh_prob <- ifelse(lh_prob <= 0, 0.01, lh_prob)
    gh_prob <- ifelse(gh_prob <= 0, 0.01, gh_prob)

    # who is with their home group?
    (at_home <- purrr::map2(t, id_tags, ~ stringr::str_subset(., .y) ))
    (at_home <- at_home[lengths(at_home) > 0])
    if(length(at_home) >= 1){
      (f <- purrr::map(lengths(at_home), function(x) rbinom(x, 1, lh_prob) %>% sum(.)))
      (leaves_home <- purrr::map2(at_home, f, ~ sample(., size=.y)))
      (leaves_home <- leaves_home[lengths(leaves_home) > 0])
    }

    # who is not with their home group?
    (not_at_home <- purrr::map2(t, id_tags, ~ stringr::str_subset(., .y, negate = TRUE) ))
    (not_at_home <- not_at_home[lengths(not_at_home) > 0])
    # n is always one because we're dealing with one group at a time
    # size is the number of individuals in the group
    # prob is a vector of probabilities for:
    # (1) goes home, (2) stays, (3) switches to another co-located group
    # the probability of going home is whatever we said it was
    # the probability of staying or switching is (1-prob of going home)/2
    if(length(not_at_home) >= 1){
      (f <- purrr::map(lengths(not_at_home), function(x) rmultinom(n = 1, size = x, prob = c( gh_prob, (1-gh_prob)/2, (1-gh_prob)/2 ))))
      (go_home_samp <- purrr::flatten(purrr::map(f, 1)))
      (switch_samp <- purrr::flatten(purrr::map(f, 3)))
      # sample from not_at_home once
      (does_go_home <- purrr::map2(not_at_home, go_home_samp, ~ sample(., size=.y)))
      does_go_home <- does_go_home[lengths(does_go_home) > 0]
      # then not_at_home needs to be updated
      if(length(does_go_home) > 0){
        (not_at_home <- purrr::map2(not_at_home, paste(unlist(does_go_home), collapse = "|"), ~ stringr::str_subset(., .y, negate = T) ))
      }
      (does_switch <- purrr::map2(not_at_home, switch_samp, ~ sample(., size=.y)))
      does_switch <- does_switch[lengths(does_switch) > 0]
    }

    # using custom functions to move animals around
    mbrs_list2 <- stringr::str_split(mbrs_list, "-") %>% `names<-`(names(mbrs_list)) # structural bits here that could mess things up down the line
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
    if(exists("does_switch")){
      if(length(does_switch) > 0){
        mbrs_list2 <- switch_animals(mbrs_list2, does_switch)
      }
    }
    # mbrs_list2 <- mbrs_list2[lengths(mbrs_list2) > 0]
    # (purrr::map2(mbrs_list2, "", ~ stringr::str_subset(., .y) ) %>% lengths()) # I don't know why this works for removing empty "", but it does
    # lengths(mbrs_list2)
    (mbrs_list2 <- purrr::map2(mbrs_list2, "", ~ stringr::str_subset(., .y) ))
    mbrs_list2 <- purrr::map(mbrs_list2, ~ paste(., collapse = "-"))
    mbrs_list <- mbrs_list2
    curr_vec <- names(mbrs_list2)
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
  }

  if(any(c("fission", "fission-fusion") %in% next_actions)){
    for(k in 1:length(mbrs_list)){
      un_mbrs_list <- unlist(mbrs_list[k])[[1]]
      fwd_index <- index_forward(t2, "vector", value = names(mbrs_list)[k], i)
      # if(!isTRUE(fwd_index)){next}
      already_there <- t2$holding[fwd_index]
      if(is.na(already_there)){
        t2$members[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$members[fwd_index], un_mbrs_list)) %>% gsub(" ", "/", .)
        t2$holding[fwd_index] <- gsub("NA| NA|NA ", "", paste(t2$holding[fwd_index], names(mbrs_list)[k])) %>% gsub(" ", "-", .)
      }else{
        t2$members[fwd_index] <- paste(t2$members[fwd_index], un_mbrs_list) %>% gsub(" ", "/", .)
        t2$holding[fwd_index] <- paste(t2$holding[fwd_index], names(mbrs_list)[k]) %>% gsub(" ", "-", .)
        new_order <- stringr::str_split(t2$holding[fwd_index], "-")[[1]] %>% as.numeric() %>% base::order() # get the order, don't sort the values
        t2$holding[fwd_index] <- paste(stringr::str_split(t2$holding[fwd_index], "-")[[1]][new_order], collapse = "-")
        t2$members[fwd_index] <- paste(stringr::str_split(t2$members[fwd_index], "/")[[1]][new_order], collapse = "/")
        x <- stringr::str_split(t2$holding[fwd_index], "-")[[1]]
        if( any(duplicated(x)) ){
          out_n <- length(unique(x))
          out_list <- list()
          for(p in 1:out_n){
            grab_index <- which(x == unique(x)[p])
            mems <- stringr::str_split(t2$members[fwd_index], "/")[[1]]
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
