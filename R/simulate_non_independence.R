#' simulate non-independent group-switching
#' @inheritParams simulate_graph
#' @examples
#' \donttest{
#' t2 <- simulate_non_independence(n_groups = 4,
#' time_to_leave = 5,
#' time_to_return = 2,
#' travel_time = c(0,2),
#' sampling_duration = 7,
#' samples_per_day = 1)
#' }

simulate_non_independence <- function(
  n_groups = 4,
  time_to_leave = 5,
  time_to_return = 2,
  travel_time = c(0,2),
  sampling_duration = 7,
  samples_per_day = 1
){
  group_list <- list()
  for(m in 1:n_groups){
    group_list[[m]] <- simulate_groups(animals_home = m,
                                       n_groups = n_groups,
                                       time_to_leave = time_to_leave,
                                       time_to_return = time_to_return,
                                       travel_time = travel_time,
                                       sampling_duration = sampling_duration,
                                       samples_per_day = samples_per_day)
  }

  names(group_list) <- 1:length(group_list)

  groups_transformed <- lapply(group_list, dt_fxn)

  # create all of the time intervals at once, then figure out who was where with whom afterwards
  grps_trans <- lapply(group_list, function (animal) {
    animal$locations  %>%
      dplyr::mutate(end = dplyr::lead(.data$cumulative_time),
                    state = dplyr::lead(.data$current_state)) %>%
      dplyr::rename(start = .data$cumulative_time) %>%
      dplyr::select("state", "start","end") %>%
      na.omit() %>%
      dplyr::mutate_all(~as.numeric(.))
  })

  ints <- dplyr::bind_rows(grps_trans) %>%
    dplyr::select(-"state")

  complete_intervals <- data.frame(start = c(ints$start, ints$end)) %>%
    dplyr::group_by(start) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(end = dplyr::lead(start)) %>%
    dplyr::slice(1:dplyr::n()-1)

  # now I have a list, one data.frame per id. intervals match across all dfs in list. location is tracked
  comp_ints_list <- lapply(groups_transformed, interval_function, complete_intervals)

  df <- dplyr::bind_rows(comp_ints_list, .id = "id")

  test <- df %>%
    dplyr::group_by(state, start, end) %>%
    dplyr::summarise(vector=paste(id, collapse="-")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(state, start, end)

  # initiate groups
  test$members <- mapply(function(start, vector) ifelse(start==0, initiate_group(vector, 5), NA), test$start, test$vector)

  t2 <- test %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(idx = match(start, unique(start))) %>%
    data.frame() %>%
    dplyr::mutate(action = NA,
                  holding = NA)

  for(i in 1:nrow(t2)){
    value <- paste(stringr::str_split(t2$vector[i], "-")[[1]], collapse = "|")
    t2$action[i] <- delta_grp(t2, "vector", value, i)
  }

  #--------------------------------------
  # set.seed(123)
  for(i in 1:nrow(t2)){
    # if(i == 239){break}
    if(t2$action[i] %in% c(NA)){
      next
    }else if(t2$action[i] == "same"){
      # take care of current line
      if(is.na(t2$members[i])){
        t2$members[i] <- t2$members[index_back_same(t2, "vector", t2$vector[i], i)]
      }else{
        (grp_tags <- str_extract_all(t2$members[i], "\\d{1,}(?=_)")[[1]] %>% unique())
        (incoming_grps <- str_split(t2$vector[i], "-")[[1]])
        (incoming_ids <- str_split(t2$members[index_back_same(t2, "vector", t2$vector[i], i)], "/")[[1]])
        (existing_mems <- str_split(t2$members[i], "/")[[1]])
        for(k in seq_along(incoming_grps)){
          if(incoming_grps[k] %in% grp_tags){
            grp_to_paste <- grp_tags[which(grp_tags == incoming_grps[k])]
            ids_to_paste <- existing_mems[str_detect(existing_mems, paste0(grp_to_paste,"_"))]
            incoming_ids[k] <- paste(incoming_ids[k], ids_to_paste, sep = "-")
          }
        }
        t2$members[i] <- paste(incoming_ids, collapse = "/")
      }
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      mbrs_list <- as.list(str_split(t2$members[i], "/")[[1]]) %>% `names<-`(curr_vec)
      t2 <- ff_forward2(t2, curr_vec, mbrs_list, i, time_to_leave, time_to_return)
    }else if(t2$action[i] == "fusion"){
      # take care of current, but these will also be used looking forward
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      n <- length(curr_vec)
      mbrs_list <- list()
      for(j in 1:n){
        all_mems_back <- str_split(t2$members[index_back(t2, "vector", curr_vec[j], i)] , "/")[[1]]
        vec_back <- str_split(t2$vector[index_back(t2, "vector", curr_vec[j], i)] , "-")[[1]]
        mem_idx <- which(vec_back == curr_vec[j])
        mbrs_list[[j]] <- all_mems_back[mem_idx]
        names(mbrs_list)[j] <- curr_vec[[j]]
      }
      if(is.na(t2$members[i])){
        t2$members[i] <- paste(unlist(mbrs_list), collapse = "/") # assign current
      }else{
        (grp_tags <- str_extract_all(t2$members[i], "\\d{1,}(?=_)")[[1]] %>% unique())
        (incoming_grps <- str_split(t2$vector[i], "-")[[1]])
        (incoming_ids <- paste(mbrs_list, sep = " "))
        (existing_mems <- str_split(t2$members[i], "/")[[1]])
        for(k in seq_along(incoming_grps)){
          if(incoming_grps[k] %in% grp_tags){
            grp_to_paste <- grp_tags[which(grp_tags == incoming_grps[k])]
            ids_to_paste <- existing_mems[str_detect(existing_mems, paste0(grp_to_paste,"_"))]
            incoming_ids[k] <- paste(incoming_ids[k], ids_to_paste, sep = "-")
          }
        }
        t2$members[i] <- paste(incoming_ids, collapse = "/")
      }
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      mbrs_list <- as.list(str_split(t2$members[i], "/")[[1]]) %>% `names<-`(curr_vec)
      t2 <- ff_forward2(t2, curr_vec, mbrs_list, i, time_to_leave, time_to_return)
    } else if(t2$action[i] %in% c("fission", "fission-fusion")){
      # members should always be accounted for to start
      if(t2$vector[i] != t2$holding[i]){stop(paste("All groups not accounted for at start of line", i, ", fission group."))}
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      mbrs_list <- as.list(str_split(t2$members[i], "/")[[1]]) %>% `names<-`(curr_vec)
      t2 <- ff_forward2(t2, curr_vec, mbrs_list, i, time_to_leave, time_to_return)
    }
  }

  return(t2)
}



