#' simulate non-independent group-switching
#' @inheritParams simulate_graph
#' @param cohesion the probability that individuals stay with their group from
#' the previous time step during fission.

simulate_non_independence <- function(
  n_groups = 4,
  time_to_leave = 5,
  time_to_return = 2,
  cohesion = 0.8,
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
    dplyr::slice(1:n()-1)

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
    data.frame() %>%
    dplyr::mutate(action = NA,
           holding = NA)

  for(i in 1:nrow(t2)){
    value <- paste(stringr::str_split(t2$vector[i], "-")[[1]], collapse = "|")
    t2$action[i] <- delta_grp(t2, "vector", value, i)
  }

  cohesion = cohesion # how much should they stick to their previous group?

  for(i in 1:nrow(t2)){
    if(!t2$members[i] %in% NA & t2$action[i] %in% c(NA)){
      next
    }else if(t2$action[i] == "same"){
      # take care of current line
      t2$members[i] <- t2$members[index_back(t2, "vector", t2$vector[i], i)]
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      n <- length(curr_vec)
      mbrs_list <- list()
      for(j in 1:n){
        mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
        names(mbrs_list)[j] <- curr_vec[[j]]
      }
      t2 <- ff_forward(t2, curr_vec, n, mbrs_list, i)
    }else if(t2$action[i] == "fusion"){
      # take care of current, but these will also be used looking forward
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      n <- length(curr_vec)
      mbrs_list <- list()
      for(j in 1:n){
        mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
        names(mbrs_list)[j] <- curr_vec[[j]]
      }
      t2$members[i] <- paste(unlist(mbrs_list), collapse = "/") # assign current
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      t2 <- ff_forward(t2, curr_vec, n, mbrs_list, i)
    } else if(t2$action[i] == "fission"){
      # members should always be accounted for to start
      if(t2$vector[i] != t2$holding[i]){stop(paste("All groups not accounted for at start of line", i, ", fission group."))}
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      n <- length(curr_vec)
      mbrs_list <- list()
      for(j in 1:n){
        mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
        names(mbrs_list)[j] <- curr_vec[[j]]
      }
      t2 <- ff_forward(t2, curr_vec, n, mbrs_list, i)
    }else if(t2$action[i] == "fission-fusion"){
      # members should always be accounted for to start
      if(t2$vector[i] != t2$holding[i]){stop(paste("All groups not accounted for at start of line", i, ", fission-fusion group"))}
      # look forward
      if(t2$start[i] == max(t2$start)){next}
      curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]
      n <- length(curr_vec)
      mbrs_list <- list()
      for(j in 1:n){
        mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
        names(mbrs_list)[j] <- curr_vec[[j]]
      }
      t2 <- ff_forward(t2, curr_vec, n, mbrs_list, i)
    }
  }
  return(t2)
}




