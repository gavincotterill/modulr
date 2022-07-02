#' simulate non-independent group-switching experimental
#'
#' @keywords internal
#' @inheritParams simulate_groups
#' @param n_splits the number of subgroups that groups will splinter into when they leave home. If NA, the number of subgroups is dependent on the number of animals in a group.

simulate_non_independence2 <- function(
  n_groups = 4,
  n_animals = 16,
  n_splits = NA,
  time_to_leave = 5,
  time_to_return = 2,
  travel_time = c(0.01,2),
  sampling_duration = 7
){
  grp_lengths_vector <- rand_vect(n_groups, n_animals, sd = 1)

  # unfortunately this has groups always split into the same number of subgroups, but maybe still better than fixing all groups to same number of splits
  # it is at least a first stab at size-dependent splitting, maxing out at 6 subgroups or the number of non-resident groups, whichever is smaller
  n_split_list <- list()
  if(!is.na(n_splits)){
    n_split_list <- list(n_splits)
  }else if(is.na(n_splits)){
    n_split_list <- lapply(grp_lengths_vector, function (x){
      if(x == 1){y = 1}
      if(x == 2){y = rbinom(1, 1, 0.5) + 1} # one or two with 50% prob
      if(x >= 3 & x <= 5){y = rbinom(1, 1, 0.5) + 2} # two or three
      if(x >= 6 & x <= 10){y = sample(2:4, 1, prob=c(1/3, 1/3, 1/3))} # two, three, or four
      if(x >= 11 & x <= 15){y = sample(3:5, 1, prob = c(1/3, 1/3, 1/3))} # 3:5
      if(x >= 16){y = sample(4:6, 1, prob = c(1/3, 1/3, 1/3))} # 4:6
      if(y > n_groups - 1){y = n_groups - 1}else{y = y}
      return(y)
    })
  }
  group_list <- list()
  for(m in 1:n_groups){
    group_list[[m]] <- simulate_groups2(animals_home = m,
                                        n_groups = n_groups,
                                        n_splits = ifelse(length(n_split_list) == 1, n_split_list[[1]], n_split_list[[m]]),
                                        time_to_leave = time_to_leave,
                                        time_to_return = time_to_return,
                                        travel_time = travel_time,
                                        sampling_duration = sampling_duration)
  }

  names(group_list) <- 1:length(group_list)

  groups_transformed <- purrr::map2(
    .x = group_list,
    .y = n_split_list,
    .f = ~ sglt_bind(.x, .y)
  )

  ints <- lapply(groups_transformed, `[[`, 1) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-"state", -"sub_id")

  complete_intervals <-
    data.frame(start = c(unique(ints$start), unique(ints$end))) %>%
    dplyr::arrange(start) %>%
    round(4) %>% # without rounding again, you can have such small differences in intervals that they appear to be x to x, rather than only x to y, y to z
    dplyr::distinct() %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(end = dplyr::lead(start))

  sub_group = groups_transformed[[1]][[1]]
  intervals = complete_intervals
  cust_fxn <- function(sub_group, intervals){
    t1 <- intervals %>%
      dplyr::left_join(sub_group, by = c("end")) %>%
      tidyr::fill(state, .direction = "up") %>%
      tidyr::fill(sub_id, .direction = "up") %>%
      dplyr::select(- start.y) %>%
      dplyr::rename(start = start.x) %>%
      na.omit()
    t1
  }

  nested_lapply <- function(data, fun, argument) {
    lapply(data, function(sublist) { lapply(sublist, fun, argument) })
  }
  list_of_lists <- nested_lapply(groups_transformed, cust_fxn, complete_intervals)

  comp_ints_list <- lapply(list_of_lists, dplyr::bind_rows)

  df <- dplyr::bind_rows(comp_ints_list, .id = "id") %>%
    dplyr::arrange(start, end, state, id, sub_id)

  #' now collapse instances where sub_ids are together -- usually at home, but not always
  #' then remove duplicates

  test <- df %>%
    dplyr::group_by(id, state, start, end) %>%
    dplyr::summarise(sub_vector=paste(sub_id, collapse="-")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(start, end, id, state)

  # this will be spatially explicit, so we'll judge 'time home' vs 'time away' in 'recovering' the switching rates
  # but we'll analyze network properties on the basis of time spent together as edge weights

  #' initiate groups and hold them in reserve
  p <- test %>% dplyr::filter(start == 0) %>% dplyr::select(-"sub_vector")
  p$members <- purrr::map2(p$id, grp_lengths_vector, ~initiate_group(.x, .y))

  mem_df <- dplyr::left_join(test, p, by = c("state", "start", "end", "id")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "NULL"))

  #' need to create vector column to see which groups are where
  t2 <- test %>%
    dplyr::group_by(state, start, end) %>%
    dplyr::summarise(vector=paste(id, collapse="-")) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(start, end, state) %>%
    dplyr::mutate(idx = match(start, unique(start))) %>%
    data.frame() %>%
    dplyr::mutate(holding = NA) %>%
    dplyr::left_join(mem_df, by = c("state", "start", "end")) %>% # adding by to suppress message
    dplyr::select(state, start, end, idx, vector, members, holding) %>%
    dplyr::distinct() %>%
    dplyr::filter(start <= sampling_duration)

  t2$vector <- purrr::map(t2$vector, ~clean_vector(.x))

  for(i in 1:nrow(t2)){
    # if(i == 1341){break}

    if(!is.na(t2[i,"holding"])){
      t2[i,"holding"] <- clean_holding(t2[i, "holding"])
    }

    #' split vector[i] into a list of groups present
    curr_vec <- stringr::str_split(t2$vector[i], "-")[[1]]

    # for empty travel states:
    # if(i > (n_groups + 1) & t2$holding[i] %in% c(NA, "")){ # I'm worried about the empty character string "" being an issue
    if(t2[i, "idx"] > 1 & t2$holding[i] %in% c(NA, "")){ # I'm worried about the empty character string "" being an issue
      # t2$holding[i] <- paste0(stringr::str_split(t2$vector[i], "-")[[1]], "_0") %>% unlist() %>% sort() %>% paste(collapse = "/") # this won't order them correctly
      t2$holding[i] <- paste0(stringr::str_split(t2$vector[i], "-")[[1]], "_0") %>% paste(collapse = "/") # since vector is ordered correctly, this should work

    }

    if(t2[i, "idx"] > 1 & is.na(t2$members[i])){
      # switch <- stringr::str_detect(t2$holding[i], "/")
      holding_groups <- stringr::str_split(t2$holding[i], "/")[[1]] %>% extract_group()
      # holding_groups
      n_holding_groups <- length(holding_groups)

      if(n_holding_groups == length(curr_vec)){
        t2$members[i] <- t2$holding[i]
      }else if(n_holding_groups < length(curr_vec)){
        present <- extract_group(holding_groups)
        # present
        missing <- curr_vec[which(!curr_vec %in% holding_groups)]
        # missing
        add_ons <- paste0(missing, "_0")
        # add_ons
        t2$holding[i] <- paste(t2$holding[i], add_ons, sep = "/")
        t2$members[i] <- clean_holding(t2$holding[i])
      }else if(n_holding_groups > length(curr_vec)){
        stop(paste0("somehow we ended up with too many groups in holding at row ", i))
      }

      # if(switch %in% c(FALSE, NA)){
      #   t2$members[i] <- t2$holding[i]
      # }else{
      #   # resolve groups
      #   intermediate <- stringr::str_split(t2$holding[i], "/")[[1]] #%>% sort()
      #   if(length(curr_vec) == 1){
      #     t2$members[i] <- intermediate %>% paste(collapse = "-") %>% stringr::str_split(.,"-") %>% unlist() %>% sort() %>% paste(collapse = "-")
      #   }else{
      #     int2 <- intermediate %>% paste(collapse = "-") %>% stringr::str_split(.,"-") %>% unlist() %>% sort()
      #     int3 <- lapply( as.list(paste0(curr_vec, "_")), grep, as.list(int2), value = TRUE) %>% # changed sapply to lapply
      #       lapply(., paste, collapse = "-")
      #     # if there's an 'empty split', make a dummy group_0 animal and delete the dummy animal when they go home, delete all dummy animals at the very end before returning t2
      #     t2$members[i] <- int3 %>%
      #       purrr::map2(., curr_vec, ~ifelse(.x == "", paste0(.y, "_0"), .x)) %>%
      #       unlist() %>%
      #       paste(collapse = "/")
      #   }
      # }
    }

    # I shouldn't need this gusb anymore:
    # mbrs_list <- as.list(stringr::str_split(t2$members[i], "/")[[1]] %>%  gsub(paste0(t2$state[i],"_0-"),"", .) %>% sort())
    mbrs_list <- as.list(stringr::str_split(t2$members[i], "/")[[1]]) %>% #%>%  gsub(paste0(t2$state[i],"_0-"),"", .) %>% sort())
      `names<-`(curr_vec)
    # I shouldn't need this chunk either:
    # # another fix to accomodate issue where subgroups randomly end up together rather than split after travel to away state:
    # if(length(mbrs_list) < length(curr_vec)){
    #
    #   int6 <- purrr::map2(mbrs_list, curr_vec, ~stringr::str_detect(.x, paste0(.y, "_"), negate = TRUE))
    #
    #   missing_group <- curr_vec[which(int6 == TRUE)] %>% paste0("_0")
    #   new_members <- paste(t2$members[i], missing_group, sep = "/")
    #   mbrs_list <- stringr::str_split(new_members, "/")[[1]] %>%
    #     gsub(paste0(t2$state[i],"_0-"),"", .) %>%
    #     sort() %>%
    #     as.list() %>%
    #     `names<-`(curr_vec)
    #
    # }else if(length(mbrs_list) == length(curr_vec)){
    #   names(mbrs_list) <- curr_vec
    # }
    # # overwriting here which should be ok, just inefficient
    # t2$members[i] <- mbrs_list %>% paste(collapse = "/")

    #' make a mbrs_list that has length n vectors and populate it with members from holding
    curr_time <- t2$idx[i]
    next_time <- curr_time + 1
    #' look ahead to idx[i]+1, the next time interval
    #' number_of_current_locations <- the number of rows in which vector occurs at present time_interval
    #' number_of_destinations <- the number of rows in which vector/mbrs_list have to go next
    curr_temp <- t2 %>% dplyr::filter(idx == curr_time)
    curr_vl <- curr_temp %>% dplyr::select(vector) %>% as.list(stringr::str_split(., "-")[[1]])
    next_temp <- t2 %>% dplyr::filter(idx == next_time)
    next_vl <- next_temp %>% dplyr::select(vector) %>% as.list(stringr::str_split(., "-")[[1]])
    n_curr_locs_list <- lapply(purrr::map2(names(mbrs_list), curr_vl, function(x, y) grep(paste0("\\b",x,"\\b"), y)), length)
    n_next_locs_list <- lapply(purrr::map2(names(mbrs_list), next_vl, function(x, y) grep(paste0("\\b",x,"\\b"), y)), length)

    for(j in 1:length(mbrs_list)){
      # condition 1
      #' if number of current locations is > 1 and number of destinations == 1, send all mbrs_list to the single destination's holding at next time interval
      if(n_curr_locs_list[j] >= 1 & n_next_locs_list[j] == 1){
        destination <- index_forward(t2, "vector", names(mbrs_list)[j], i)
        already_there <- t2$holding[destination]
        if(is.na(already_there)){
          t2$holding[destination] <- gsub("NA| NA|NA ", "", paste(t2$holding[destination], mbrs_list[[j]])) %>% gsub(" ", "-", .)
        }else{
          t2$holding[destination] <- paste(t2$holding[destination], mbrs_list[[j]], sep = "/")
        }
        #' condition 2
        #' if number of current locations == 1 and number of destinations > 1, do multinomial draw to determine how many to send to each destination
        #' sample mbrs_list and send animals to destinations holding at next time interval
      }else if(n_curr_locs_list[j] == 1 & n_next_locs_list[j] > 1){
        indivs <- stringr::str_split(mbrs_list[[j]], "-")[[1]]
        n_indivs <- length(indivs)
        probs <- rep(1/n_next_locs_list[[j]], n_next_locs_list[[j]])
        f <- as.list(rmultinom(n = 1, size = n_indivs, prob = probs )) # this can result in 0 draws, meaning no indivs are sent to a 'split', that's ok by itself but the code can't handle it
        out_list <- list()
        for(k in 1:length(probs)){
          out_list[[k]] <- sample(indivs, f[[k]], replace = FALSE)
          indivs <- indivs[which(!indivs %in% out_list[[k]])]
        }
        destinations <- index_forward(t2, "vector", names(mbrs_list)[j], i) %>% as.list()
        for(k in 1:length(out_list)){
          already_there <- t2$holding[destinations[[k]]]
          if(is.na(already_there)){
            t2$holding[destinations[[k]]] <- gsub("NA| NA|NA ", "", paste(t2$holding[destinations[[k]]], out_list[[k]])) %>% paste(., collapse = "-")
          }else{
            out_indivs <- paste(out_list[[k]], collapse = "-")
            t2$holding[destinations[[k]]] <- paste(t2$holding[destinations[[k]]], out_indivs, sep = "/") %>% gsub(" ", "-", .)
          }
        }
        #' condition 3
        #' else if number of current locations > 1 and == number of destinations
      }else if(n_curr_locs_list[j] > 1 & identical(n_curr_locs_list[j], n_next_locs_list[j])){

        curr_states <- curr_temp[grep(paste0("\\b",names(mbrs_list)[j],"\\b"), curr_temp$vector), "state"]
        n_curr_states <- length(curr_states)
        next_states <- next_temp[grep(paste0("\\b",names(mbrs_list)[j],"\\b"), next_temp$vector), "state"]
        n_next_states <- length(next_states)

        # get a relative index from curr_temp by matching curr_temp and t2
        x <- curr_temp[grep(paste0("\\b",names(mbrs_list)[j],"\\b"), curr_temp$vector),]
        y <- t2[i,]

        rel_ind <- apply(x, 1, match, y) %>% colSums(na.rm = T) %>% which.max()
        destinations_list <- index_forward(t2, "vector", names(mbrs_list)[j], i) %>% as.list()

        already_there <- t2$holding[destinations_list[[rel_ind]]]

        if(is.na(already_there)){
          t2$holding[destinations_list[[rel_ind]]] <- gsub("NA| NA|NA ", "", paste(t2$holding[destinations_list[[rel_ind]]], mbrs_list[[j]])) %>% paste(., collapse = "-")
        }else{
          t2$holding[destinations_list[[rel_ind]]] <- paste(t2$holding[destinations_list[[rel_ind]]], mbrs_list[[j]], sep = "/") %>% gsub(" ", "-", .)
        }
        #' if n_curr_locs > 1 and n_next_locs != 1 and n_next_locs != n_curr_locs, subgroups split appropriately but due to sampling some sub groups ended at the same
        #' location after travelling separately, this handles the extra travelling state on the way home as well
      }else if(n_curr_locs_list[[j]] > 1 & !n_next_locs_list[[j]] %in% c(0,1) & n_next_locs_list[[j]] != n_curr_locs_list[[j]]){

        random_choice <- sample(x = n_next_locs_list[[j]], size = 1)
        destinations_list <- index_forward(t2, "vector", names(mbrs_list)[j], i) %>% as.list()

        already_there <- t2$holding[destinations_list[[random_choice]]]
        if(is.na(already_there)){
          t2$holding[destinations_list[[random_choice]]] <- gsub("NA| NA|NA ", "", paste(t2$holding[destinations_list[[random_choice]]], mbrs_list[[j]])) %>% paste(., collapse = "-")
        }else{
          t2$holding[destinations_list[[random_choice]]] <- paste(t2$holding[destinations_list[[random_choice]]], mbrs_list[[j]], sep = "/") %>% gsub(" ", "-", .)
        }
        #' need a condition handler for the last idx
      }else if(t2$idx[i] == max(t2$idx)){
        next
      }else{
        stop(paste0("At row ", i, ", no handling condition exists."))
      }
    } # end j loop
  } # end i
  # # not a perfect scrub of dummies:
  # t2$members <- stringr::str_replace(string = t2$members, pattern = "\\d{1,}_0/|/\\d{1,}_0|\\d{1,}_0-|\\d{1,}_0|\\d{1,}_0-\\d{1,}_0-", replacement = "") # same as str_remove

  # try this:
  t2$members <- stringr::str_remove(string = t2$members, pattern = "\\d{1,}_0")
  t2$members <- stringr::str_remove(string = t2$members, pattern = "(?<!\\d)/")
  t2$members <- stringr::str_remove(string = t2$members, pattern = "/(?!\\d)")
  t2$members <- stringr::str_remove(string = t2$members, pattern = "(?<!\\d)-")
  t2$members <- stringr::str_remove(string = t2$members, pattern = "-(?!\\d)")

  return(t2)
}




