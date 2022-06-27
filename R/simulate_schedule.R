#' A group simulator function
#' @inheritParams simulate_groups
#' @export
simulate_schedule <- function(n_animals,
                              n_groups,
                              n_splits = NA,
                              time_to_leave,
                              time_to_return,
                              travel_time,
                              sampling_duration,
                              simulator = "independent"){
  if (!requireNamespace(c("igraph"), quietly = TRUE)) {
    stop("Package \"igraph\"  must be installed to use this function.",call. = FALSE )
  }
  if (n_groups == 1) {
    stop("single module networks are currently not supported -- n_groups must be >= 2",call. = FALSE)
  }
  if (!simulator %in% c("independent", "group-think", "non-independent")) {
    stop( "sampler must be either \"independent\", \"non-independent\" or \"group-think\".",call. = FALSE)
  }

  if(simulator == "independent"){
    out <- animals_transformed(n_groups = n_groups,
                               n_animals = n_animals,
                               time_to_leave = time_to_leave,
                               time_to_return = time_to_return,
                               travel_time = travel_time,
                               sampling_duration = sampling_duration)
  }else if(simulator == "non-independent"){
    t2 <- simulate_non_independence2(n_groups = n_groups,
                                     n_animals = n_animals,
                                     n_splits = n_splits,
                                     time_to_leave = time_to_leave,
                                     time_to_return = time_to_return,
                                     travel_time = travel_time,
                                     sampling_duration = sampling_duration)
    # check that all ids accounted for at all times
    t3 <- t2 %>%
      dplyr::group_by(start) %>%
      dplyr::summarise(mems = paste(members, collapse = "-")) %>%
      dplyr::mutate(mems = stringr::str_replace_all(mems, "/", "-")) %>%
      dplyr::mutate(mems = stringr::str_split(mems, "-"))
    t4 <- lapply(t3$mems, unlist)
    t5 <- purrr::map(t4, sort)
    ids <- t5[[1]]
    # convert to list of keyed data.tables
    at_non_ind <- purrr::map(ids, ~t2[stringr::str_which(t2$members, .x),] %>%
                               dplyr::select(state, start, end, vector) %>%
                               na.omit() %>%
                               dplyr::mutate_at(2:3, ~as.numeric(.)) %>%
                               data.table::setDT() %>%
                               data.table::setkeyv(c("start", "end"))) %>%
      `names<-`(ids)
    return(at_non_ind)
  }
  else if(simulator == "group-think"){
    t2 <- simulate_non_independence(n_groups = n_groups,
                                    n_animals = n_animals,
                                    time_to_leave = time_to_leave,
                                    time_to_return = time_to_return,
                                    travel_time = travel_time,
                                    sampling_duration = sampling_duration)

    t3 <- t2 %>%
      dplyr::group_by(start) %>%
      dplyr::summarise(mems = paste(members, collapse = "-")) %>%
      dplyr::mutate(mems = stringr::str_replace_all(mems, "/", "-")) %>%
      dplyr::mutate(mems = stringr::str_split(mems, "-"))
    t4 <- lapply(t3$mems, unlist)
    t5 <- purrr::map(t4, sort)
    ids <- t5[[1]]

    # convert to list of keyed data.tables
    at_non_ind <- purrr::map(ids, ~t2[stringr::str_which(t2$members, .x),] %>%
                               dplyr::select(state, start, end, vector) %>%
                               na.omit() %>%
                               dplyr::mutate_at(2:3, ~as.numeric(.)) %>%
                               data.table::setDT() %>%
                               data.table::setkeyv(c("start", "end"))) %>%
      `names<-`(ids)
    return(at_non_ind)
  }
}
