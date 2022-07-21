#' Simulate a Network of Continuous-Time Movement Descriptions
#'
#' Generates a continuous-time-movement description for each individual in a simulated network according to the rules set forth by the chosen simulator. Returns a list of data.frames.
#'
#' @inheritParams simulate_groups
#' @param n_animals the number of animals in the network
#' @param n_splits if using the non-independent simulator. Integer value or NA. If an integer value, the number of sub-groups to split into when moving. If NA (default) larger groups will split into larger subgroups.
#' @param simulator the simulator to use. One of: "independent", "non-independent", or "group-think".
#' @export
#' @examples
#' \donttest{
#' obj <- simulate_schedule(n_animals = 15,
#'                          n_groups = 3,
#'                          time_to_leave = 5,
#'                          time_to_return = 2,
#'                          travel_time = c(0.001, 0.2),
#'                          sampling_duration = 20,
#'                          simulator = "independent")
#' }
simulate_schedule <- function(n_animals,
                              n_groups,
                              n_splits = NA,
                              time_to_leave,
                              time_to_return,
                              travel_time,
                              sampling_duration,
                              simulator = "independent"){
  if (!requireNamespace(c("igraph"), quietly = TRUE)) {stop("Package \"igraph\"  must be installed to use this function.",call. = FALSE )}
  if (n_groups == 1) {stop("single module networks are currently not supported -- n_groups must be >= 2",call. = FALSE)}
  if (!simulator %in% c("independent", "group-think", "non-independent")) {stop( "sampler must be either \"independent\", \"non-independent\" or \"group-think\".",call. = FALSE)}
  if(time_to_return - max(travel_time) < 0.75){warning("Travel times that are close to or exceed \'time_to_return\' may break the code.")}

  if(simulator == "independent"){
    out <- simulate_independence(n_groups = n_groups,
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
      dplyr::group_by(.data$start) %>%
      dplyr::summarise(mems = paste(.data$members, collapse = "-")) %>%
      dplyr::mutate(mems = stringr::str_replace_all(.data$mems, "/", "-")) %>%
      dplyr::mutate(mems = stringr::str_split(.data$mems, "-"))
    t4 <- lapply(t3$mems, unlist)
    t5 <- purrr::map(t4, sort)
    ids <- t5[[1]]

    at_non_ind <- purrr::map(ids, ~t2[stringr::str_which(t2$members, paste0("\\b",.x,"\\b")),] %>%
                               dplyr::select(.data$state, .data$start, .data$end, .data$vector) %>%
                               stats::na.omit() %>%
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
      dplyr::group_by(.data$start) %>%
      dplyr::summarise(mems = paste(.data$members, collapse = "-")) %>%
      dplyr::mutate(mems = stringr::str_replace_all(.data$mems, "/", "-")) %>%
      dplyr::mutate(mems = stringr::str_split(.data$mems, "-"))
    t4 <- lapply(t3$mems, unlist)
    t5 <- purrr::map(t4, sort)
    ids <- t5[[1]]

    at_non_ind <- purrr::map(ids, ~t2[stringr::str_which(t2$members, paste0("\\b",.x,"\\b")),] %>%
                               dplyr::select(.data$state, .data$start, .data$end, .data$vector, .data$members) %>%
                               stats::na.omit() %>%
                               dplyr::mutate_at(2:3, ~as.numeric(.)) %>%
                               data.table::setDT() %>%
                               data.table::setkeyv(c("start", "end"))) %>%
      `names<-`(ids)
    return(at_non_ind)
  }
}
