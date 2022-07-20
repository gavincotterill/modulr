#' Get the lambda and xi rates for a single individual
#'
#' Takes an individual's continuous-time movement description and calculates
#' the average number of days until it leaves its home state and the average
#' number of days until it returns to its home state.
#'
#' @param sub_sched the individual's continuous time description
#' @param sub_id the individual's id string
#' @param sim the simulator used to generate the schedule
#' @export
#' @examples
#' \donttest{
#' obj <- simulate_schedule(n_animals = 10, n_groups = 2, time_to_leave = 5,
#'                          time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30,
#'                          simulator = "independent")
#'
#' single_id_rate(sub_sched = obj[[1]], sub_id = names(obj)[[1]], "independent")
#' }
single_id_rate <- function(sub_sched, sub_id, sim){
  home <- extract_group(sub_id) %>% as.numeric()

  if(sim == "independent"){
    sub_sched <- sub_sched %>%
      dplyr::filter(.data$state >= 1) %>%
      dplyr::mutate(start = ifelse(rownames(.) != 1, dplyr::lag(.data$end), .data$start),
                    home = ifelse(.data$state == home, TRUE, FALSE),
                    lambda = ifelse(home == TRUE, .data$end - .data$start, NA),
                    xi = ifelse(home == FALSE, .data$end - .data$start, NA))
  }else if(sim %in% c("non-independent", "group-think")){

    test <- sub_sched %>%
      # dplyr::select(-members) %>%
      dplyr::filter(.data$state >= 1)

    if(length(unique(test$state)) == 1){
      r <- 1
    }else{
      r <- rle(test$state)$l
    }

    sub_sched <- tryCatch({test %>%
      dplyr::mutate(cons = rep(seq(r), r)) %>%
      dplyr::group_by(.data$state, .data$cons) %>%
      dplyr::summarise(start = min(.data$start),
                end = max(.data$end)) %>%
      dplyr::arrange(.data$start) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(start = ifelse(rownames(.) != 1, dplyr::lag(.data$end), .data$start),
                    home = ifelse(.data$state == home, TRUE, FALSE),
                    lambda = ifelse(home == TRUE, .data$end - .data$start, NA),
                    xi = ifelse(home == FALSE, .data$end - .data$start, NA))},
      error = function(x){
        print(test)
        print(r)
      }
    )
  }


  ttlg <- sub_sched$lambda[-which(sub_sched$lambda %in% NA)]
  ttrg <- sub_sched$xi[-which(sub_sched$xi %in% NA)]

  out_list <- list(ttlg = mean(ttlg), ttrg = mean(ttrg))
  return(out_list)
}
