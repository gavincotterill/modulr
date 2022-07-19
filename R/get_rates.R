#' Calculate Averaged lambda and xi Rates for Individuals in a Network
#'
#' Wrapper to apply single_id_rate to all individuals in a network and calculate
#' the mean number of days individuals took to leave their respective home states
#' and the mean number of days individuals took to return to their respective
#' home states.
#'
#' @param sched a schedule object for a network from simulate_schedule()
#' @param sim the simulator used to generate the schedule
#' @export
#' @examples
#' \donttest{
#' obj <- simulate_schedule(n_animals = 10, n_groups = 2, time_to_leave = 5,
#'                          time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30,
#'                          simulator = "independent")
#'
#' get_rates(sched = obj, sim = "independent")
#' }
get_rates <- function(sched, sim){

  sub_sched <- sched
  sub_ids <- names(sched)

  out <- purrr::map2(sub_sched, sub_ids, ~single_id_rate(.x, .y, sim))
  ttlg <- purrr::map(out, function(x) x[1]["ttlg"]) %>% purrr::flatten() %>% unlist()
  ttrg <- purrr::map(out, function(x) x[2]["ttrg"]) %>% purrr::flatten() %>% unlist()
  df <- data.frame(mean_ttlg = ttlg, mean_ttrg = ttrg)
  return(df)

}
