#' get average time to leave home and time to return home rates for every
#' individual in a network
#' @param sched a schedule object for a network from simulate_schedule()
#' @param sim the simulator used to generate the schedule
#' @keywords internal
get_rates <- function(sched, sim){

  sub_sched <- sched
  sub_ids <- names(sched)

  out <- purrr::map2(sub_sched, sub_ids, ~single_id_rate(.x, .y, sim))
  ttlg <- purrr::map(out, function(x) x[1]["ttlg"]) %>% purrr::flatten() %>% unlist()
  ttrg <- purrr::map(out, function(x) x[2]["ttrg"]) %>% purrr::flatten() %>% unlist()
  df <- data.frame(mean_ttlg = ttlg, mean_ttrg = ttrg)
  return(df)

}
