#' Internal to Continuous-Time Movement Simulators
#'
#' Takes individual time/movement descriptions and formats as a data.table
#' with discrete intervals
#'
#' @param animal an element of animal_list from simulate_graph
#' @keywords internal
dt_fxn <- function(animal){
  t1 <- animal$locations  %>%
    dplyr::mutate(end = dplyr::lead(.data$cumulative_time),
                  state = dplyr::lead(.data$current_state)) %>%
    dplyr::rename(start = .data$cumulative_time) %>%
    dplyr::select("state", "start","end") %>%
    stats::na.omit() %>%
    dplyr::mutate_all(~as.numeric(.)) %>%
    data.table::setDT()

  keycols <- c("start", "end")
  t2 <- data.table::setkeyv(t1, keycols)
  t2
}
