#' Internal to simulate_non_independence
#'
#' Used to make matching time intervals across all individuals in a network
#'
#' @param groups a pairwise set of groups
#' @param intervals the intervals dataframe
#' @keywords internal

interval_function <- function(groups, intervals){
  intervals %>%
    dplyr::left_join(groups, by = "start") %>%
    data.frame() %>%
    tidyr::fill(state) %>%
    dplyr::select(- end.y) %>%
    dplyr::rename(end = end.x)
}
