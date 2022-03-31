#' Extract data from list on individual basis and make keyed data.table
#' @param groups
#' @param interval

interval_function <- function(groups, intervals){
  intervals %>%
    dplyr::left_join(groups, by = "start") %>%
    data.frame() %>%
    tidyr::fill(state) %>%
    dplyr::select(- end.y) %>%
    dplyr::rename(end = end.x)
}
