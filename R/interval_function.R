#' Extract data from list on individual basis and make keyed data.table
#' @param groups a pairwise set of groups
#' @param intervals the intervals dataframe

interval_function <- function(groups, intervals){
  intervals %>%
    dplyr::left_join(groups, by = "start") %>%
    data.frame() %>%
    tidyr::fill(state) %>%
    dplyr::select(- end.y) %>%
    dplyr::rename(end = end.x)
}
