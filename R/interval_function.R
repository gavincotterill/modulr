#' Extract data from list on individual basis and make keyed data.table
#' @param groups a pairwise set of groups
#' @param intervals the intervals dataframe
#' @keywords internal

interval_function <- function(groups, intervals){
  intervals %>%
    dplyr::left_join(groups, by = "start") %>%
    data.frame() %>%
    tidyr::fill(state) %>%
    dplyr::select(- end.y) %>%
    dplyr::rename(end = end.x)%>%
    dplyr::mutate(trigger = ifelse(dplyr::lead(state) != state & state %% 1 == 0, 1 , 0))
}
