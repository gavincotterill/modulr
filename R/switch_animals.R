#' Internal to simulate_non_independence
#'
#' Sends animals leaving home to another group at current time step
#'
#' @param inlist a list like mbrs_list
#' @param elem_leave a list with same length  as mbrs_list of animals to send away
#' from their home group
#' @keywords internal
switch_animals <- function(inlist, elem_leave){

  elem_leave = paste(unlist(elem_leave), sep = " ")
  removed = lapply(inlist, setdiff, elem_leave)
  for(i in seq_along(elem_leave)){
    home_group <- stringr::str_extract(elem_leave[i], "\\d{1,}(?=_)")
    other_groups <- names(removed)[!names(removed) %in% home_group]
    send_to_group <- sample(other_groups, 1)
    removed[[send_to_group]] <- c(removed[[send_to_group]], elem_leave[i])
  }
  removed
}
