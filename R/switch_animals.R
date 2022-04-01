#' Sends animals leaving home to another group at current time step
#' @param inlist a list like mbrs_list
#' @param elem_leave a list with same length  as mbrs_list of animals to send away
#' fromt heir home group
#' @keywords internal
switch_animals <- function(inlist, elem_leave){
  elem_leave = unlist(elem_leave)[[1]]
  removed = lapply(inlist, setdiff, elem_leave)
  for(i in seq_along(elem_leave)){

    home_group <- str_extract(elem_leave, "\\d{1,}(?=_)")
    other_groups <- names(removed)[!names(removed) %in% home_group]
    send_to_group <- sample(other_groups, 1)
    removed[[send_to_group]] <- c(removed[[send_to_group]], elem_leave[i])
  }
  removed
}
