#' Internal to simulate_non_independence
#'
#' Relocate animals going home to a new mbrs_list slot
#'
#' @param inlist a list like mbrs_list
#' @param elem_relocate a list with same length as mbrs_list of animals to send
#' to home groups at next time slot
#' @keywords internal
relocate_animals <- function(inlist, elem_relocate){
  # inlist = mbrs_list2
  # elem_relocate = does_go_home
  elem_relocate = paste(unlist(elem_relocate), sep = " ")
  removed = lapply(inlist, setdiff, elem_relocate)
  for(i in seq_along(elem_relocate)){
    home_group <- stringr::str_extract(elem_relocate[i], "\\d{1,}(?=_)")
    removed[[home_group]] <- c(removed[[home_group]], elem_relocate[i])
  }
  removed
}
