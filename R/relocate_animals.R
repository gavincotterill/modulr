#' relocate animals going home to a new mbrs_list slot
#' @param inlist a list like mbrs_list
#' @param elem_relocate a list with same length  as mbrs_list of animals to send
#' to home groups at next time slot
#' @keywords internal
relocate_animals <- function(inlist, elem_relocate){
  elem_relocate = unlist(elem_relocate)[[1]]
  removed = lapply(inlist, setdiff, elem_relocate)
  for(i in seq_along(elem_relocate)){
    home_group <- str_extract(elem_relocate, "\\d{1,}(?=_)")
    removed[[home_group]] <- c(removed[[home_group]], elem_relocate[i])
  }
  removed
}
