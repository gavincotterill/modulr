#' Deletes animals from a nested list
#' @param inlist a list like mbrs_list
#' @param elem_remove a list with same length  as mbrs_list of animals to delete
#' @keywords internal
remove_animals <- function(inlist, elem_remove){
  outlist = lapply(inlist, setdiff, elem_remove)
  # outlist[lengths(outlist) > 0]
  outlist
}
