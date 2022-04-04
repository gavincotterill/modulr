#' rejoin animals to their home groups
#' @param inlist a list like mbrs_list
#' @param elem_rejoin a list with same length  as mbrs_list of animals to rejoin
#' their home group within current groups
#' @keywords internal
rejoin_animals <- function(inlist, elem_rejoin){
  # inlist = mbrs_list2
  # elem_rejoin = does_rejoin
  elem_rejoin = paste(unlist(elem_rejoin), sep = " ")
  removed = lapply(inlist, setdiff, elem_rejoin)
  for(i in seq_along(elem_rejoin)){
    # join_group <- str_extract(unlist(elem_rejoin)[i], "\\d{1,}(?=_)")
    join_group <- str_extract(elem_rejoin[i], "\\d{1,}(?=_)")
    removed[[join_group]] <- c(removed[[join_group]], elem_rejoin[i])
  }
  removed
}
