#' Extract data from list on individual basis and make keyed data.table
#' @param x the group number
#' @param n the number of individuals in the group to generate
#' @keywords internal
initiate_group <- function(x, n){
  paste(unlist(paste0(x, "_", 1:n)), collapse = "-")
}
