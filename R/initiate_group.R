#' Internal to simulate_non_independence and simulate_non_independence2
#'
#' Creates string vector for all individuals in a group
#'
#' @param x the group number
#' @param n the number of individuals in the group to generate
#' @keywords internal
initiate_group <- function(x, n){
  paste(unlist(paste0(x, "_", 1:n)), collapse = "-")
}
