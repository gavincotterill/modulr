#' Extract data from list on individual basis and make keyed data.table
#' @param x
#' @param n
initiate_group <- function(x, n){
  paste(unlist(paste0(x, "_", 1:n)), collapse = "-")
}
