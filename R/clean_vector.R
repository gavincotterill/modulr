#' put vector string in numeric order
#' @param cell a t2$vector cell
#' @keywords internal
clean_vector <- function(cell){
  x <- stringr::str_split(cell, "-")[[1]]
  y <- stringr::str_split(cell, "-")[[1]][order(characterRank(x))] %>%
    paste(collapse = "-")
  return(y)
}
