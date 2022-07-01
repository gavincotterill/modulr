#' Take a vector of characters that are numbers and return a vector of what their numeric order should be with tie-breaking
#'
#' @param x a vector of numeric characters
#'
#' @keywords internal
characterRank <- function(x) {
  y <- x[order(as.numeric(x))]
  z <- match(x, y)
  return(make.unique(as.character(z)) %>% as.numeric())
}
