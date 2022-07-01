#' wrapper on stringr::str_extract with regex for group tag
#' @param x a string
extract_group <- function(x){
  stringr::str_extract(x, "^\\d{1,}(?=_)")
}
