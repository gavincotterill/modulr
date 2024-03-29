#' Internal to simulate_non_independence
#'
#' Pulls the row number of the last occurrence of a group from any combination of vector values
#'
#' @param df the t2 dataframe
#' @param column_name the column to reference, usually 'vector'
#' @param value usually the group number
#' @param i the row index
#' @keywords internal

index_back <- function(df, column_name, value, i){
  q <- stringr::str_split(df[, column_name], "-")
  q2 <- purrr::map2(q, value, ~grep(paste0("\\b",.y,"\\b"), .x) %>% any())
  q3 <- which(q2 %in% TRUE)
  max(q3[q3 < i])

}
