#' Pulls the row number of the last occurrence of a group from any combination of vector values
#' @param df the t2 dataframe
#' @param column_name the column to reference, usually 'vector'
#' @param value usually the group number
#' @param i the row index
#' @keywords internal

index_back <- function(df, column_name, value, i){
  # mx_rows <- which(df[, column_name] == value)
  # mx_rows[max(which(mx_rows < i))]
  q <- str_split(df[, column_name], "-")
  q2 <- map2(q, value, ~str_detect(.x, .y) %>% any())
  q3 <- which(q2 %in% TRUE)
  max(q3[q3 < i])

}
