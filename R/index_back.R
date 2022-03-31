#' Title
#' @param df
#' @param column_name
#' @param value
#' @param i

index_back <- function(df, column_name, value, i){
  mx_rows <- which(df[, column_name] == value)
  mx_rows[max(which(mx_rows < i))]
}
