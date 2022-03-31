#' Index forward
#' @inheritParams index_back
#' @keywords internal
index_forward <- function(df, column_name, value, i){
  value_rows <- stringr::str_which(df[, column_name], value)
  value_rows[min(which(value_rows > i))]
}
