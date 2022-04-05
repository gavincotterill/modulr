#' Pulls the row number of the last occurrence of the identical group/vector configuration
#' @param df the t2 dataframe
#' @param column_name the column to reference, usually 'vector'
#' @param value usually the group number
#' @param i the row index
#' @keywords internal

index_back_same <- function(df, column_name, value, i){
  mx_rows <- which(df[, column_name] == value)
  mx_rows[max(which(mx_rows < i))]

}
