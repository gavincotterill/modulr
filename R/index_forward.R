#' Internal to simulate_non_independence
#'
#' Returns row indices for where group can be at the next time interval
#'
#' @inheritParams index_back
#' @keywords internal
index_forward <- function(df, column_name, value, i){
  idx <- df$idx
  w <- df[idx == idx[i]+1,]
  value_rows <- grep(paste0("\\b",value,"\\b"), df[,column_name])
  out <- value_rows[value_rows %in% rownames(w)]
  return(out)
}
