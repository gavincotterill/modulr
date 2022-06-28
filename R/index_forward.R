#' Index forward
#' @inheritParams index_back
#' @keywords internal
index_forward <- function(df, column_name, value, i){
  idx <- df$idx
  w <- df[idx == idx[i]+1,]
  value_rows <- grep(paste0("\\b",value,"\\b"), df[,column_name]) # grep seems to be a little faster
  # value_rows <- stringr::str_which(df[,column_name], paste0("\\b",value,"\\b"))
  out <- value_rows[value_rows %in% rownames(w)]
  return(out)
}
