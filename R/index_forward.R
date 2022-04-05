#' Index forward
#' @inheritParams index_back
#' @keywords internal
index_forward <- function(df, column_name, value, i){
  idx <- df$idx
# index_forward <- function(df, column_name, value, i, idx = t2$idx){
  (w <- df[idx == idx[i]+1,])
  # (value_rows <- stringr::str_which(df[, column_name], value))
  (value_rows <- grep(paste0("\\b",value,"\\b"), df[,column_name]))

  out <- value_rows[value_rows %in% rownames(w)]
  # value_rows[min(which(value_rows > i))]
  # value_rows[min(which(value_rows > i) & which(idx == idx[i] + 1)) ]
  if(length(out) > 1){stop(print(i))}else{return(out)}
}
