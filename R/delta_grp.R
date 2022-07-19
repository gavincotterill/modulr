#' Internal to 'simulate_non_independence'
#'
#' Categorizes group-switching event as 'fission', 'fusion', 'fission-fusion' or 'same'.
#'
#' @inheritParams index_back
#' @keywords internal
delta_grp <- function(df, column_name, value, i){
  out <- tryCatch({
    if(length(stringr::str_split(df[i, column_name], "-")[[1]]) > 1){
      vals <- stringr::str_split(df[i, column_name], "-")[[1]]
      find_vals <- list()
      prev_ocrs <- list()
      prev_vals <- list()
      for(j in seq_along(vals)){
        find_vals[j] <- vals[[j]]
        prev_ocrs[[j]] <- grep(paste0("\\b", find_vals[[j]], "\\b"), df[,column_name]) # I don't know why I'm not using index_back here
        prev_vals[j] <- df[prev_ocrs[[j]][max(which(prev_ocrs[[j]] < i))], column_name]
      }
      unique_prev_val <- unique(prev_vals)
      prev_val <- stringr::str_split(unique_prev_val, "-")
      curr_val <- stringr::str_split(df[i, column_name], "-")[[1]]
      if(length(unlist(prev_val)) > length(curr_val) & any(!curr_val %in% prev_val[[1]])){ # if there were more before, and any unlisted current were not in listed previous
        "fission-fusion"
      }else if(length(unlist(prev_val)) > length(curr_val) & all(curr_val%in% prev_val[[1]])){ # if there were more before,  and all current were in listed previous
        "fission"
      }else if(length(unlist(prev_val)) <= length(curr_val) & all(curr_val %in% unlist(prev_val)) & !identical(prev_val[[1]], curr_val)){
        "fusion"
      }else if(identical(prev_val[[1]], curr_val)){
        "same"
      }
    }else if(length(stringr::str_split(df[i, column_name], "-")[[1]]) == 1){
      prev_ocr <- grep(paste0("\\b", value, "\\b"), df[,column_name])
      prev_val <- df[prev_ocr[max(which(prev_ocr < i))], column_name]
      curr_val <- df[i, column_name]
      if(prev_val == curr_val){
        "same"
      }
      else if(stringr::str_length(curr_val) < stringr::str_length(prev_val)){
        "fission"
      }
    }
  }, warning = function(w) { return(NA) }
  )
  out
}
