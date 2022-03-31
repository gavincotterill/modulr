#' Delta grp
#' @inheritParams index_back
delta_grp <- function(df, column_name, value, i){ # am I using 'value'?
  out <- tryCatch({
    if(str_length(df[i, column_name]) > 1){
      vals <- str_split(df[i, column_name], "-")[[1]]
      find_vals <- list()
      prev_ocrs <- list()
      prev_vals <- list()
      for(j in seq_along(vals)){
        find_vals[j] <- vals[[j]]
        prev_ocrs[[j]] <- grep(find_vals[[j]], df[,column_name])
        prev_vals[j] <- df[prev_ocrs[[j]][max(which(prev_ocrs[[j]] < i))], column_name]
      }
      unique_prev_val <- unique(prev_vals)
      # prev_val <- str_split(prev_val, "-")
      prev_val <- str_split(unique_prev_val, "-")

      curr_val <- str_split(df[i, column_name], "-")[[1]]
      if(length(unlist(prev_val)) > length(curr_val) & any(!curr_val %in% prev_val[[1]])){ # if there were more before, and any unlisted current were not in listed previous
        "fission-fusion"
      }else if(length(unlist(prev_val)) > length(curr_val) & all(curr_val%in% prev_val[[1]])){ # if there were more before,  and all current were in listed previous
        "fission"
        # }else if(length(unlist(prev_val)) <= length(curr_val) & all(curr_val %in% prev_val)){
        # }else if(length(unlist(prev_val)) < length(curr_val)){
      }else if(length(unlist(prev_val)) <= length(curr_val) & all(curr_val %in% unlist(prev_val)) & !identical(prev_val[[1]], curr_val)){
        "fusion"
        # }else if(length(unlist(prev_val)) == length(curr_val) & all(curr_val %in% unlist(prev_val))){
      }else if(identical(prev_val[[1]], curr_val)){
        "same"
      }
    }else if(str_length(df[i, column_name]) == 1){
      prev_ocr <- grep(value, df[,column_name])
      prev_val <- df[prev_ocr[max(which(prev_ocr < i))], column_name]
      curr_val <- df[i, column_name]
      if(prev_val == curr_val){
        "same"
      }
      else if(str_length(curr_val) < str_length(prev_val)){
        "fission"
      }
    }
  }, warning = function(w) { return(NA) })
  out
}
