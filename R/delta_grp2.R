#' Delta grp
#' @inheritParams index_back
#' @keywords internal
delta_grp2 <- function(df, column_name, value, i){
  out <- tryCatch({
    if(length(stringr::str_split(df[i, column_name], "-")[[1]]) > 1){
      (vals <- stringr::str_split(df[i, column_name], "-")[[1]])
      find_vals <- list()
      ocrs <- list()
      prev_vals <- list()
      # next_vals <- list()
      prev_stat <- list()
      curr_stat <- df$state[i]
      # next_stat <- list()
      for(j in seq_along(vals)){
        find_vals[j] <- vals[[j]]
        ocrs[[j]] <- grep(paste0("\\b", find_vals[[j]], "\\b"), df[,column_name]) # I don't know why I'm not using index_back here
        prev_vals[j] <- df[ocrs[[j]][max(which(ocrs[[j]] < i))], column_name]
        # next_vals[j] <- df[ocrs[[j]][min(which(ocrs[[j]] > i))], column_name]
        prev_stat[j] <- df[index_back(df, "vector", vals[j], i),"state"]
        # next_stat[j] <- df[index_forward(df, "vector", vals[j], i),"state"]
      }
      prev_val <- stringr::str_split(unique(prev_vals), "-")
      curr_val <- stringr::str_split(df[i, column_name], "-")[[1]]
      # next_val <- stringr::str_split(unique(next_vals), "-")
      if(length(unlist(prev_val)) > length(curr_val) & any(!curr_val %in% prev_val[[1]])){ # if there were more before, and any unlisted current were not in listed previous
        "fission-fusion"
      }else if(length(unlist(prev_val)) > length(curr_val) & all(curr_val%in% prev_val[[1]])){ # if there were more before,  and all current were in listed previous
        "fission"
      }else if(length(unlist(prev_val)) <= length(curr_val) & all(curr_val %in% unlist(prev_val)) & !identical(prev_val[[1]], curr_val)){
        "fusion"
      # }else if((identical(prev_val[[1]], curr_val) & identical(next_stat[[1]], curr_stat))| # if the vectors and states haven't changed
      #          (identical(prev_val[[1]], curr_val) & curr_stat[[1]] < 1)){ # or if the vectors are same, states have changed, but prev state was travel
      #   "same"
      # }else if((identical(prev_val[[1]], curr_val) & !identical(next_stat[[1]], curr_stat) & curr_stat[[1]] >= 1)){ # vectors are same, states have changed and prev state was NOT travel
      #   "same-trigger"
      # }
      }else if((identical(prev_val[[1]], curr_val) & identical(prev_stat[[1]], curr_stat))| # if the vectors and states haven't changed
               (identical(prev_val[[1]], curr_val) & prev_stat[[1]] < 1)){ # or if the vectors are same, states have changed, but prev state was travel
        "same"
      }else if((identical(prev_val[[1]], curr_val) & !identical(prev_stat[[1]], curr_stat) & prev_stat[[1]] >= 1)){ # vectors are same, states have changed and prev state was NOT travel
        "same-trigger"
      }
    }else if(length(stringr::str_split(df[i, column_name], "-")[[1]]) == 1){
      ocr <- grep(paste0("\\b", value, "\\b"), df[,column_name])
      prev_val <- df[ocr[max(which(ocr < i))], column_name]
      # next_val <- df[ocr[min(which(ocr > i))], column_name]
      curr_val <- df[i, column_name]
      prev_stat <- df[index_back(df, "vector", df$vector[i], i),"state"]
      curr_stat <- df$state[i]
      # next_stat <- df[index_forward(df, "vector", df$vector[i], i),"state"]

      if(prev_val == curr_val & prev_stat != curr_stat & prev_stat >= 1){ # if 'same' but state changed and previous wasn't travel
      # if(prev_val == curr_val & next_stat != curr_stat & curr_stat >= 1){ # if 'same' but state changed and previous wasn't travel
        "same-trigger"
      }else if((prev_val == curr_val & prev_stat == curr_stat) |
               (prev_val == curr_val &  prev_stat < 1)){
      # }else if((prev_val == curr_val & next_stat == curr_stat) | (prev_val == curr_val & curr_stat < 1)){
        "same"
      }else if(stringr::str_length(curr_val) < stringr::str_length(prev_val)){
        "fission"
      }
    }
  }, warning = function(w) { return(NA) }
  )
  out
}
