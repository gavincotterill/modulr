#' get the lambda and xi rates for a single individual
#' @param sub_sched the individual's continuous time description
#' @param sub_id the individual's id string
#' @param sim the simulator used to generate the schedule
#' @keywords internal
single_id_rate <- function(sub_sched, sub_id, sim){
  home <- modulr:::extract_group(sub_id) %>% as.numeric()

  if(sim == "independent"){
    sub_sched <- sub_sched %>%
      dplyr::filter(state >= 1) %>%
      dplyr::mutate(start = ifelse(rownames(.) != 1, dplyr::lag(end), start),
                    home = ifelse(state == home, TRUE, FALSE),
                    lambda = ifelse(home == TRUE, end - start, NA),
                    xi = ifelse(home == FALSE, end - start, NA))
  }else if(sim %in% c("non-independent", "group-think")){

    test <- sub_sched %>%
      # dplyr::select(-members) %>%
      dplyr::filter(state >= 1)

    if(length(unique(test$state)) == 1){
      r <- 1
    }else{
      r <- rle(test$state)$l
    }

    sub_sched <- tryCatch({test %>%
      dplyr::mutate(cons = rep(seq(r), r)) %>%
      dplyr::group_by(state, cons) %>%
      dplyr::summarise(start = min(start),
                end = max(end)) %>%
      dplyr::arrange(start) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(start = ifelse(rownames(.) != 1, dplyr::lag(end), start),
                    home = ifelse(state == home, TRUE, FALSE),
                    lambda = ifelse(home == TRUE, end - start, NA),
                    xi = ifelse(home == FALSE, end - start, NA))},
      error = function(x){
        print(test)
        print(r)
      }
    )
  }


  ttlg <- sub_sched$lambda[-which(sub_sched$lambda %in% NA)]
  ttrg <- sub_sched$xi[-which(sub_sched$xi %in% NA)]

  out_list <- list(ttlg = mean(ttlg), ttrg = mean(ttrg))
  return(out_list)
}
