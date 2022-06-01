#' get times
#' @param schedule a dataframe schedule of the type output by simulate_schedule
#' @param id the character string name of an individual node
#' @keywords internal
get_times <- function(schedule, id, simulator){
  if(simulator == "independent"){
    grp <- stringr::str_extract(id, "\\d{1,}(?=_)")
    max_time <- max(schedule$end)
    p <- schedule %>% dplyr::mutate(time = end - start)
    p3 <- p %>% dplyr::filter(state == as.numeric(grp))
    p4 <- p %>% dplyr::filter(state != as.numeric(grp))
    out <- data.frame(id = id,
                      grp = grp,
                      time_at_home = sum(p3$time) / max_time,
                      time_not_at_home = sum(p4$time) / max_time
    )
    return(out)
  }else if(simulator == "group-think"){
    grp <- stringr::str_extract(id, "\\d{1,}(?=_)")
    max_time <- max(schedule$end)
    p <- schedule %>% dplyr::mutate(time = end - start)
    STR <- suppressWarnings(stringr::str_detect(stringr::str_split(p$state, "-"), grp)) # state is actually vector

    p3 <- p[which(STR == TRUE),]
    p4 <- p[which(STR == FALSE),]
    out <- data.frame(id = id,
                      grp = grp,
                      time_at_home = sum(p3$time) / max_time,
                      time_not_at_home = sum(p4$time) / max_time
    )
    return(out)
  }

}
