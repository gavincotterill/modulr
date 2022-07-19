#' Calculate the Proportion of Time at Home vs. Away
#'
#' Takes an individual's continuous-time movement description and calculates the proportion of time they spent with their home group vs any other group in their network.
#'
#' @param schedule a data.frame schedule of the type output by simulate_schedule
#' @param id the character string name of an individual node
#' @param option additional option for group-think, "co-located" or "attached"
#' @export
get_times <- function(schedule, id, simulator, option = NA){
  if(simulator %in% c("independent", "non-independent")){
    grp <- stringr::str_extract(id, "\\d{1,}(?=_)")
    max_time <- max(schedule$end)
    p <- schedule %>% dplyr::mutate(time = end - start)
    p3 <- p %>% dplyr::filter(state == as.numeric(grp))
    p4 <- p %>% dplyr::filter(state != as.numeric(grp))
    out <- data.frame(id = id,
                      grp = grp,
                      time_at_home = sum(p3$time) / max_time,
                      time_not_at_home = sum(p4$time) / max_time)
  }else if(simulator %in% c("group-think")){
    if (!option %in% c("co-located", "attached")) {
      stop("For the group-think simulator option must equal \'co-located\' or \'attached\'",call. = FALSE)
    }
    if(option == "co-located"){
      grp <- stringr::str_extract(id, "\\d{1,}(?=_)")
      max_time <- max(schedule$end)
      p <- schedule %>% dplyr::mutate(time = end - start)
      STR <- purrr::map(stringr::str_split(p$vector, "-"), stringr::str_detect, paste0("\\b",grp,"\\b"))
      idx <- sapply(STR, function(x) any(x %in% TRUE))
      p3 <- p[idx == TRUE,]
      p4 <- p[idx == FALSE,]
      out <- data.frame(id = id,
                        grp = grp,
                        time_at_home = sum(p3$time) / max_time,
                        time_not_at_home = sum(p4$time) / max_time)
    }else if(option == "attached"){
      grp <- stringr::str_extract(id, "\\d{1,}(?=_)")
      max_time <- max(schedule$end)
      p <- schedule %>% dplyr::mutate(time = end - start)
      STR <- purrr::map(stringr::str_split(p$vector, "-"), stringr::str_detect, grp)
      MBR <- purrr::map(stringr::str_split(p$members, "/"), stringr::str_detect, paste0("\\b",id, "\\b"))
      idx <- purrr::map2(STR, MBR, ~all(.x == .y))
      p3 <- p[idx == TRUE,]
      p4 <- p[idx == FALSE,]
      out <- data.frame(id = id,
                        grp = grp,
                        time_at_home = sum(p3$time) / max_time,
                        time_not_at_home = sum(p4$time) / max_time)
    }
  }
  return(out)
}
