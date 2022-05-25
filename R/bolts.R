#' internal function to graph crossing
#' @param animal unique individual
#' @param infectious a data.table with infectious individuals from previous time step
#' @keywords internal

bolts <- function(animal, infectious){
  new_overlaps <- data.frame()
  for(i in 1:nrow(infectious)){
    mod <- as.numeric(infectious[i, "state"])
    x <- dplyr::filter(animal, state == mod)
    y <- data.table::setkey(infectious[i,2:3])

    df <- na.omit(data.table::foverlaps(x,y)) %>%
      dplyr::mutate(start = ifelse(i.start > start, i.start, start),
                    end = ifelse(i.end < end, i.end, end)) %>%
      dplyr::select(state, start, end)

    new_overlaps <- rbind(new_overlaps, df)
  }
  return(new_overlaps)
}
