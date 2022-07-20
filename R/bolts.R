#' Internal Function to graph_crossing()
#'
#' Identifies instances where an individual was co-located with an infectious animal
#'
#' @param animal unique individual
#' @param infectious a data.table with infectious individuals from previous time step
#' @keywords internal

bolts <- function(animal, infectious){
  new_overlaps <- data.frame()
  for(i in 1:nrow(infectious)){
    mod <- as.numeric(infectious[i, "state"])
    x <- dplyr::filter(animal, .data$state == mod)
    y <- data.table::setkey(infectious[i,2:3])

    df <- stats::na.omit(data.table::foverlaps(x,y)) %>%
      dplyr::mutate(start = ifelse(.data$i.start > .data$start, .data$i.start, .data$start),
                    end = ifelse(.data$i.end < .data$end, .data$i.end, .data$end)) %>%
      dplyr::select(.data$state, .data$start, .data$end)

    new_overlaps <- rbind(new_overlaps, df)
  }
  return(new_overlaps)
}
