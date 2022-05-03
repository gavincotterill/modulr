#' graph non independence
#' @param t2 a dataframe from simulate_non_independence()
#' #' @examples
#' \donttest{
#' t2 <- simulate_non_independence(n_groups = 4,
#' time_to_leave = 5,
#' time_to_return = 2,
#' cohesion = 0.8,
#' travel_time = c(0,2),
#' sampling_duration = 7,
#' samples_per_day = 1)
#' g <- graph_from_non_independence(t2)
#' }
#' @export

graph_from_non_independence <- function(t2){
  n_groups <- max(t2$state)
  ids <- stringr::str_split(paste(t2$members[1:n_groups], collapse = "-"), "-")[[1]]
  animals_list = replicate(n = length(ids),
                           expr = {t2},
                           simplify = F)
  names(animals_list) <- ids

  a2 <- purrr::map2(animals_list, ids, ~ dplyr::filter(., stringr::str_detect(members, .y) ) %>%
                      dplyr::select(state, start, end) %>%
                      dplyr::mutate(id = .y,
                                    time = end - start))

  # create dyads df
  vals <- unique(c(ids, ids))
  dyads <- data.frame(t(combn(vals, 2)))
  names(dyads) <- c("Var1", "Var2")

  # adj_mat <- matrix(NA, 5 * n_groups, 5 * n_groups)
  adj_mat <- matrix(NA, length(ids), length(ids))

  row.names(adj_mat) <- colnames(adj_mat) <- ids

  dyads$time_together <- NA
  dyads$max_time <- NA
  for(d in 1:nrow(dyads)){
    tt1 <- a2[[dyads[d,"Var2"]]]
    tt2 <- a2[[dyads[d,"Var1"]]]
    a3 <- dplyr::inner_join(tt1, tt2, by = c("state", "start", "end"))

    time_together <- sum(a3$time.x)
    max_time <- sum(tt1$time)
    dyads$time_together[d] <- time_together
    dyads$max_time <- max_time
    adj_mat[dyads[d,"Var1"], dyads[d,"Var2"]] <- time_together / max_time
  }

  g <- igraph::graph.adjacency(adj_mat, mode = "upper", weighted = TRUE, diag = FALSE)
  memberships <- stringr::str_extract(ids, "\\d{1,}(?=_)")
  igraph::V(g)$membership <- as.numeric(memberships) # memberships need to be numeric
  return(g)
}

