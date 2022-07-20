#' Summarize Module Count Across Networks
#'
#' Calculates nodes-per-module and modules-per-network on a list of graphs with defined memberships
#'
#' @param network_list a list of 'igraph' graph objects with community memberships.
#'
#' @return a data.frame where each row corresponds the the nth element of
#'   the network_list.
#' @export
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @import ggplot2
#' @name module_summary
#' @examples
#' \donttest{
#' g1 <- simulate_graph(n_animals = 12, n_groups = 3, time_to_leave = 4, time_to_return = 1,
#'                      travel_time = c(0.001, 0.002), sampling_duration = 30,
#'                      sampler = "discrete", samples_per_day = 1)
#' g2 <- simulate_graph(n_animals = 12, n_groups = 3, time_to_leave = 4, time_to_return = 1,
#'                      travel_time = c(0.001, 0.002), sampling_duration = 30,
#'                      sampler = "discrete", samples_per_day = 1)
#' g_list <- list(g1, g2)
#' module_summary(g_list)
#' }

utils::globalVariables(".")

module_summary <- function(network_list){

  if (!requireNamespace(c("igraph", "dplyr", "purrr", "stats"), quietly = TRUE)) {stop("Packages \"igraph\", \"dplyr\", \"purrr\", and \"stats\" must be installed to use this function.",call. = FALSE)}
  sum_list <- lapply(network_list, function(x) {
    y1 <- igraph::V(x)$membership %>%
      dplyr::as_tibble() %>%
      dplyr::count(.data$value) %>%
      dplyr::select(.data$n)
    y2 <- igraph::V(x)$membership %>%
      dplyr::as_tibble() %>%
      dplyr::count(.data$value) %>%
      dplyr::select(.data$n) %>%
      sum()
    out <- list(grps = y1, size = y2)

    return(out)
  }
  )

  t <- unique(as.vector(sapply(sum_list, function(x) x[['size']])))

  out_list <- list()
  for(i in 1:length(t)){
    tmp <- sum_list %>%
      purrr::keep(., purrr::as_mapper(~.x$size == t[i]))

    tmp2 <- lapply(tmp, function (x) { list(n = nrow(x[['grps']]),
                                            mean = mean(unlist(x[['grps']])),
                                            sd = stats::sd(unlist(x[['grps']]))) } ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(netSize = t[i])

    out_list[[i]] <- tmp2
  }

  fin <- dplyr::bind_rows(out_list[1:length(t)]) %>%
    `names<-`(c("n_modules", "mean_nodes_per_module", "sd_nodes_per_module", "total_no_nodes_in_net"))

  return(fin)
}




