#' Calculate nodes per module and modules per network statistics on a list of
#' graphs with defined membership
#'
#' @param network_list a list of `igraph` graph objects. Each vertex must have a
#'   defined `$membership`.
#'
#' @return a `data.frame()` where each row corresponds the the nth element of
#'   the network_list.
#' @export
#'
#' @examples # add at a later date
module_summary <- function(network_list){

  if (!requireNamespace(c("igraph", "dplyr", "purrr", "stats"), quietly = TRUE)) {
    stop(
      "Packages \"igraph\", \"dplyr\", \"purrr\", and \"stats\" must be installed to use this function.",
      call. = FALSE
    )
  }
  sum_list <- lapply(network_list, function(x) {
    y1 <- igraph::V(x)$membership %>%
      dplyr::as_tibble() %>%
      dplyr::count(value) %>%
      dplyr::select(n)
    y2 <- igraph::V(x)$membership %>%
      dplyr::as_tibble() %>%
      dplyr::count(value) %>%
      dplyr::select(n) %>%
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




