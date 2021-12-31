#' plot_simulated_graph()
#'
#' @param g, an igraph object output from simulate_graph()
#'
#' @return
#' @export
#'
#' @examples
#' plot_simulated_graph(g)
#'
#' @seealso \code{\link{plot_simulated_graph}}
plot_simulated_graph <- function(g){
  igraph::V(g)$name <- stringr::str_extract(igraph::V(g)$name, "\\d{1,}")

  grp <- data.frame(name = igraph::V(g)$name,
                    mem = igraph::V(g)$membership)

  mark_col <- grDevices::rainbow(length(unique(grp$mem)), alpha = 0.3)
  mark_border <- grDevices::rainbow(length(unique(grp$mem)), alpha = 1)

  grp$mc <- mark_col[grp$mem]
  grp$mb <- mark_border[grp$mem]

  whole_lists <- grp %>%
    dplyr::group_by(mem) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = map(data, as.list)) %>%
    dplyr::pull(data)

  grp_list_whole <- lapply(whole_lists, `[[`, "name")
  mcs_whole <- lapply(whole_lists, `[[`, "mc") %>%
    purrr::flatten() %>%
    unique() %>%
    unlist()
  mbs_whole <- lapply(whole_lists, `[[`, "mb") %>%
    purrr::flatten() %>%
    unique() %>%
    unlist()

  igraph:::plot.igraph(g,
                       layout = igraph::layout.fruchterman.reingold(g),
                       edge.width = igraph::E(g)$weight,
                       edge.color = "black",
                       vertex.color = grp$mb,
                       vertex.frame.color = "grey20",
                       mark.groups = grp_list_whole,
                       mark.col = mcs_whole,
                       mark.border = mbs_whole)
}
