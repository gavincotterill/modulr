#' plot_transmissions()
#'
#' @param g, an igraph object output from simulate_graph()
#' @param vertex.size, node size in plot
#' @param mark.expand, padding of polygon denoting modules around nodes
#' @param vertex.label, label for graph vertices
#' @param vertex.label.cex, size for vertex labels
#' @param title, plot title
#'
#' @return
#' a plot of the igraph object
#'
#' @export
#' @importFrom rlang .data

plot_transmissions <- function(g,
                              vertex.size = 40,
                              vertex.label =  NA,
                              edge.arrow.size = 0.5,
                              vertex.label.cex = 1,
                              title = ""){
  grp <- data.frame(name = igraph::V(g)$name,
                    mem = igraph::V(g)$membership)

  mark_col <- grDevices::rainbow(length(unique(grp$mem)), alpha = 0.3)
  mark_border <- grDevices::rainbow(length(unique(grp$mem)), alpha = 1)

  grp$mc <- mark_col[grp$mem]
  grp$mb <- mark_border[grp$mem]

  whole_lists <- grp %>%
    dplyr::group_by(.data$mem) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, as.list)) %>%
    dplyr::pull(.data$data)

  grp_list_whole <- lapply(whole_lists, `[[`, "name")
  mcs_whole <- lapply(whole_lists, `[[`, "mc") %>%
    purrr::flatten() %>%
    unique() %>%
    unlist()
  mbs_whole <- lapply(whole_lists, `[[`, "mb") %>%
    purrr::flatten() %>%
    unique() %>%
    unlist()

  set.seed(123)
  lo_whole <- igraph::layout.fruchterman.reingold(g) %>%
    data.frame()
  lo_whole$name <- igraph::V(g)$name

  lo <- lo_whole[,1:2] %>%
    data.matrix()
  xbuf <- ybuf <- .1
  xmin <- min(lo[,1] - xbuf)
  xmax <- max(lo[,1] + xbuf)
  ymin <- min(lo[,2] - ybuf)
  ymax <- max(lo[,2] + ybuf)

  igraph::plot.igraph(g,
                      layout = lo,
                      xlim = c(xmin, xmax),
                      ylim = c(ymin, ymax),
                      rescale = F,
                      edge.width = igraph::E(g)$weight,
                      edge.color = "black",
                      edge.arrow.size = edge.arrow.size,
                      vertex.color = grp$mb,
                      vertex.frame.color = "grey20",
                      vertex.label = vertex.label,
                      vertex.label.cex = vertex.label.cex,
                      vertex.size = vertex.size,
                      main = title)
}
