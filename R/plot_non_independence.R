#' plot non independence
#' @inheritParams plot_simulated_graph
#' @export


plot_non_independence <- function(g,
                                  vertex.size = 40,
                                  mark.expand = 25,
                                  vertex.label = "",
                                  vertex.label.cex = 1.5,
                                  title = ""){
  # modulr::plot_simulated_graph(g) # needs to be modified slightly to work:
  # just comment out the renaming part here
  # igraph::V(g)$name <- stringr::str_extract(igraph::V(g)$name,
  #                                           "\\d{1,}")
  grp <- data.frame(name = igraph::V(g)$name, mem = igraph::V(g)$membership)
  mark_col <- grDevices::rainbow(length(unique(grp$mem)), alpha = 0.3)
  mark_border <- grDevices::rainbow(length(unique(grp$mem)),
                                    alpha = 1)
  grp$mc <- mark_col[grp$mem]
  grp$mb <- mark_border[grp$mem]
  whole_lists <- grp %>% dplyr::group_by(.data$mem) %>% tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, as.list)) %>% dplyr::pull(.data$data)
  grp_list_whole <- lapply(whole_lists, `[[`, "name")
  mcs_whole <- lapply(whole_lists, `[[`, "mc") %>% purrr::flatten() %>%
    unique() %>% unlist()
  mbs_whole <- lapply(whole_lists, `[[`, "mb") %>% purrr::flatten() %>%
    unique() %>% unlist()
  set.seed(123)
  lo_whole <- igraph::layout.fruchterman.reingold(g) %>% data.frame()
  lo_whole$name <- igraph::V(g)$name
  lo <- lo_whole[, 1:2] %>% data.matrix()
  xbuf <- ybuf <- 0.1
  xmin <- min(lo[, 1] - xbuf)
  xmax <- max(lo[, 1] + xbuf)
  ymin <- min(lo[, 2] - ybuf)
  ymax <- max(lo[, 2] + ybuf)

  igraph::plot.igraph(g, layout = lo, xlim = c(xmin, xmax),
                      ylim = c(ymin, ymax), rescale = F, edge.width = igraph::E(g)$weight,
                      edge.color = "black", vertex.color = grp$mb, vertex.frame.color = "grey20",
                      vertex.label = vertex.label, vertex.label.cex = vertex.label.cex,
                      mark.groups = grp_list_whole, mark.col = mcs_whole, mark.border = mbs_whole,
                      vertex.size = vertex.size, mark.expand = mark.expand,
                      main = title)



}
