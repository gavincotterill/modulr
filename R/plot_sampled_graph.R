#' plot_sampled_graph()
#'
#' @param g_obs an igraph object output from sample_graph()
#' @inheritParams plot_simulated_graph
#' @return a plot of the sampled graph
#' @export
#' @importFrom rlang .data
#' @examples
#' \donttest{
#' set.seed(123)
#' library(igraph)
#' g <- modulr::simulate_graph(n_animals = 25,
#'                     n_groups = 4,
#'                     time_to_leave = 5,
#'                     time_to_return = 2,
#'                     samples_per_day = 1,
#'                     sampling_duration = 7)
#' igraph::V(g)$name <- stringr::str_extract(igraph::V(g)$name, "\\d{1,}")
#' g_obs <- sample_graph(
#'   graph = g,
#'   sample_nNodes = 13,
#'   prop_hi_res = 1,
#'   regime = "grab-two",
#'   alg = "fast_greedy")
#' par(mfrow = c(1,2))
#' plot_simulated_graph(g)
#' plot_sampled_graph(g_obs, g)
#' }
plot_sampled_graph <- function(g_obs, g, vertex.size = 40, mark.expand = 25,
                               vertex.label = NA,
                               vertex.label.cex = 1.5,
                               title = ""){

  grp <- data.frame(name = igraph::V(g)$name,
                    mem = as.numeric(igraph::V(g)$membership))

  mark_col <- grDevices::rainbow(length(unique(grp$mem)), alpha = 0.3)
  mark_border <- grDevices::rainbow(length(unique(grp$mem)), alpha = 1)

  grp$mc <- mark_col[grp$mem]
  grp$mb <- mark_border[grp$mem]

  obs_lists <- grp %>%
    dplyr::filter(.data$name %in% igraph::V(g_obs)$name) %>%
    dplyr::group_by(.data$mem) %>%
    tidyr::nest(.) %>%
    dplyr::mutate(data = purrr::map(.data$data, as.list)) %>%
    dplyr::pull(.data$data)

  grp_list_obs <- lapply(obs_lists, `[[`, "name")

  mcs_obs <- lapply(obs_lists, `[[`, "mc") %>%
    purrr::flatten() %>%
    unique() %>%
    unlist()

  mbs_obs <- lapply(obs_lists, `[[`, "mb") %>%
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

  lo2 <- lo_whole %>%
    dplyr::filter(.data$name %in% igraph::V(g_obs)$name)
  lo2 <- lo2[match(igraph::V(g_obs)$name, lo2$name),] %>%
    dplyr::select(-3) %>%
    data.matrix()

  plot(g_obs,
       layout = lo2,
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       rescale = F,
       # edge.width = igraph::E(g_obs)$sim_weight*2,
       edge.width = igraph::E(g_obs)$weight,

       edge.color = "black",
       mark.groups = grp_list_obs,
       mark.col = mcs_obs,
       mark.border = mbs_obs,
       vertex.color = igraph::V(g_obs)$membership,
       vertex.frame.color =  "grey20",
       vertex.size = vertex.size,
       vertex.label = vertex.label,
       vertex.label.cex = vertex.label.cex,
       mark.expand = mark.expand,
       main = title)
}
