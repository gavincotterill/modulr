#' plot_sampled_graph()
#'
#' @param g_obs, an igraph object output from sample_graph()
#' @inheritParams plot_simulated_graph
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' g <- simulate_graph(n_animals = 25,
#'                     n_groups = 4,
#'                     time_to_leave = 5,
#'                     time_to_return = 2,
#'                     samples_per_day = 1,
#'                     sampling_duration = 7,
#'                     time_cut = 7)
#'
#' igraph::V(g)$name <- stringr::str_extract(igraph::V(g)$name, "\\d{1,}")
#'
#' g_obs <- sample_graph(
#'   graph = g,
#'   missingness = 0.5,
#'   propGPS = 1,
#'   regime = "better")
#'
#' plot_sampled_graph(g_obs, g)
#'
plot_sampled_graph <- function(g_obs, g){

  grp <- data.frame(name = igraph::V(g)$name,
                    mem = igraph::V(g)$membership)

  mark_col <- grDevices::rainbow(length(unique(grp$mem)), alpha = 0.3)
  mark_border <- grDevices::rainbow(length(unique(grp$mem)), alpha = 1)

  grp$mc <- mark_col[grp$mem]
  grp$mb <- mark_border[grp$mem]

  obs_lists <- grp %>%
    filter(name %in% V(g_obs)$name) %>%
    group_by(mem) %>%
    nest %>%
    mutate(data = map(data, as.list)) %>%
    pull(data)

  grp_list_obs <- lapply(obs_lists, `[[`, "name")

  mcs_obs <- lapply(obs_lists, `[[`, "mc") %>%
    flatten() %>%
    unique() %>%
    unlist()

  mbs_obs <- lapply(obs_lists, `[[`, "mb") %>%
    flatten() %>%
    unique() %>%
    unlist()

  plot(g_obs,
       layout = layout.fruchterman.reingold(g_obs),
       edge.width = E(g_obs)$sim_weight*2,
       edge.color = "black",
       mark.groups = grp_list_obs,
       mark.col = mcs_obs,
       mark.border = mbs_obs,
       vertex.color = V(g_obs)$membership,
       vertex.frame.color =  "grey20")
}
