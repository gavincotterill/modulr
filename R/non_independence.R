# V8 was pretty stable, now trying to reduce code redundancy

#' simulate group movements between points of attraction
#' here, the group numbers are points of attraction, each group has its preferred 'home base'
#' we first simulate groups as single entities that move between home/non-home bases
#'    according to the same rules as simulate_animal() but with travel time
#' Whether groups switch instantaneously or with travel time is currently a coin flip, but we may just want to impose travel time to all
#' Travel time is ~unif on user-designated values
#' Traveling 'states' are also ~unif 0.01 - 0.99 at .01 increments. This is for code-reasons originally, but tailoring the
#'     values here means you can make it a non-zero chance that traveling groups interact
#' Next we determine when and where the three groups overlapped in space and time
#' Then we create a list of individuals to associate with the groups that each have a residence preference
#' When groups switch on their own and don't encounter other groups, membership stays the same
#' When groups come together, all of their inviduals from the previous time step combine and interact equally
#' When groups split there's a cohesion parameter that informs the probabilities of individuals staying with the group they were last with
#' it would be nice to send individuals to their home group or home base periodically though (this isn't in here yet)
#'
#' seems like it's working ok now, the code needs to be cleaned up and where there's redundant code, turn it into a function to call internally
#' since there's nothing sending individuals home, the longer you sample for and the more they switch, the more random and less modular the networks become

rm(list = ls())
dev.off()
library(dplyr)
library(stringr)

set.seed(123)

n_groups <- 4

group_list <- list()
for(m in 1:n_groups){
  group_list[[m]] <- simulate_groups(animals_home = m,
                                      n_groups = n_groups,
                                      time_to_leave = 8,
                                      time_to_return = 4,
                                      travel_time = c(0,2),
                                      sampling_duration = 21,
                                      samples_per_day = 1)
}

names(group_list) <- 1:length(group_list)

groups_transformed <- lapply(group_list, dt_fxn)

# create all of the time intervals at once, then figure out who was where with whom afterwards
grps_trans <- lapply(group_list, function (animal) {
  animal$locations  %>%
    dplyr::mutate(end = dplyr::lead(.data$cumulative_time),
                  state = dplyr::lead(.data$current_state)) %>%
    dplyr::rename(start = .data$cumulative_time) %>%
    dplyr::select("state", "start","end") %>%
    na.omit() %>%
    mutate_all(~as.numeric(.))
})

ints <- bind_rows(grps_trans) %>%
  select(-"state")

complete_intervals <- data.frame(start = c(ints$start, ints$end)) %>%
  group_by(start) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(end = lead(start)) %>%
  slice(1:n()-1)

# now I have a list, one data.frame per id. intervals match across all dfs in list. location is tracked
comp_ints_list <- lapply(groups_transformed, interval_function, complete_intervals)

df <- dplyr::bind_rows(comp_ints_list, .id = "id")

# ggplot2::ggplot(df) +
#   ggplot2::geom_linerange(ggplot2::aes(x = state,
#                                        ymin = start, ymax = end,
#                                        group = as.factor(id),
#                                        color = as.factor(id)),
#                           position = ggplot2::position_dodge(width = .2),
#                           size = 2) +
#   ggplot2::theme_bw()

test <- df %>%
  group_by(state, start, end) %>%
  summarise(vector=paste(id, collapse="-")) %>%
  ungroup() %>%
  arrange(state, start, end)

# initiate groups
test$members <- mapply(function(start, vector) ifelse(start==0, initiate_group(vector, 5), NA), test$start, test$vector)

t2 <- test %>%
  arrange(start) %>%
  mutate(idx = match(start, unique(start))) %>%
  data.frame() %>%
  mutate(action = NA,
         holding = NA)

for(i in 1:nrow(t2)){
  value <- paste(str_split(t2$vector[i], "-")[[1]], collapse = "|")
  t2$action[i] <- delta_grp(t2, "vector", value, i)
}

cohesion = 0.8 # how much should they stick to their previous group?

for(i in 1:nrow(t2)){
  if(!t2$members[i] %in% NA & t2$action[i] %in% c(NA)){
    next
  }else if(t2$action[i] == "same"){
    # take care of current line
    t2$members[i] <- t2$members[index_back(t2, "vector", t2$vector[i], i)]
    # look forward
    if(t2$start[i] == max(t2$start)){next}
    curr_vec <- str_split(t2$vector[i], "-")[[1]]
    n <- length(curr_vec)
    mbrs_list <- list()
    for(j in 1:n){
      mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
      names(mbrs_list)[j] <- curr_vec[[j]]
    }
    t2 <- ff_forward(t2, curr_vec, n, mbrs_list)
  }else if(t2$action[i] == "fusion"){
    # take care of current, but these will also be used looking forward
    curr_vec <- str_split(t2$vector[i], "-")[[1]]
    n <- length(curr_vec)
    mbrs_list <- list()
    for(j in 1:n){
      mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
      names(mbrs_list)[j] <- curr_vec[[j]]
    }
    t2$members[i] <- paste(unlist(mbrs_list), collapse = "/") # assign current
    # look forward
    if(t2$start[i] == max(t2$start)){next}
    t2 <- ff_forward(t2, curr_vec, n, mbrs_list)
  } else if(t2$action[i] == "fission"){
    # members should always be accounted for to start
    if(t2$vector[i] != t2$holding[i]){stop(paste("All groups not accounted for at start of line", i, ", fission group."))}
    # look forward
    if(t2$start[i] == max(t2$start)){next}
    curr_vec <- str_split(t2$vector[i], "-")[[1]]
    n <- length(curr_vec)
    mbrs_list <- list()
    for(j in 1:n){
      mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
      names(mbrs_list)[j] <- curr_vec[[j]]
    }
    t2 <- ff_forward(t2, curr_vec, n, mbrs_list)
  }else if(t2$action[i] == "fission-fusion"){
    # members should always be accounted for to start
    if(t2$vector[i] != t2$holding[i]){stop(paste("All groups not accounted for at start of line", i, ", fission-fusion group"))}
    # look forward
    if(t2$start[i] == max(t2$start)){next}
    curr_vec <- str_split(t2$vector[i], "-")[[1]]
    n <- length(curr_vec)
    mbrs_list <- list()
    for(j in 1:n){
      mbrs_list[[j]] <- t2$members[index_back(t2, "vector", curr_vec[j], i)]
      names(mbrs_list)[j] <- curr_vec[[j]]
    }
    t2 <- ff_forward(t2, curr_vec, n, mbrs_list)
  }
}


#' make the list of unique individuals and use str_detect on t2 to get all rows for that id
#' then you'd have n_individuals data.frames with state, start, end, id
#' then you'd run dyads

ids <- str_split(paste(t2$members[1:n_groups], collapse = "-"), "-")[[1]]
animals_list = replicate(n = length(ids),
                        expr = {t2},
                        simplify = F)
names(animals_list) <- ids

a2 <- purrr::map2(animals_list, ids, ~ dplyr::filter(., str_detect(members, .y) ) %>%
                    select(state, start, end) %>%
                    mutate(id = .y,
                           time = end - start))

# create dyads df
vals <- unique(c(ids, ids))
dyads <- data.frame(t(combn(vals, 2)))
names(dyads) <- c("Var1", "Var2")

adj_mat <- matrix(NA, 5 * n_groups, 5 * n_groups)
row.names(adj_mat) <- colnames(adj_mat) <- ids

dyads$time_together <- NA
dyads$max_time <- NA
for(d in 1:nrow(dyads)){
  tt1 <- a2[[dyads[d,"Var2"]]]
  tt2 <- a2[[dyads[d,"Var1"]]]
  a3 <- inner_join(tt1, tt2, by = c("state", "start", "end"))

  time_together <- sum(a3$time.x)
  max_time <- sum(tt1$time)
  dyads$time_together[d] <- time_together
  dyads$max_time <- max_time
  adj_mat[dyads[d,"Var1"], dyads[d,"Var2"]] <- time_together / max_time
}

g <- igraph::graph.adjacency(adj_mat, mode = "upper", weighted = TRUE, diag = FALSE)
memberships <- str_extract(ids, "\\d")
igraph::V(g)$membership <- as.numeric(memberships) # memberships need to be numeric

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

vertex.size = 40
mark.expand = 25
vertex.label = igraph::V(g)$name
vertex.label.cex = 1.5
title = ""

igraph::plot.igraph(g, layout = lo, xlim = c(xmin, xmax),
                    ylim = c(ymin, ymax), rescale = F, edge.width = igraph::E(g)$weight,
                    edge.color = "black", vertex.color = grp$mb, vertex.frame.color = "grey20",
                    vertex.label = vertex.label, vertex.label.cex = vertex.label.cex,
                    mark.groups = grp_list_whole, mark.col = mcs_whole, mark.border = mbs_whole,
                    vertex.size = vertex.size, mark.expand = mark.expand,
                    main = title)


