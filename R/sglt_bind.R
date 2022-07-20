#' Internal to simulate_non_independence2
#'
#' Handles sub group list transformations
#'
#' @keywords internal
sglt_bind <- function(a, b){
  t0 <- a["locations"][[1]]
  t_home <- t0 %>% dplyr::filter(.data$at_home == "home")
  t_away <- t0 %>% dplyr::filter(.data$at_home == "away")

  t_away_sub_list <- t_away %>%
    dplyr::mutate(sub_id = rep(1:b, dplyr::n()/b)) %>%
    split(.$sub_id)

  sub_group_list <- lapply(t_away_sub_list, merge, t_home, all = TRUE)
  sub_group_list <- lapply(sub_group_list, function(x){dplyr::arrange(x, .data$cumulative_time) %>% tidyr::fill(.data$sub_id, .direction = "up")})

  # convert to state//start//end format
  sgl_transformed <- lapply(sub_group_list, new_dt_fxn)
  # out <- dplyr::bind_rows(sgl_transformed) # don't bind just yet
  out <- sgl_transformed
  return(out)
}
