#' Internal to sglt_bind
#'
#' Helper to handle sub-group splitting
#'
#' @param obj_in_sgl a dataframe from sub_group_list
#' @keywords internal
new_dt_fxn <- function(obj_in_sgl){
  t1 <- obj_in_sgl%>%
    dplyr::rename(end = .data$cumulative_time,
                  state = .data$current_state) %>%
    dplyr::mutate(start = .data$end - .data$waiting_time) %>%
    dplyr::select("state", "start","end", "sub_id") %>%
    na.omit() %>%
    dplyr::mutate_all(~as.numeric(.))
}
