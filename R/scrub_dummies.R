#' drop unnecessary dummy animals from a cell
#' @param cell a cell containing a string of ids
#' @keywords internal
scrub_dummies <- function(cell){
  grp_list <- stringr::str_split(cell, "/") %>%
    `[[`(1) %>%
    stringr::str_split(., "-")
  # if there are multiple ids per group, replace dummy with empty
  int_list <- purrr::map_if(.x = grp_list, .p = ~length(.x) > 1, .f = ~stringr::str_replace_all(.x, "\\d{1,}_0", ""))
  # drop dummy
  no_empty <- purrr::map(.x = int_list, .f = ~  .x[.x != ""])
  # re-form string
  new_string <- purrr::map(no_empty, ~paste(.x, collapse = "-"))
  out <- paste(new_string, collapse = "/")
  return(out)
}
