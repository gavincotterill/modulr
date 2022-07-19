#' Internal function to simulate_non_independence2() that removes excess empty strings and dummy animals
#'
#' @param cell the holding value string from a single cell
#' @keywords internal
clean_holding <- function(cell){

  x <- stringr::str_split(cell, "/")[[1]] %>% extract_group()

  # put them in proper order
  y <- stringr::str_split(cell, "/")[[1]][order(characterRank(x))] %>%
    paste(collapse = "-") %>%
    stringr::str_split("-") %>%
    `[[`(1)
  y <- y[y != ""]

  z <- characterRank(y %>% extract_group())

  a <- y[order(z)]

  b <- unique(a)

  list_names <- b %>% extract_group() %>% unique()

  where_to_append <- purrr::map(b, ~extract_group(.x) %>% match(list_names) )%>% unlist()

  df <- data.frame(id = b, list_index = where_to_append)
  groups0 <- split(df, df$list_index) %>% `names<-`(list_names)

  grp_list <- lapply(groups0, function(x) x[,"id"])

  # if there are multiple ids per group, replace dummy with empty
  int_list <- purrr::map_if(.x = grp_list, .p = ~length(.x) > 1, .f = ~stringr::str_replace(.x, "\\d{1,}_0", ""))
  # drop dummy
  no_empty <- purrr::map(.x = int_list, .f = ~  .x[.x != ""])
  # re-form string
  new_string <- purrr::map(no_empty, ~paste(.x, collapse = "-"))
  out <- paste(new_string, collapse = "/")

  return(out)
}
