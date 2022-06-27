#' Check that all ids are accounted for at each time step using output from simulate_schedule
#' @export
check_ids <- function(t2){
  t3 <- t2 %>%
    dplyr::group_by(start) %>%
    dplyr::summarise(mems = paste(members, collapse = "-")) %>%
    dplyr::mutate(mems = stringr::str_replace_all(mems, "/", "-")) %>%
    dplyr::mutate(mems = stringr::str_split(mems, "-"))
  t4 <- lapply(t3$mems, unlist)
  t5 <- purrr::map(t4, sort)
  list_of_duplicates <- purrr::map(t5, ~.x[which(duplicated(.x) == TRUE)])

  list_of_duplicates[which(list_of_duplicates == "")] <- NULL
  test1 <- all(lapply(list_of_duplicates, identical, character(0)) == TRUE)

  allSame <- function(x) length(unique(x)) == 1
  t6 <- purrr::map2(t5, "", ~ stringr::str_subset(., .y) )
  test2 <- allSame(t6) # should return true

  if(test1 & test2 == TRUE){
    print("Both tests passed")
  }else{
    print(paste(test1, test2))
  }
}
