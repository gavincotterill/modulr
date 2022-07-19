#' Custom function to sample from integers
#'
#' Used in simulate_animal and simulate_groups function to avoid issues when
#' there are only two groups and animals_other_groups is a length 1 integer
#'
#' @param x one or more integers to sample from
#' @keywords internal
sample_vec <- function(x, ...) x[sample(length(x), ...)]
