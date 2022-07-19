#' Internal to simulate_schedule
#'
#' sim = "independent"
#'
#' @inheritParams simulate_groups
#' @keywords internal
simulate_independence <- function(n_animals,
                                n_groups,
                                time_to_leave,
                                time_to_return,
                                travel_time,
                                sampling_duration) {
  animal_list <- animal_sample_df <- vector("list", length = n_animals)
  for(a in 1:n_animals){
    animal_list[[a]] <- modulr::simulate_animal(n_groups = n_groups,
                                                time_to_leave = time_to_leave,
                                                time_to_return = time_to_return,
                                                travel_time = travel_time,
                                                sampling_duration = sampling_duration)
    animal_sample_df[[a]] <- cbind(animal_list[[a]]$samples,
                                   id = rep(paste(animal_list[[a]]$animals_home, a, sep = "_"),
                                            nrow(animal_list[[a]]$samples)))
    names(animal_sample_df)[a] <- paste(animal_list[[a]]$animals_home, a, sep = "_")
  }
  # names(animal_list) <- names(animal_sample_df)
  animals_transformed <- lapply(animal_list, dt_fxn)
  names(animals_transformed) <- names(animal_sample_df)

  n_groups_out <- length(unique(stringr::str_extract(names(animals_transformed), "\\d{1,}(?=_)")))

  if(n_groups_out != n_groups){warning(paste0(n_groups_out, " group(s) in simulated network, not ", n_groups, " as requested.
  Individuals are randomly assigned to groups.
  At small average group sizes it is possible that fewer groups are returned than desired."), call. = FALSE)}

  # out_list <- list(animals_transformed = animals_transformed, animal_list = animal_list)
  # return(out_list)
  return(animals_transformed)
}




