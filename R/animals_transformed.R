#' simulate group switching to get animals_transformed df
#' @inheritParams simulate_groups
#' @export
animals_transformed <- function(n_animals,
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
                                   id = rep(paste("Animal_", a, sep = ""), nrow(animal_list[[a]]$samples)))
    names(animal_sample_df)[a] <- (paste0("Animal_", a))
  }
  names(animal_list) <- 1:length(animal_list)
  animals_transformed <- lapply(animal_list, dt_fxn)
  return(animals_transformed)
}




