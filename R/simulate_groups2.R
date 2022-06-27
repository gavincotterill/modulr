#' Simulate groups experimental for use in sim_non_ind2
#' @param n_splits the number of splinter groups that leave home, one or more
#' @inheritParams simulate_graph
#' @keywords internal
simulate_groups2 <- function(animals_home,
                            n_groups,
                            n_splits,
                            time_to_leave,
                            time_to_return,
                            travel_time = c(0.001, 0.001),
                            sampling_duration){
  if (time_to_return < max(travel_time)) {
    stop("time_to_return should be greater than possible travel times. Decrease travel_time and/or increase group-switching times.",call. = FALSE)
  }

  animals_other_groups <- which(c(1:n_groups) != animals_home)

  # store inputs in named list (returned at end of function)
  inputs <- list(animals_home = animals_home,
                 time_to_leave = time_to_leave,
                 time_to_return = time_to_return,
                 n_groups = n_groups,
                 sampling_duration = sampling_duration)

  # specify starting conditions (animals always start in their home group at time 0)
  current_state_now <- animals_home
  cumulative_time <- 0
  travelling_time <- 0

  # build empty storage df
  locations <- data.frame(current_state = c(animals_home),
                          waiting_time = NA,
                          cumulative_time = 0)

  while(cumulative_time < sampling_duration){

    current_state_last <- current_state_now

    if(length(current_state_last) == 1 & all(current_state_last == animals_home)){
      waiting_time_now <- stats::rexp(n = 1, 1 / (time_to_leave - travelling_time)) # waiting time from an exponential with delta if they're at home

      # travel time imposed on all
      cumulative_time <- cumulative_time + waiting_time_now

      current_state_now <- as.list(sample(animals_other_groups, size = n_splits, replace = T))


      travel_row <- c(current_state_last,
                      waiting_time_now,
                      cumulative_time)

      locations <- as.data.frame(rbind(locations, travel_row))

      travelling_time <- sample(seq(from = travel_time[1], to = travel_time[2], by = .001), 1)

      cumulative_time <- cumulative_time + travelling_time

      new_row <- list()
      for(i in 1:n_splits){
        new_row[[i]] <- c(sample(seq(0.001,0.094, by = 0.001), 1),
                          travelling_time,
                          cumulative_time)

        locations <- as.data.frame(rbind(locations, new_row[[i]]))
      }

    }else{
      waiting_time_now <- stats::rexp(n = 1, 1 / (time_to_return - travelling_time))

      cumulative_time <- cumulative_time + waiting_time_now

      current_state_now <- animals_home

      travel_row <- list()
      for(i in 1:length(current_state_last)){
        travel_row[[i]] <- c(current_state_last[[i]],
                             waiting_time_now,
                             cumulative_time)
        locations <- as.data.frame(rbind(locations, travel_row[[i]]))
      }

      travelling_time <- sample(seq(from = travel_time[1], to = travel_time[2], by = .001), 1)
      cumulative_time <- cumulative_time + travelling_time

      new_row <- list()
      for(i in 1:length(current_state_last)){
        new_row[[i]] <- c(sample(seq(0.001,0.094, by = 0.001), 1),
                          travelling_time,
                          cumulative_time)
        locations <- as.data.frame(rbind(locations, new_row[[i]]))
      }
    }
  } # end while

  # add variable for whether animal is in its "home" group
  locations$at_home <- ifelse(locations$current_state == animals_home, "home", "away")
  locations$cumulative_time <- round(locations$cumulative_time, 4)
  locations$waiting_time <- round(locations$waiting_time, 4)


  # return inputs, (continuous-time) locations, and (discrete-time) samples
  outlist <- list(inputs = inputs,
                  locations = locations,
                  animals_home = animals_home)
}
