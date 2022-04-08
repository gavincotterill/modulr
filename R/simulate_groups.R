#' Simulate groups
#' @inheritParams simulate_graph
#' @keywords internal
simulate_groups <- function(animals_home,
                            n_groups,
                            time_to_leave,
                            time_to_return,
                            travel_time = c(0.01, 2),
                            sampling_duration = 7,
                            samples_per_day = 1){


  animals_other_groups <- which(c(1:n_groups) != animals_home)

  # convert expected times to departure rates
  delta <- 1/time_to_leave
  xi <- 1/time_to_return

  # store inputs in named list (returned at end of function)
  inputs <- list(animals_home = animals_home,
                 time_to_leave = time_to_leave,
                 time_to_return = time_to_return,
                 n_groups = n_groups,
                 samples_per_day = samples_per_day,
                 sampling_duration = sampling_duration)

  # specify starting conditions (animals always start in their home group at time 0)
  current_state_now <- animals_home
  cumulative_time <- 0

  # build empty storage df
  locations <- data.frame(current_state = c(animals_home),
                          waiting_time = NA,
                          cumulative_time = 0)

  while(cumulative_time < sampling_duration){

    current_state_last <- current_state_now
    waiting_time_now <- ifelse(current_state_last == animals_home,
                               stats::rexp(n = 1, delta), # waiting time from an exponential with delta if they're at home
                               stats::rexp(n = 1, xi)) # waiting time from an exponential with xi if they're away.

    # travel can be instantaneous or up to 1 day long
    # coin flip for it
    if(rbinom(1, 1, 0.5) == 1){

      cumulative_time <- cumulative_time + waiting_time_now

      current_state_now <- ifelse(current_state_last == animals_home,
                                  sample(animals_other_groups, size = 1),
                                  animals_home)

      travel_row <- c(current_state_last,
                      waiting_time_now,
                      cumulative_time)

      locations <- as.data.frame(rbind(locations, travel_row))

      travelling_time <- sample(seq(from = travel_time[1], to = travel_time[2], by = .01), 1)

      cumulative_time <- cumulative_time + travelling_time

      new_row <- c(sample(seq(0.01,0.099, by = 0.01), 1),
                   travelling_time,
                   cumulative_time)

      locations <- as.data.frame(rbind(locations, new_row))

    }else{ # same as original code, istantaneous switching
      cumulative_time <- cumulative_time + waiting_time_now

      current_state_now <- ifelse(current_state_last == animals_home,
                                  sample(animals_other_groups, size = 1),
                                  animals_home)

      new_row <- c(current_state_last,
                   waiting_time_now,
                   cumulative_time)

      locations <- as.data.frame(rbind(locations, new_row))
    }
  }

  # add variable for whether animal is in its "home" group
  locations$at_home <- ifelse(locations$current_state == animals_home, "home", "away")

  # sample group membership on a fixed time grid and record ----
  sample_times <- seq(1:(sampling_duration * samples_per_day)) / samples_per_day # added parentheses around sampling dur * spd
  sample_locations <- rep(NA, length(sample_times))
  for(i in 1:length(sample_times)){
    sample_locations[i] <- locations$current_state[min(which(as.numeric(locations$cumulative_time) >= sample_times[i]))]
  }

  # build df of samples
  samples <- as.data.frame(cbind(sample_times, sample_locations))

  # return inputs, (continuous-time) locations, and (discrete-time) samples
  outlist <- list(inputs = inputs,
                  locations = locations,
                  samples = samples,
                  animals_home = animals_home)

  return(outlist)
}
