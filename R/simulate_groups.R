#' Simulate groups
#' @inheritParams simulate_graph
#' @keywords internal
simulate_groups <- function(animals_home,
                            n_groups,
                            time_to_leave,
                            time_to_return,
                            travel_time = c(0.001, 2),
                            sampling_duration = 7){


  animals_other_groups <- which(c(1:n_groups) != animals_home)

  # # convert expected times to departure rates
  # delta <- 1/time_to_leave
  # xi <- 1/time_to_return

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
    waiting_time_now <- ifelse(current_state_last == animals_home,
                               stats::rexp(n = 1, 1 / (time_to_leave - travelling_time)), # waiting time from an exponential with delta if they're at home
                               stats::rexp(n = 1, 1 / (time_to_return - travelling_time))) # waiting time from an exponential with xi if they're away.

    # travel time imposed on all
    cumulative_time <- cumulative_time + waiting_time_now

    current_state_now <- ifelse(current_state_last == animals_home,
                                sample(animals_other_groups, size = 1),
                                animals_home)

    travel_row <- c(current_state_last,
                    waiting_time_now,
                    cumulative_time)

    locations <- as.data.frame(rbind(locations, travel_row))

    travelling_time <- sample(seq(from = travel_time[1], to = travel_time[2], by = .001), 1)

    cumulative_time <- cumulative_time + travelling_time

    new_row <- c(sample(seq(0.001,0.094, by = 0.001), 1), # state draw always less than 1 even with rounding (94 states to choose from means they'll almost never meet by chance during travel)
                 travelling_time,
                 cumulative_time)

    locations <- as.data.frame(rbind(locations, new_row))

  } # end while

  # add variable for whether animal is in its "home" group
  locations$at_home <- ifelse(locations$current_state == animals_home, "home", "away")

  # return inputs, (continuous-time) locations, and (discrete-time) samples
  outlist <- list(inputs = inputs,
                  locations = locations,
                  animals_home = animals_home)

  return(outlist)
}
