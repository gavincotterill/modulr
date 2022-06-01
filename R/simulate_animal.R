#' The backbone of the network simulator, called from within simulate_graph
#'
#' @param n_groups The number of modules in the network.
#' @param time_to_leave The average amount of time spent within the home
#'   group prior to leaving (days per sampling period).
#' @param time_to_return The average amount of time spent abroad before
#'   returning to home group.
#' @param travel_time Vector of the range of values that travel to the next group can take,
#'  drawn from a uniform distribution. Default == c(0,1)
#' @param sampling_duration Default == 365.
#' @param samples_per_day Default == 1.
#'
#' @return A vector of sampling locations for a single animal.
#' @export
#'
#' @examples
#' \donttest{
#' simulate_animal(time_to_leave = 3,
#'                 time_to_return = 1,
#'                 travel_time = c(0.01, 1),
#'                 n_groups = 4,
#'                 samples_per_day = 1,
#'                 sampling_duration = 7)
#'}
#'
simulate_animal <- function(n_groups,
                            time_to_leave,
                            time_to_return,
                            travel_time = c(0.001, 0.002),
                            sampling_duration,
                            samples_per_day = 1
){

  if (!requireNamespace(c("stats"), quietly = TRUE)) {
    stop("Package \"stats\" must be installed to use this function.",call. = FALSE)
  }
  # generate home group for each animal at random
  # NOTE: this assumes that all animals are equally likely to choose all groups,
  #       which _should_ lead to groups that are approximately uniform in size.
  animals_home <- sample(1:n_groups, size = 1)
  animals_other_groups <- which(c(1:n_groups) != animals_home)

  # convert expected times to departure rates
  # delta <- 1/time_to_leave
  # xi <- 1/time_to_return

  # store inputs in named list (returned at end of function)
  inputs <- list(time_to_leave = time_to_leave,
                 time_to_return = time_to_return,
                 n_groups = n_groups,
                 samples_per_day = samples_per_day,
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


  }

  # add variable for whether animal is in its "home" group
  locations$at_home <- ifelse(locations$current_state == animals_home, "home", "away")

  # sample group membership on a fixed time grid and record ----
  sample_times <- seq(1:(sampling_duration * samples_per_day)) / samples_per_day # added parentheses around sampling dur * spd
  sample_locations <- rep(NA, length(sample_times))
  for(i in 1:length(sample_times)){
    sample_locations[i] <- locations$current_state[min(which(locations$cumulative_time >= sample_times[i]))]
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
