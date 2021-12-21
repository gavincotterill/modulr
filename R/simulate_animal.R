#' The backbone of the network simulator, called from within simulate_graph
#'
#' @param time_to_leave_group The average amount of time spent within the home
#'   group prior to leaving (days per sampling period).
#' @param time_to_return_to_group The average amount of time spent abroad before
#'   returning to home group.
#' @param n_groups The number of modules in the network.
#' @param time_cut Optional, is used, the number of days prior to censoring (eg.
#'   animal died, collar died)
#' @param samples_per_day Default == 1.
#' @param sampling_duration Default == 365.
#'
#' @return A vector of sampling locations for a single animal.
#' @export
#'
#' @examples # not yet
simulate_animal <- function(time_to_leave_group,
                            time_to_return_to_group,
                            n_groups,
                            time_cut = 365,
                            samples_per_day = 1,
                            sampling_duration = 365){

  if (!requireNamespace(c("stats"), quietly = TRUE)) {
    stop(
      "Package \"stats\" must be installed to use this function.",
      call. = FALSE
    )
  }
  # generate home group for each animal at random
  # NOTE: this assumes that all animals are equally likely to choose all groups,
  #       which _should_ lead to groups that are approximately uniform in size.
  animals_home <- sample(1:n_groups, size = 1)
  animals_other_groups <- which(c(1:n_groups) != animals_home)

  # convert expected times to departure rates
  delta <- 1/time_to_leave_group
  xi <- 1/time_to_return_to_group

  # store inputs in named list (returned at end of function)
  inputs <- list(time_to_leave_group = time_to_leave_group,
                 time_to_return_to_group = time_to_return_to_group,
                 n_groups = n_groups,
                 time_cut = time_cut,
                 samples_per_day = samples_per_day,
                 sampling_duration = sampling_duration)

  # specify starting conditions (animals always start in their home group at time 0)
  current_state_now <- animals_home
  cumulative_time <- 0

  # while-loop forward through state-transitions until time_cut is reached
  # build empty storage df
  locations <- data.frame(current_state = c(animals_home),
                          waiting_time = NA,
                          cumulative_time = 0)

  while(cumulative_time < time_cut){
    current_state_last <- current_state_now
    waiting_time_now <- ifelse(current_state_last == animals_home,
                               stats::rexp(n = 1, delta), # waiting time from an exponential with delta if they're at home
                               stats::rexp(n = 1, xi)) # waiting time from an exponential with xi if they're away.
    cumulative_time <- cumulative_time + waiting_time_now

    current_state_now <- ifelse(current_state_last == animals_home,
                                sample(animals_other_groups, size = 1),
                                animals_home)

    new_row <- c(current_state_last,
                 waiting_time_now,
                 cumulative_time)

    locations <- as.data.frame(rbind(locations, new_row))
  }
  # add variable for whether animal is in its "home" group
  locations$at_home <- ifelse(locations$current_state == animals_home, "home", "away")

  # sample group membership on a fixed time grid and record ----
  sample_times <- seq(1:sampling_duration * samples_per_day) / samples_per_day
  sample_locations <- rep(NA, length(sample_times))
  for(i in 1:length(sample_times)){
    sample_locations[i] <- locations$current_state[min(which(locations$cumulative_time >= sample_times[i]))]
  }

  # build df of samples
  samples <- as.data.frame(cbind(sample_times, sample_locations))

  # return inputs, (continuous-time) locations, and (disrete-time) samples
  outlist <- list(inputs = inputs,
                  locations = locations,
                  samples = samples,
                  animals_home = animals_home)

  return(outlist)
}
