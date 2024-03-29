% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_id_rate.R
\name{single_id_rate}
\alias{single_id_rate}
\title{Get the lambda and xi rates for a single individual}
\usage{
single_id_rate(sub_sched, sub_id, sim)
}
\arguments{
\item{sub_sched}{the individual's continuous time description}

\item{sub_id}{the individual's id string}

\item{sim}{the simulator used to generate the schedule}
}
\description{
Takes an individual's continuous-time movement description and calculates
the average number of days until it leaves its home state and the average
number of days until it returns to its home state.
}
\examples{
\donttest{
obj <- simulate_schedule(n_animals = 10, n_groups = 2, time_to_leave = 5,
                         time_to_return = 2, travel_time = c(0.001, 0.2), sampling_duration = 30,
                         simulator = "independent")

single_id_rate(sub_sched = obj[[1]], sub_id = names(obj)[[1]], "independent")
}
}
