% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_crossing.R
\name{graph_crossing}
\alias{graph_crossing}
\title{Calculate Graph Crossing Time}
\usage{
graph_crossing(schedule, exposure_time, infectious_time, index_case)
}
\arguments{
\item{schedule}{a schedule object from simulate_schedule()}

\item{exposure_time}{for graph-crossing times, 0, otherwise the number of days prior to becoming infectious}

\item{infectious_time}{for graph-crossing times, a large value of the simulation duration, otherwise the number of days infectious}

\item{index_case}{an animal id from the schedule_object to be the first infected}
}
\description{
Recursive function to calculate graph crossing time and do individual-based SEIR modeling
}
\examples{
\donttest{
obj <- simulate_schedule(n_animals = 15,
                         n_groups = 3,
                         time_to_leave = 5,
                         time_to_return = 2,
                         travel_time = c(0.001, 0.2),
                         sampling_duration = 20,
                         simulator = "independent")
out <- graph_crossing(schedule = obj,
                      exposure_time = 2,
                      infectious_time = 5,
                      index_case = names(obj)[[1]])
}
}
