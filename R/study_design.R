#' Metadata corresponding to real_networks.
#'
#' A dataframe where each row corresponds to elements of the list of igraph
#' objects in real_networks.
#'
#' @format A data frame with 24 rows and 5 variable.
#' \describe{
#'   \item{nNodes}{The number of nodes in the simulated network}
#'   \item{nModules}{The number of modules in the simulated network}
#'   \item{ttlg}{time to leave group, the average waiting time before departure from the home group}
#'   \item{ttrg}{time to return to group, the average waiting time before returning to the home group}
#'   \item{qrel}{qrel, the modularity score of the true network}
#'   ...
#' }
"study_design"
