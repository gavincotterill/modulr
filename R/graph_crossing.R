#' Recursive function to calculate graph crossing time and do agent-based SEIR modeling
#' @param schedule first element(?) output of the animals_transformed function
#' @param exposure_time E
#' @param infectious_time I
#' @param index_case the animal to start the transmission chain-- an integer in quotes
#' @export

graph_crossing <- function(schedule, exposure_time, infectious_time, index_case){

  N <- length(schedule)

  transmissions <- data.frame(from = as.character(),
                              to = as.character(),
                              state = as.numeric(),
                              time_exposed = as.numeric(),
                              time_infected = as.numeric(),
                              end_infectious = as.numeric(),
                              time_step = as.numeric())

  if(nrow(transmissions) == 0){
      time_step <- 0

      # this part gets the modules and time frames from the index case
      start_infectious <- time_step + exposure_time
      end_infectious <- time_step + exposure_time + infectious_time

      infectious <- schedule[[index_case]] %>%
        dplyr::filter(start <= end_infectious,
                      end >= start_infectious) %>%
        dplyr::mutate(start = ifelse(start <= start_infectious, start_infectious, start),
                      end = ifelse(end >= end_infectious, end_infectious, end))

      # new_modules <- infectious[,"state"]$state # not sure if $ is going to work down the road
      new_modules <- infectious[,"state"][[1]] # not sure if $ is going to work down the road

      first_row <- data.frame(from = "imported",
                              to = index_case,
                              state = as.numeric(infectious[1, "state"]),
                              time_exposed = 0,
                              time_infected = start_infectious,
                              end_infectious = start_infectious + infectious_time,
                              time_step = time_step)

      transmissions <- rbind(transmissions, first_row)

      time_step <- time_step + 1

      message(paste0("There are ", length(schedule), " animals in memo at the beginning of time_step ", time_step)) # 50 start 1

      new_list <- lapply(schedule, bolts, infectious)
      new_exposures <- new_list[unlist(lapply(new_list, function(x) nrow(x) > 0 ))]

      for(j in 1:length(new_exposures)){
        new_row <- data.frame(from = index_case, to = names(new_exposures[j]),
                              state = as.numeric(new_exposures[[j]][1, "state"]),
                              time_exposed = as.numeric(new_exposures[[j]][1, "start"]),
                              time_infected = as.numeric(new_exposures[[j]][1, "start"]) + exposure_time,
                              end_infectious = as.numeric(new_exposures[[j]][1, "start"]) + exposure_time + infectious_time,
                              time_step = time_step)

        transmissions <- rbind(transmissions, new_row) %>%
          dplyr::filter(from != to) # when exposure time is really low (zero) we don't want self-loops

      }

      complete <- transmissions[which(transmissions$time_step == (time_step - 1)), "to"]
      schedule[which(names(schedule) %in% complete)] <- NULL
    }

  recurser <- function(schedule, exposure_time, infectious_time){

        time_step <<- time_step + 1

        ## adding timestep check
        if(time_step == 100){
          message(paste0("possible run-away recursion at timestep ", time_step, ", halting process"))
          return(transmissions)
        }

        message(paste0("There are ", length(schedule), " animals in memo the beginning of time_step ", time_step))

        for(m in 1:length(schedule)){
          # m = 1
          index_case <- names(schedule)[m] # was new_exposures
          if(!index_case %in% transmissions$to){next} # if they're not infected yet, skip
          if(is.null(index_case)){ return(transmissions) } # if there are no new infections we should be done

          tmp <- transmissions %>% dplyr::filter(to == index_case)
          start_infectious <- tmp$time_infected
          end_infectious <- tmp$time_infected + infectious_time

          new_infectious <- schedule[[index_case]]
          if(!is.null(new_infectious)){ # this check might not be necessary if everything else is working

            infectious <- schedule[[index_case]]%>%
              dplyr::filter(start <= end_infectious, # this filter is throwing error
                     end >= start_infectious) %>%
              dplyr::mutate(start = ifelse(start <= start_infectious, start_infectious, start),
                     end = ifelse(end >= end_infectious, end_infectious, end))

            # new_modules <- infectious[,"state"]$state # not sure if $ is going to work down the road
            new_modules <- infectious[,"state"][[1]] # not sure if $ is going to work down the road

            schedule[[index_case]] <- NULL
            new_list <- lapply(schedule, bolts, infectious)
            new_exposures <- new_list[unlist(lapply(new_list, function(x) nrow(x) > 0 ))]

            if(length(new_exposures) == 0){
              message(paste0("ran out of new exposures at timestep ", time_step))
              return(transmissions)
              }

            for(j in 1:length(new_exposures)){
              new_row <- data.frame(from = index_case,
                                    to = names(new_exposures[j]),
                                    state = as.numeric(new_exposures[[j]][1, "state"]),
                                    time_exposed = as.numeric(new_exposures[[j]][1, "start"]),
                                    time_infected = as.numeric(new_exposures[[j]][1, "start"]) + exposure_time,
                                    end_infectious = as.numeric(new_exposures[[j]][1, "start"]) + exposure_time + infectious_time,
                                    time_step = time_step)

              # use only first exposure
              transmissions <<- rbind(transmissions, new_row) %>%
                dplyr::filter(from != to) %>% # when exposure time is really low (zero) we don't want self-loops
                dplyr::group_by(to) %>%
                dplyr::arrange(time_exposed) %>%
                dplyr::filter(time_exposed == min(time_exposed)) %>% # checked this filter
                dplyr::sample_n(1) %>% # is this redundant?... not if there are multiples at same time
                dplyr::ungroup() %>%
                dplyr::arrange(time_exposed, from)
            } # close j loop

          }# close if !is.null new infectious

        } # close m loop

    complete <- transmissions[which(transmissions$time_step == (time_step - 1)), "to"]
    schedule[which(names(schedule) %in% complete)] <- NULL # removed $to

    if(nrow(transmissions) < N){
      Recall(schedule, exposure_time, infectious_time)
    }

    return(transmissions) # added for recurser

  } # close recurser

  transmissions <- recurser(schedule, exposure_time, infectious_time)

  return(transmissions)

}

