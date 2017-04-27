#********************************************************************
# Function to adjust initialisation data for specified events

#.init_adjust <- function(modeldata,
prep_cts_init <- function(modeldata
                        #outfile = NULL
                        ){

  # # Create outfile name if missing
  # if(is.null(outfile)){
  #   outfile <- paste('siminf_', modeldata$model, '_initdata_',
  #                    str_sub(modeldata$startdate, start = 1, end = 4), '-',
  #                    str_sub(modeldata$startdate, start = 6, end = 7), '-',
  #                    str_sub(modeldata$startdate, start = 9, end = 10), '.csv',
  #                    sep = '')
  # }

  # Check if init data already pre-processed?
  if(modeldata$stepslog['prep_cts_init'] == TRUE){
    stop('The CTS initialisation data are already pre-processed!')
  }

  # Check if events data tidied?
  if(modeldata$stepslog['tidy_cts_events'] == FALSE){
    stop('Please run tidy_cts_events() first.')
  }

  # Check if init data tidied?
  if(modeldata$stepslog['tidy_cts_init'] == FALSE){
    stop('Please run tidy_cts_init() first.')
  }

  # Check if events data pre-processed?
  if(modeldata$stepslog['prep_cts_events'] == FALSE){
    stop('Please run prep_cts_events() first.')
  }

  cat('Adjusting initialisation data for modelling (this may take a while)...\n')

  initdata <- modeldata$init
  eventdata <- modeldata$events

  (mintime <- min(eventdata$time))
  (maxtime <- max(eventdata$time))
  starttime <- Sys.time()
  starttime_global <- starttime
  #for(d in mintime:maxtime){
  day_u0 <- initdata
  for(d in mintime:maxtime){
    day_events <- subset(eventdata, time == d)
    n_events <- nrow(day_events)
    checkpoints_1000 <- c(0, seq(from = 1000, to = n_events, by = 1000), n_events)
    #checkpoints_1000 <- c(seq(from = 1740000, to = n_events, by = 1000), n_events) # optional: to start from previous data
    for(i in 2:(length(checkpoints_1000))){
      model <- SIR(u0 = day_u0,
                   events = day_events[1:checkpoints_1000[i], ],
                   tspan = seq(from = d, #mintime,
                               to = d + 1, #maxtime,
                               by = 1),
                   beta = 0,
                   gamma = 0)
      cc <- try(run(model), silent = TRUE)
      if(is(cc,"try-error")){
        checkpoints_100 <- seq(from = checkpoints_1000[i - 1], to = checkpoints_1000[i], by = 100)
        if(checkpoints_100[length(checkpoints_100)] != checkpoints_1000[i]){
          checkpoints_100 <- c(checkpoints_100, checkpoints_1000[i])
        }
        for(j in 2:(length(checkpoints_100))){
          model <- SIR(u0 = day_u0,
                       events = day_events[1:checkpoints_100[j], ],
                       tspan = seq(from = d, #mintime,
                                   to = d + 1, #maxtime,
                                   by = 1),
                       beta = 0,
                       gamma = 0)
          cc <- try(run(model), silent = TRUE)
          if(is(cc,"try-error")){
            checkpoints_10 <- seq(from = checkpoints_100[j - 1], to = checkpoints_100[j], by = 10)
            if(checkpoints_10[length(checkpoints_10)] != checkpoints_100[j]){
              checkpoints_10 <- c(checkpoints_10, checkpoints_100[j])
            }
            for(k in 2:(length(checkpoints_10))){
              model <- SIR(u0 = day_u0,
                           events = day_events[1:checkpoints_10[k], ],
                           tspan = seq(from = d, #mintime,
                                       to = d + 1, #maxtime,
                                       by = 1),
                           beta = 0,
                           gamma = 0)
              cc <- try(run(model), silent = TRUE)
              if(is(cc,"try-error")){
                checkpoints_1 <- seq(from = checkpoints_10[k - 1], to = checkpoints_10[k], by = 1)
                for(h in 2:(length(checkpoints_1))){
                  model <- SIR(u0 = day_u0,
                               events = day_events[1:checkpoints_1[h], ],
                               tspan = seq(from = d, #mintime,
                                           to = d + 1, #maxtime,
                                           by = 1),
                               beta = 0,
                               gamma = 0)
                  cc <- try(run(model), silent = TRUE)
                  if(is(cc,"try-error")){
                    print(day_events[checkpoints_1[h], ])
                    initdata$S[day_events$node[checkpoints_1[h]]] <- initdata$S[day_events$node[checkpoints_1[h]]] + day_events$n[checkpoints_1[h]]
                    day_u0$S[day_events$node[checkpoints_1[h]]] <- day_u0$S[day_events$node[checkpoints_1[h]]] + day_events$n[checkpoints_1[h]]
                  }
                }
              }
            }
          }
        }
      }
      cat(paste(checkpoints_1000[i], ': ', Sys.time() - starttime, '\n', sep = ''))
      starttime <- Sys.time()
    }
    # Run final model for the current day and adjust day_u0
    model <- SIR(u0 = day_u0,
                 events = day_events,
                 tspan = seq(from = d, #mintime,
                             to = d + 1, #maxtime,
                             by = 1),
                 beta = 0,
                 gamma = 0)
    day_u0$S <- susceptible(run(model))[, 2]
  }
  cat(paste('Overall time:', Sys.time() - starttime_global, '\n', sep = ''))

  #cat('done\n')

  # Update modeldata object
  modeldata$init <- initdata
  modeldata$events <- eventdata
  modeldata$stepslog['prep_cts_init'] <- TRUE

  # # Save adjusted initialisation data data to file on local system
  # write.csv(modeldata$initdata, file = outfile, row.names = T, quote = F)

  return(modeldata)
}

#********************************************************************

