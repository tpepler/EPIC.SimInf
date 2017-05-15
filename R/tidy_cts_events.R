#********************************************************************
# Function to tidy CTS movement data (events)

tidy_cts_events <- function(modeldata,
                            events_file = NULL
                            #outfile = NULL
                            ){

  # Check if events data already tidied?
  if(modeldata$stepslog['tidy_cts_events'] == TRUE){
    stop('The CTS events data are already tidied!')
  }

  # # Create outfile name if missing
  # if(is.null(outfile)){
  #   outfile <- paste('siminf_', modeldata$model, '_events_',
  #                    str_sub(modeldata$startdate, start = 1, end = 4), '-',
  #                    str_sub(modeldata$startdate, start = 6, end = 7), '-',
  #                    str_sub(modeldata$startdate, start = 9, end = 10), '_',
  #                    str_sub(modeldata$enddate, start = 1, end = 4), '-',
  #                    str_sub(modeldata$enddate, start = 6, end = 7), '-',
  #                    str_sub(modeldata$enddate, start = 9, end = 10), '.csv',
  #                    sep = '')
  # }

  # Print error message if neither events data nor valid file specified
  if(is.null(modeldata$events) & is.null(events_file)){
    stop('An events data frame or file should be specified.')
  }

  # Tidy extracted CTS events data
  cat('Tidying events data... ')

  if(!is.null(modeldata$events)){
    eventdata <- modeldata$events
  } else {
    eventdata <- read.csv(events_file)
  }

  # Remove last (row count) row, if applicable
  if(is.na(eventdata$animals[nrow(eventdata)])){
    eventdata <- eventdata[-c(nrow(eventdata)), ]
  }

  # Format time column
  #startdate <- as.numeric(min(as.Date(eventdata$movement_date))) - 1
  #eventdata$time <- as.numeric(as.Date(eventdata$movement_date)) - startdate
  eventdata$time <- as.Date(eventdata$movement_date)

  # Format event column
  eventdata$event <- rep(NA, times = nrow(eventdata))
  eventdata$event[!is.na(eventdata$off_location_id) & is.na(eventdata$on_location_id)] <- 0 # exit (deaths)
  eventdata$event[is.na(eventdata$off_location_id) & !is.na(eventdata$on_location_id)] <- 1 # enter (births)
  eventdata$event[!is.na(eventdata$off_location_id) & !is.na(eventdata$on_location_id)] <- 3 # transfers between locations

  # Format node column
  eventdata$node <- rep(NA, times = nrow(eventdata))
  eventdata$node[eventdata$event == 0] <- eventdata$off_location_id[eventdata$event == 0] # exit (deaths)
  eventdata$node[eventdata$event == 1] <- eventdata$on_location_id[eventdata$event == 1] # enter (births)
  eventdata$node[eventdata$event == 3] <- eventdata$off_location_id[eventdata$event == 3] # transfers between locations

  # Format dest column
  eventdata$dest <- rep(0, times = nrow(eventdata))
  eventdata$dest[eventdata$event == 3] <- eventdata$on_location_id[eventdata$event == 3] # transfers between locations

  # Format n column
  eventdata$n <- eventdata$animals

  # Format proportion column
  eventdata$proportion <- rep(0, times = nrow(eventdata))

  # Format shift column
  eventdata$shift <- rep(0, times = nrow(eventdata))

  # Format select column
  eventdata$select <- rep(2, times = nrow(eventdata))

  # Select only applicable columns
  eventdata <- eventdata[, c('event', 'time', 'node', 'dest', 'n', 'proportion', 'select', 'shift')]

  # # Save tidied CTS events data to file on local system
  # write.csv(eventdata, file = outfile, row.names = F, quote = F)

  # Update modeldata
  modeldata$events <- eventdata
  modeldata$stepslog['tidy_cts_events'] <- TRUE

  cat('done\n') # done tidying events

  return(modeldata)
}

#********************************************************************

