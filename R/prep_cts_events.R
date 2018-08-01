#********************************************************************
# Function to pre-process CTS movement data (events)

prep_cts_events <- function(modeldata){
  # modeldata: list with initialisation and events data, and the model type

  # Check if events data already pre-processed?
  if(modeldata$stepslog['prep_cts_events'] == TRUE){
    stop('The CTS events data are already pre-processed!')
  }

  # Check if events data already tidied?
  if(modeldata$stepslog['tidy_cts_events'] == FALSE){
    stop('Please run tidy_cts_events() first.')
  }

  # Check if init data already tidied?
  if(modeldata$stepslog['tidy_cts_init'] == FALSE){
    stop('Please run tidy_cts_init() first.')
  }

  initdata <- modeldata$init
  eventdata <- modeldata$events

  if(modeldata$model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }
  else{
    cat('Preparing events data for modelling... ')
    # Ensure select column has correct values for SIR model
    eventdata$select[eventdata$event == 'exit'] <- 4
    eventdata$select[eventdata$event == 'enter'] <- 1
    #eventdata <- eventdata[eventdata$event != 2, ]
    eventdata$select[eventdata$event == 'extTrans'] <- 4
  }

  # Process entry events on previous day
  firstday <- min(eventdata$time)
  eventdata$time[eventdata$event == 'enter' & eventdata$time > firstday] <- eventdata$time[eventdata$event == 'enter' & eventdata$time > firstday] - 1

  # Ensure 'node' and 'dest' columns in event data refer to rows of initialisation data
  #require(plyr)
  lookuptab <- data.frame(location_id = c(0, as.numeric(rownames(initdata))),
                          node_num = c(0, 1:length(rownames(initdata))))
  events_nodes <- data.frame(location_id = eventdata$node)
  temp <- plyr::join(x = events_nodes, y = lookuptab, by = c('location_id'), type = 'left', match = 'first')
  eventdata$node <- temp$node_num
  events_dest <- data.frame(location_id = eventdata$dest)
  temp <- plyr::join(x = events_dest, y = lookuptab, by = c('location_id'), type = 'left', match = 'first')
  eventdata$dest <- temp$node_num

  # Update modeldata object
  modeldata$events <- eventdata
  modeldata$stepslog['prep_cts_events'] <- TRUE

  cat('done\n')

  return(modeldata)
}

#********************************************************************

