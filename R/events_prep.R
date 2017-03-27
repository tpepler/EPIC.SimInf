#********************************************************************
# Function to pre-process events data

.events_prep <- function(modeldata){
  # modeldata: list with initialisation and events data, and the model type

  if(modeldata$model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }
  else{
    cat('Preparing events data for modelling... ')
    # Ensure select column has correct values for SIR model
    modeldata$eventdata <- .events2SIR(eventdata = modeldata$eventdata)
  }
  
  # Process events of type 1 (entries) on previous day
  modeldata$eventsdata$time[modeldata$eventsdata$event == 1 & modeldata$eventsdata$time > 1] <- modeldata$eventsdata$time[modeldata$eventsdata$event == 1 & modeldata$eventsdata$time > 1] - 1
  
  # Ensure 'node' and 'dest' columns in event data refer to rows of initialisation data
  #require(plyr)
  lookuptab <- data.frame(location_id = c(0, as.numeric(rownames(modeldata$initdata))),
                          node_num = c(0, 1:length(rownames(modeldata$initdata))))
  events_nodes <- data.frame(location_id = modeldata$eventdata$node)
  temp <- plyr::join(x = events_nodes, y = lookuptab, by = c('location_id'), type = 'left', match = 'first')
  modeldata$eventdata$node <- temp$node_num
  events_dest <- data.frame(location_id = modeldata$eventdata$dest)
  temp <- plyr::join(x = events_dest, y = lookuptab, by = c('location_id'), type = 'left', match = 'first')
  modeldata$eventdata$dest <- temp$node_num
  
  cat('done\n')
  
  return(modeldata)
}

#********************************************************************

