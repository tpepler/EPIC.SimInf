#********************************************************************
# Function to format initialisation data for specified model

tidy_cts_init <- function(modeldata,
                        #cphsizes,
                        #initdata = NULL,
                        init_file = NULL
                        #outfile = NULL
                        ){

  # Check if init data already tidied?
  if(modeldata$stepslog['tidy_cts_init'] == TRUE){
    stop('The CTS initialisation data are already tidied!')
  }

  # Check if events data already tidied?
  if(modeldata$stepslog['tidy_cts_events'] == FALSE){
    stop('Please run tidy_cts_events() first.')
  }

  # Print error message if neither init data nor valid file specified
  if(is.null(modeldata$initdata) & is.null(init_file)){
    stop('An initialisation data frame or file should be specified.')
  }

  cat('Tidying CPH initialisation data... ')

  initdata <- modeldata$init
  eventdata <- modeldata$events

  # # Create outfile name if missing
  # if(is.null(outfile)){
  #   outfile <- paste('siminf_', modeldata$model, '_cphsizes_',
  #                    str_sub(modeldata$startdate, start = 1, end = 4), '-',
  #                    str_sub(modeldata$startdate, start = 6, end = 7), '-',
  #                    str_sub(modeldata$startdate, start = 9, end = 10), '.csv',
  #                    sep = '')
  # }

  # Remove rows without a location ID
  ind <- !is.na(initdata$location_id)
  initdata <- initdata[ind, ]

  if(modeldata$model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }
  else{
    initdata <- data.frame(S = initdata$num_cattle, row.names = initdata$location_id,
                                     I = rep(0, times = nrow(initdata)),
                                     R = rep(0, times = nrow(initdata)))
  }

  # Add missing nodes to initdata
  temp <- sort(rownames(initdata))
  temp2 <- unique(c(eventdata$node[!(eventdata$node %in% temp)],
                    eventdata$dest[!(eventdata$dest %in% temp)]))
  temp3 <- data.frame(S = rep(0, times = length(temp2)),
                      I = rep(0, times = length(temp2)),
                      R = rep(0, times = length(temp2)))
  rownames(temp3) <- temp2
  initdata <- rbind(initdata, temp3)
  initdata <- initdata[order(as.numeric(rownames(initdata))), ]
  initdata <- initdata[-c(which(as.numeric(rownames(initdata)) == 0)), ] # remove row for node 0

  # Update modeldata object
  modeldata$init <- initdata
  modeldata$stepslog['tidy_cts_init'] <- TRUE

  cat('done\n')

  return(modeldata)
}

#********************************************************************
