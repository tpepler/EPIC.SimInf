#********************************************************************
# Function to format initialisation data for specified model

tidy_cts_init <- function(modeldata,
                        #cphsizes,
                        #initdata = NULL,
                        init_file = NULL
                        #outfile = NULL
                        ){

  require(stringr)

  # Check if init data already tidied?
  if(modeldata$stepslog['tidy_cts_init'] == TRUE){
    stop('The CTS initialisation data are already tidied!')
  }

  # Check if events data already tidied?
  if(modeldata$stepslog['tidy_cts_events'] == FALSE){
    stop('Please run tidy_cts_events() first.')
  }

  # Print error message if neither init data nor valid file specified
  if(is.null(modeldata$init) & is.null(init_file)){
    stop('An initialisation data frame or file should be specified.')
  }

  cat('Tidying CPH initialisation data... ')

  if(!is.null(modeldata$init)){
    initdata <- modeldata$init
  } else {
    initdata <- read.csv(init_file)
  }

  eventdata <- modeldata$events

  # # Create outfile name if missing
  # if(is.null(outfile)){
  #   outfile <- paste('siminf_', modeldata$model, '_cphsizes_',
  #                    str_sub(modeldata$startdate, start = 1, end = 4), '-',
  #                    str_sub(modeldata$startdate, start = 6, end = 7), '-',
  #                    str_sub(modeldata$startdate, start = 9, end = 10), '.csv',
  #                    sep = '')
  # }

  # Remove last (row count) row, if applicable
  if(is.na(initdata$num_cattle[nrow(initdata)])){
    initdata <- initdata[-c(nrow(initdata)), ]
  }

  # Remove rows without a location ID
  #ind <- !is.na(initdata$location_id)
  ind <- stringr::str_length(initdata$location_id) > 0
  initdata <- initdata[ind, ]

  if(!(modeldata$model %in% c('SIR', 'SEIResp', 'SLHV'))){
    stop('Error: Only SIR, SEIResp and SLHV models currently implemented.')
  } else {
    if(modeldata$model == 'SIR'){
      initdata <- data.frame(S = initdata$num_cattle, row.names = initdata$location_id,
                             I = rep(0, times = nrow(initdata)),
                             R = rep(0, times = nrow(initdata)))
    }
    if(modeldata$model == 'SEIResp'){
      initdata <- data.frame(S = initdata$num_cattle, row.names = initdata$location_id,
                             E = rep(0, times = nrow(initdata)),
                             I = rep(0, times = nrow(initdata)),
                             R = rep(0, times = nrow(initdata)))
    }
    if(modeldata$model == 'SLHV'){
      initdata <- data.frame(S = initdata$num_cattle, row.names = initdata$location_id,
                             L = rep(0, times = nrow(initdata)),
                             H = rep(0, times = nrow(initdata)),
                             V = rep(0, times = nrow(initdata)))
    }
  }

  # Add missing nodes to initdata
  temp <- sort(rownames(initdata))
  temp2 <- unique(c(eventdata$node[!(eventdata$node %in% temp)], # list of all nodes in eventdata which are not in initdata
                    eventdata$dest[!(eventdata$dest %in% temp)]))
  if(modeldata$model == 'SIR'){
    temp3 <- data.frame(S = rep(0, times = length(temp2)),
                        I = rep(0, times = length(temp2)),
                        R = rep(0, times = length(temp2)))
  }
  if(modeldata$model == 'SEIResp'){
    temp3 <- data.frame(S = rep(0, times = length(temp2)),
                        E = rep(0, times = length(temp2)),
                        I = rep(0, times = length(temp2)),
                        R = rep(0, times = length(temp2)))
  }
  if(modeldata$model == 'SLHV'){
    temp3 <- data.frame(S = rep(0, times = length(temp2)),
                        L = rep(0, times = length(temp2)),
                        H = rep(0, times = length(temp2)),
                        V = rep(0, times = length(temp2)))
  }
  rownames(temp3) <- temp2
  initdata <- rbind(initdata, temp3) # add additional nodes (if any) to initdata set
  initdata <- initdata[order(as.numeric(rownames(initdata))), ]
  initdata <- initdata[!(as.numeric(as.character(rownames(initdata))) == 0), ] # remove row for node 0 (if applicable)

  # Update modeldata object
  modeldata$init <- initdata
  modeldata$stepslog['tidy_cts_init'] <- TRUE

  cat('done\n')

  return(modeldata)
}

#********************************************************************

