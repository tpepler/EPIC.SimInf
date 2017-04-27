#********************************************************************
# Function to format initialisation data for specified model

tidy_cts_init <- function(#modeldata,
                        #cphsizes,
                        initdata = NULL,
                        init_file = NULL,
                        outfile = NULL){

  # Print error message if neither init data nor valid file specified
  if(is.null(initdata) | is.null(init_file)){
    stop('An initialisation data frame or file should be specified.')
  }
  else{
    cat('Tidying CPH initialisation data... ')
  }

  # Create outfile name if missing
  if(is.null(outfile)){
    outfile <- paste('siminf_', modeldata$model, '_cphsizes_',
                     str_sub(modeldata$startdate, start = 1, end = 4), '-',
                     str_sub(modeldata$startdate, start = 6, end = 7), '-',
                     str_sub(modeldata$startdate, start = 9, end = 10), '.csv',
                     sep = '')
  }

  # Remove rows without a location ID
  ind <- !is.na(initdata$location_id)
  initdata <- initdata[ind, ]
  cat('done\n')

  if(modeldata$model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }
  else{
    initdata <- data.frame(S = initdata$num_cattle, row.names = initdata$location_id,
                                     I = rep(0, times = nrow(initdata)),
                                     R = rep(0, times = nrow(initdata)))
  }

  # Add missing nodes to initdata
  temp <- sort(rownames(modeldata$initdata))
  temp2 <- unique(c(modeldata$eventdata$node[!(modeldata$eventdata$node %in% temp)],
                    modeldata$eventdata$dest[!(modeldata$eventdata$dest %in% temp)]))
  temp3 <- data.frame(S = rep(0, times = length(temp2)),
                      I = rep(0, times = length(temp2)),
                      R = rep(0, times = length(temp2)))
  rownames(temp3) <- temp2
  modeldata$initdata <- rbind(modeldata$initdata, temp3)
  modeldata$initdata <- modeldata$initdata[order(as.numeric(rownames(modeldata$initdata))), ]
  modeldata$initdata <- modeldata$initdata[-c(which(as.numeric(rownames(modeldata$initdata)) == 0)), ] # remove row for node 0

  cat('done\n')

  return(modeldata)
}

#********************************************************************

