#********************************************************************
# Function to format initialisation data for specified model

.init_format <- function(modeldata,
                        cphsizes){
  
  if(modeldata$model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }
  else{
    cat('Formatting initialisation data... ')
    modeldata$initdata <- data.frame(S = cphsizes$num_cattle, row.names = cphsizes$location_id,
                                     I = rep(0, times = nrow(cphsizes)),
                                     R = rep(0, times = nrow(cphsizes)))
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

