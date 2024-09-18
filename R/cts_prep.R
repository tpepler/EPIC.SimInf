#********************************************************************
# Function to extract and prepare CTS data for SimInf modelling

cts_prep <- function(uid = NULL,
                     pwd = NULL,
                     startdate,
                     enddate,
                     model = c('SIR'),
                     cphsizes_file = NULL,
                     initdata_file = NULL,
                     events_file = NULL){

  # Temporary error message
  if(model != 'SIR'){
    stop('Error: Only SIR model currently implemented.')
  }

  # Obtain EPIC username and password, if necessary to extract data from the EPIC server
  #if(is.null(initdata_file) | is.null(events_file)){
    # if(is.null(uid)){
    #   uid <- readline(prompt = "EPIC user ID: ")
    # }
    # if(is.null(pwd)){
    #   pwd <- readline(prompt = "EPIC password: ")
    # }
  #   credentials <- getLoginDetails()
  #   uid <- credentials[names(credentials) == 'uid']
  #   pwd <- credentials[names(credentials) == 'pwd']
  # }
  if(is.null(uid) | is.null(pwd)){
    cat('Please supply your login details to extract data from the EPIC server.\n')
  }
  if(is.null(uid)){
    uid <- getPass('Username:')
  }
  if(is.null(pwd)){
    pwd <- getPass('Password:')
  }

  # Initialise model object
  modeldata <- vector('list', 5)
  names(modeldata) <- c('initdata', 'eventdata', 'model', 'startdate', 'enddate')
  modeldata$model <- model[1]
  modeldata$startdate <- startdate
  modeldata$enddate <- enddate

  # Obtain initialisation data (if already available on file)
  if(!is.null(initdata_file)){
    modeldata$initdata <- read.csv(initdata_file, row.names = 1)
  }
  else{
    # Obtain CPH size data
    if(is.null(cphsizes_file)){ # extract CTS data from EPIC server and estimate CPH size (if no cphsizes file supplied)
      cphsizes <- cphsizes_estimate(uid = uid, pwd = pwd, modeldata = modeldata)
    }
    else{ # read in CPH size data from specified file
      cphsizes <- read.csv(cphsizes_file)
    }
  }

  # Obtain CTS events data
  if(is.null(events_file)){ # extract CTS movement data from EPIC server (if no events file supplied)
    modeldata$eventdata <- cts_events_extract(uid = uid, pwd = pwd, modeldata = modeldata)
  }
  else{ # read in events data from specified file
    modeldata$eventdata <- read.csv(events_file)
  }

  if(is.null(initdata_file)){
    # Format initialisation (system state at start of simulation) data for specified model
    modeldata <- .init_format(modeldata = modeldata, cphsizes = cphsizes)
  }

  # Prepare events data for modelling, based on initdata
  modeldata <- .events_prep(modeldata = modeldata)

  if(is.null(initdata_file)){
    # Adjusting initialisation data for specified event data
    modeldata <- .init_adjust(modeldata = modeldata)
  }

  cat('Finished!\n')
  return(modeldata)
}

#********************************************************************

