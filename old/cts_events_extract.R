#********************************************************************
# Function to extract CTS movement data (events) from EPIC server

extract_cts_events <- function(uid = NULL,
                               pwd = NULL
                               #modeldata,
                               #startdate = '2012/01/01',
                               #enddate = '2012/01/07',
                               ){

  #require(RPostgreSQL)

  # Obtain EPIC username and password, if not yet specified
  #if(is.null(uid) | is.null(pwd)){
  #   uid <- readline(prompt = "EPIC user ID: ")
  # }
  # if(is.null(pwd)){
  #   pwd <- readline(prompt = "EPIC password: ")
  #   credentials <- getLoginDetails()
  #   uid <- credentials[names(credentials) == 'uid']
  #   pwd <- credentials[names(credentials) == 'pwd']
  # }
  if(is.null(uid) | is.null(pwd)){
    if(Sys.info()['sysname'] != 'Linux'){
      stop('EPIC.SimInf cannot currently extract data from the EPIC server under OSX or Windows systems. Please extract data using your preferred SQL software, and import to R.')
    }
    else{
      cat('Please supply your login details to extract data from the EPIC server.\n')
    }
    if(is.null(uid)){
      uid <- getPass('Username:')
    }
    if(is.null(pwd)){
      pwd <- getPass('Password:')
    }
  }

  cat('Extracting CTS data from EPIC server... ')
  # Set up PostgreSQL connection to EPIC server
  drv <- dbDriver("PostgreSQL")
  epicserver <- dbConnect(drv, host="epic.mvls.gla.ac.uk",
                          port="5432",
                          dbname="epic",
                          user = uid,
                          password = pwd)

  # SQL query to extract CTS events data
  rs <- dbSendQuery(epicserver, paste("
                                      SELECT DISTINCT
                                        animal_movements.movement_date,
                                        animal_movements.off_location_id,
                                        animal_movements.on_location_id,
                                        COUNT(animal_movements.animal_id) AS animals

                                      FROM (
                                        SELECT
                                          movements.animal_id,
                                          movements.movement_number,
                                          movements.movement_date,
                                          movements.off_location_id,
                                          movements.on_location_id,
                                          movements.is_birth,
                                          movements.is_death
                                        FROM
                                          cts201404.movements
                                        WHERE
                                          movements.movement_date >= '", modeldata$startdate, "' -- start date
                                          AND movements.movement_date <= '", modeldata$enddate, "' -- end date
                                          AND movements.is_trans = 'f' -- to avoid duplication in movement records
                                          AND movements.is_valid_history = 't' -- include only valid movement records (check this with Michael?)
                                        ) AS animal_movements

                                      GROUP BY
                                        animal_movements.movement_date,
                                        animal_movements.off_location_id,
                                        animal_movements.on_location_id

                                      ORDER BY
                                        animal_movements.movement_date", sep = ''))

  # Run SQL query on EPIC server
  eventdata <- dbFetch(rs, n = Inf)
  cat('done\n')

  # Close connection to EPIC server
  dbDisconnect(epicserver)

  return(eventdata)
}

#********************************************************************
# Function to tidy CTS movement data (events)

tidy_cts_events <- function(eventdata = NULL,
                            events_file = NULL,
                            outfile = NULL){

  # Tidy extracted CTS events data
  cat('Tidying events data... ')

  # Create outfile name if missing
  if(is.null(outfile)){
    outfile <- paste('siminf_', modeldata$model, '_events_',
                     str_sub(modeldata$startdate, start = 1, end = 4), '-',
                     str_sub(modeldata$startdate, start = 6, end = 7), '-',
                     str_sub(modeldata$startdate, start = 9, end = 10), '_',
                     str_sub(modeldata$enddate, start = 1, end = 4), '-',
                     str_sub(modeldata$enddate, start = 6, end = 7), '-',
                     str_sub(modeldata$enddate, start = 9, end = 10), '.csv',
                     sep = '')
  }

  # Print error message if neither events data nor valid file specified
  if(is.null(eventdata) | is.null(events_file)){
    stop('An events data frame or file should be specified.')
  }

  # Remove last (row count) row, if applicable
  if(is.na(eventdata$animals[nrow(eventdata)])){
    eventdata <- eventdata[-c(nrow(eventdata)), ]
  }

  # Format time column
  startdate <- as.numeric(min(as.Date(eventdata$movement_date))) - 1
  eventdata$time <- as.numeric(as.Date(eventdata$movement_date)) - startdate

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

  # Save tidied CTS events data to file on local system
  write.csv(eventdata, file = outfile, row.names = F, quote = F)

  cat('done\n') # done tidying events

  return(eventdata)
}

#********************************************************************

