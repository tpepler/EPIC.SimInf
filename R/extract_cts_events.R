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
