#********************************************************************
# Function to extract CTS data to calculate CPH sizes at start of simulation

#cphsizes_estimate <- function(uid = NULL,
extract_cts_init <- function(uid = NULL,
                              pwd = NULL,
                              modeldata
                              #startdate = '2012/01/01',
                              ){

  #require(RPostgreSQL)

  # Obtain EPIC username and password, if not yet specified
  #if(is.null(uid) | is.null(pwd)){
  #   uid <- readline(prompt = "EPIC user ID: ")
  # }
  # if(is.null(pwd)){
  #   pwd <- readline(prompt = "EPIC password: ")
    # credentials <- getLoginDetails()
    # uid <- credentials[names(credentials) == 'uid']
    # pwd <- credentials[names(credentials) == 'pwd']
  #}
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

  cat('Extracting CPH size data from EPIC server (this may take a while)... ')

  # Set up PostgreSQL connection to EPIC server
  drv <- dbDriver("PostgreSQL")
  epicserver <- dbConnect(drv, host="epic.mvls.gla.ac.uk",
                          port="5432",
                          dbname="epic",
                          user = uid,
                          password = pwd)

  # SQL query to extract CPH sizes data
  rs <- dbSendQuery(epicserver, paste("
                                      SELECT
                                        cattle_per_cph.location_id AS location_id,
                                        cattle_per_cph.cph AS cphh,
                                        COUNT(cattle_per_cph.animal_id) AS num_cattle
                                      FROM
                                        (-- Table of cattle IDs per CPH on the specified date
                                        SELECT DISTINCT
                                          cph_numbers.location_id AS location_id,
                                          cph,
                                          animal_id
                                        FROM
                                          --****************************************************************
                                          (-- Table of cattle IDs, and on/off moves per location ID
                                          SELECT DISTINCT
                                            move_on.on_location_id AS location_id,
                                            move_on.animal_id,
                                            move_on.movement_date AS on_date,
                                            move_off.movement_date AS off_date
                                          FROM
                                            --****************************************************************
                                            (-- Table of animals moved ONTO the location BEFORE the date of interest
                                            SELECT DISTINCT
                                              on_location_id,
                                              animal_id,
                                              movement_date
                                            FROM cts201404.movements
                                            WHERE (movement_date < '", modeldata$startdate, "')
                                              AND is_valid_history = 't'
                                              AND is_trans = 'f'
                                            ) AS move_on
                                            --****************************************************************

                                            LEFT JOIN

                                            --****************************************************************
                                            (-- Table of animals moved OFF OF the location AFTER the date of interest
                                            SELECT DISTINCT
                                              off_location_id,
                                              animal_id,
                                              movement_date
                                            FROM cts201404.movements
                                            WHERE is_valid_history = 't'
                                              AND is_trans = 'f'
                                            ) AS move_off
                                            --****************************************************************

                                            ON (move_on.animal_id = move_off.animal_id)
                                            AND (move_on.on_location_id = move_off.off_location_id)

                                          -- Select only the cattle for which the off movement is after the date of interest, or non-existent (i.e. the animal is presumably still on the farm)
                                          WHERE (move_off.movement_date >= '", modeldata$startdate, "')
                                          OR (move_off.movement_date IS NULL)
                                          ) AS cattle_numbers
                                          --****************************************************************

                                          LEFT JOIN

                                          --****************************************************************
                                          (-- Table of CPH numbers and associated location IDs
                                          SELECT DISTINCT
                                            location_id,
                                            cph
                                          FROM cts201404.locations
                                          ) AS cph_numbers
                                          --****************************************************************

                                          ON (cattle_numbers.location_id = cph_numbers.location_id)
                                        ) AS cattle_per_cph
                                      GROUP BY
                                        cattle_per_cph.location_id,
                                        cattle_per_cph.cph", sep = ''))

  # Run SQL query on EPIC server
  modeldata$init <- dbFetch(rs, n = Inf)
  cat('done\n')

  # Close connection to EPIC server
  dbDisconnect(epicserver)

  return(modeldata)
}

#********************************************************************
