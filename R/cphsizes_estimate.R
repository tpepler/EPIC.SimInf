#********************************************************************
# Function to extract CTS data to calculate CPH sizes at start of simulation

cphsizes_estimate <- function(uid = NULL,
                              pwd = NULL,
                              modeldata,
                              #startdate = '2012/01/01',
                              outfile = NULL){

  #require(RPostgreSQL)

  # Obtain EPIC username and password, if not yet specified
  if(is.null(uid)){
    uid <- readline(prompt = "EPIC user ID: ")
  }
  if(is.null(pwd)){
    pwd <- readline(prompt = "EPIC password: ")
  }

  # Create outfile name if missing
  if(is.null(outfile)){
    outfile <- paste('siminf_', modeldata$model, '_cphsizes_',
                     str_sub(modeldata$startdate, start = 1, end = 4), '-',
                     str_sub(modeldata$startdate, start = 6, end = 7), '-',
                     str_sub(modeldata$startdate, start = 9, end = 10), '.csv',
                     sep = '')
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
  cphdata <- dbFetch(rs, n = Inf)
  cat('done\n')

  # Close connection to EPIC server
  dbDisconnect(epicserver)

  # Tidy extracted CPH size data
  cat('Tidying CPH size data... ')
  ind <- !is.na(cphdata$location_id)
  cphsizes <- cphdata[ind, ] # remove rows without a location ID
  cat('done\n')

  # Save tidied CPH size data to file on local system
  write.csv(cphsizes, file = outfile, row.names = F, quote = F)

  return(cphsizes)
}

#********************************************************************

