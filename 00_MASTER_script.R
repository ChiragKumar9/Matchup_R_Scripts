# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# --- MATCHUPS master script
# --- This script will call other scripts that will perform specific functions.
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#

Sys.setenv(TZ = "UTC") # Define local time to be UTC
options(warn = -1)
options(verbose = TRUE)
options(dplyr.print_max = 1000)

# *****************************************************************************#
# *****************************************************************************#
# *** PREPARATORY STEPS
# *****************************************************************************#
# *****************************************************************************#

# -----------------------------------------------------------------------------#
# --- Install required R packages ----

if (!require(Cairo)) {install.packages("Cairo"); library(Cairo)}
if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(rgeos)) {install.packages("rgeos"); library(rgeos)}
if (!require(maptools)) {install.packages("maptools"); library(maptools)}
if (!require(magrittr)) {install.packages("magrittr"); library(magrittr)}
if (!require(robust)) {install.packages("robust"); library(robust)}
if (!require(rpart)) {install.packages("rpart"); library(rpart)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(plyr)) {install.packages("plyr"); library(plyr)}
if (!require(sp)) {install.packages("sp"); library(sp)}
if (!require(timeDate)) {install.packages("timeDate"); library(timeDate)}
# if (!require(mgcv)) {install.packages("mgcv"); library(mgcv)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(futile.logger)) {install.packages("futile.logger"); library(futile.logger)}
if (!require(readr)) {install.packages("readr"); library(readr)}
if (!require(rgdal)) {install.packages("rgdal"); library(rgdal)}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Read YAML configuration file ----

# --- The name of the configuration file can be passed via command line
# --- (using Rscript).
# --- NOTE: If the argument is not passed, the name of the configuration file
# --- is built using HARDCODED default script directory (different for Windows and Linux)
# --- and a file name (also different for Linux and Windows)

args <- commandArgs(trailingOnly = TRUE) # Arguments passed via command line

if (is.na(args[1])) {
  if (Sys.info()["sysname"] == 'Windows') {
    # Directory for scripts and configuration file name in Windows ** CHANGE as needed
    script.dir <- 'D:/matchups/r-projects/R_scripts/R_master_scripts/read_matchups/'

    config.file.name <- 'config_file_read_VIIRS_L2GEN_matchups_Windows.yml'
    config.file <- paste0(script.dir, config.file.name)
  } else if (Sys.info()["sysname"] == 'Linux') {
    # Directory for scripts and configuration file name in Linux ** CHANGE as needed
    script.dir <- '/home/ckk/Projects/MODIS/Matchups/MODIS_R_Scripts/Read_Matchups/'

    #config.file.name <- 'config_file_read_VIIRS_L2GEN_matchups_Linux.yml'
    
    config.file.name <- 'config_file_read_MODIS_GSFC_matchups_Linux.yml'

    config.file <- paste0(script.dir, config.file.name)
  } else {
    stop('Operating system not recognized...')
  }
} else {
  config.file <- args[1]
} # End of if that checks if arguments are passed via command line

# --- Check that specified configuration fle exists

if (!file.exists(config.file)) {
  stop("Specified configuration file does not exist...")
} else {
  cat(paste0("Reading configuration file ", config.file, "...\n"))
}

# --- Read YAML configuration file

config <- yaml::yaml.load_file(config.file)

rm(args, script.dir, config.file.name, config.file)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Change to a working directory and clean it up ----

setwd(config$working_dir)

# --- Remove all objects EXCEPT 'config'

rm(list=setdiff(ls(), 'config'))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Source common functions ----

source(file = paste0(config$functions_dir, 'common_functions.R'),
  local = FALSE, echo = FALSE, verbose = FALSE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Define logging level and location of log file ----

Log.threshold(config$log_level) 	# Desired logging level

# --- Check if log directory exists, else create it

if (! dir.exists(config$log_dir)) {
  dir.create(config$log_dir, showWarnings=TRUE)
}

# --- Define log file name

log.file <- paste0(config$log_dir, config$log_file)

if (file.exists(log.file) & config$log_file_erase == TRUE) {
  Log.appender(log.file)
  file.remove(log.file)
  Log.info('Starting from an empty log file')
} else {
  Log.appender(log.file)
}

Log.info(paste('Messages being logged in file', log.file))

rm(log.file)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Build names of directories where results will be stored ----
# --- All directories are under the 'results' directory specified in
# --- the configuration file.

if (!dir.exists(config$results_dir)) {
  dir.create(config$results_dir, showWarnings=TRUE)
  Log.info('Creating results directory')
}

# --- Directory where algorithm coefficients will be placed
coeff.outdir <- paste(config$results_dir, "coeffs/", sep="")

if (!dir.exists(coeff.outdir)) {
  dir.create(coeff.outdir, showWarnings=TRUE)
  Log.info('Creating directory to store algorithm coefficients')
}
# --- Directory where output graphics will be placed
fig.outdir <- paste(config$results_dir, "figs/", sep="")

if (!dir.exists(fig.outdir)) {
  dir.create(fig.outdir, showWarnings=TRUE)
  Log.info('Creating directory to store figures')
}

# --- Directory where processed data.frame will be placed
objects.outdir <- paste(config$results_dir, "objects/", sep="")

if (!dir.exists(objects.outdir)) {
  dir.create(objects.outdir, showWarnings=TRUE)
  Log.info('Creating directory to store output objects')
}

# --- Directory where quality hypercubes will be placed
hypercube.outdir <- paste(config$results_dir, "hypercubes/", sep="")

if (!dir.exists(hypercube.outdir)) {
  dir.create(hypercube.outdir, showWarnings=TRUE)
  Log.info('Creating directory to store hypercube with statistics')
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Build names of objects and files where matchups will be saved ----

# --- Build names of object where matchups will be saved

matchup.object <- paste(config$sensor,
  config$platform,
  config$matchups$format,
  config$insitu_source,
  config$matchups$level1.version,
  config$matchups$algo.coeffs.version,
  config$matchups$type.1.ref.field,
  str_replace_all(as.character(Sys.Date()),"-","_"),
  sep = '_')

# --- Build names of files where matchups will be saved.
# --- Both binary and ASCII versions will be saved using function save().

if (config$use_ancillary_data == "FALSE") {
  matchup.bin.file <-
    paste(objects.outdir, matchup.object, '.Rdata', sep = '')
  matchup.txt.file <-
    paste(objects.outdir, matchup.object, '.gzip', sep = '')
} else {
  matchup.bin.file <-
    paste(objects.outdir, matchup.object, '_with_ancillary.Rdata', sep = '')
  matchup.txt.file <-
    paste(objects.outdir, matchup.object, '_with_ancillary.gzip', sep = '')
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Build a vector with names of fields (columns) in matchups ----

matchup.vars <- get_matchup_variables(matchup.format = config$matchups$format,
  sensor = config$sensor, ancillary.data = config$use_ancillary_data)

lhdr <- length(matchup.vars)		# How many fields in matchup records? Should be 164...

if (config$matchups$format == 'MIA_L2GEN') {
  n.of.vars <- 168
} else if (config$matchups$format == 'GSFC') {
  n.of.vars <- 127
} else {
  Log.error('Matchup format must be \'MIA_L2GEN\' or \'GSFC\'')
}

if (lhdr != n.of.vars) { 	# Check length of header vector
  Log.error('Error in the length of vector of variable names')
  #stop('Check number of variables in header')
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Decide which variables in the matchups will be kept ----
# --- First, the configuration file specifies if all variables (columns)
# --- in a matchup file of a certain format will be kept or not.
# --- However, some variables in the matchups are not appropriate for a sensor
# --- and therefore are filled with missing values (e.g., '-999.0')
# --- This step defines WHICH variables will be kept for a given sensor.


if (config$collocation) {
  vars.to.keep <- keep_matchup_variables_collocation(matchup.format = config$matchups$format,
    sensor = config$sensor, ancillary.data = config$use_ancillary_data)
  Log.info(paste(length(vars.to.keep), 'variables will be kept for collocation studies using',
    config$sensor, 'matchups in', config$matchups$format, 'format'))
} else if (!config$collocation) {
  if (config$keep_all_vars) {
    # Keep ALL variables in matchups
    # (including those that are not relevant for sensor, e.g., for testing purposes)
    vars.to.keep <- matchup.vars
    Log.info(paste('Keep ALL variables in matchup records for',
      config$sensor, 'matchups in', config$matchups$format, 'format'))
  } else {
    # Keep only the variables that correspond to a given sensor variables
    vars.to.keep <- keep_matchup_variables(matchup.format = config$matchups$format,
      sensor = config$sensor, ancillary.data = config$use_ancillary_data)
    Log.info(paste(length(vars.to.keep), 'variables will be kept for',
      config$sensor, 'matchups in', config$matchups$format, 'format'))
  }
}

# TO DO: Fill in for All formats and sensors

# --- XXX variables are currently kept for VIIRS and format 'MIA_L2GEN'.
# --- XXX variables are currently kept for MODIS and format 'MIA_L2GEN'.
# --- XXX variables are currently kept for AVHRR and format 'MIA_L2GEN'.
# --- XXX variables are currently kept for VIIRS and format 'GSFC'.
# --- XXX variables are currently kept for MODIS and format 'GSFC'.
# --- XXX variables are currently kept for AVHRR and format 'GSFC'.
# ------------------------------------------------------------------------------

# *****************************************************************************#
# *** END OF PREPARATORY STEPS
Log.info('END of preparatory steps')
# *****************************************************************************#


# *****************************************************************************#
# *****************************************************************************#
# *** READ ALL MATCHUPS
# *****************************************************************************#
# *****************************************************************************#

# -----------------------------------------------------------------------------#
# --- STEP 1: Read daily matchup files for sensor analyzed ----

ptm <- proc.time()
Log.info('BEGIN script to read ALL daily matchup files')

source(paste0(config$scripts_dir,'01_read_matchups.R'),
  echo = TRUE)

step.1.time <- proc.time() - ptm

Log.info(paste('Step 1 (reading matchups) involved',
               round(step.1.time[3], 1),'seconds'))
Log.info('END script to read ALL daily matchup files')

rm(ptm); gc()
# ------------------------------------------------------------------------------








# ---------------------------------------------------------------------------------------#
# STEPS TO BE IMPLEMENTED:
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- STEP 2:
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- STEP 3:
# ----------------------------------------------------------------------------------------










