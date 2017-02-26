
# *****************************************************************************#
# *****************************************************************************#
# *** Script to read ALL matchup files for a given sensor
# *****************************************************************************#
# *****************************************************************************#

# --- This script reads in all daily text matchup files for a given sensor.

# --- The sensor to work on is specified in a configuration  file.

# --- These files are probably stored in separate directories for different years).
# --- This script will:
# --- (a) perform simple checks on daily files (eg, identify zero-length files),
# --- (b) read in all non-empty daily matchup files and create an R data frame
# ---     object with the desired matchup variables,
# --- (c) create and add a few additional variables that will facilitate
# ---     subsequent analyses, and
# --- (d) Save the data frame with all processed matchups into
# ---     a binary (.Rdata) R object.

# -----------------------------------------------------------------------------#
# --- Build list of daily files available to be read ----

# --- First we build a "crude" file list.
# --- It includes all file names that end with '.matchup'.

crude.file.list <- list.files(path = list.dirs(config$matchups_base_indir,
  recursive = FALSE),
  pattern = '*.matchup',
  all.files = FALSE,
  full.names = TRUE)

Log.debug(paste('First filtering produces', length(crude.file.list),'files'))

# --- Now we define a bogus variable that will reflect
# --- the results of applying ALL file selection patterns to the
# --- crude list of files.
# --- If a file name matches AT LEAST one of the patterns in the vector,
# --- then the corresponding element of uu1 is set to TRUE.

uu1 <- rep(FALSE, times = length(crude.file.list))

uu.temp <- grepl(pattern = config$file_selection_strings,
  crude.file.list,
  ignore.case = TRUE, fixed = FALSE, perl = TRUE)

uu1 <- uu1 | uu.temp

for (pattern in config$file_selection_strings) {
    uu.temp <- grepl(pattern = pattern,
    crude.file.list,
    ignore.case = TRUE, fixed = FALSE, perl = TRUE)
  uu1 <- uu1 | uu.temp
}

# --- List files that match at least one of the patterns
# --- specified in configuration pattern.

daily.file.list <- crude.file.list[uu1]

Log.debug(paste('Filename pattern selection yields',
  length(daily.file.list),'files'))

# --- Filter files that contain the desired in situ source
# --- (such as 'SNB', IQUAM', 'MAERI', 'ISAR', etc.).
# --- This is specified in config variable config$insitu_source
# --- If 'ALL' is specified, no particular in situ source is selected.

uu0 <- stringr::str_to_lower(config$insitu_source) # Convert to lower case

Log.info(paste('Matchups in situ source(s) being selected:',
  paste(uu0, collapse = ' - ')))

uub <- rep(NA, times = length(daily.file.list)) #store in situ source here

if (any(uu0 == 'all')) {
  Log.info('ALL matchup in situ sources being selected')
  uub <- rep('all', times = length(daily.file.list))
} else {
  Log.info('Only SOME matchup in situ sources being selected')
  uu1 <- rep(FALSE, times = length(daily.file.list))

  for (insitu.source in uu0) {
    uu.temp <- grepl(pattern = insitu.source,
      daily.file.list,
      ignore.case = TRUE, fixed = FALSE, perl = TRUE)
    uub[uu.temp] <- insitu.source
    uu1 <- uu1 | uu.temp
  } # End of looping through various possible in situ sources

  # --- Select files that match at least one of the desired
  # --- in situ sources (specified in configuration pattern).

  daily.file.list <- daily.file.list[uu1]
}

Log.debug(paste('In situ source selection yields',
  length(daily.file.list),'files'))

# --- Count number of daily files to be read

n.daily.files <- length(daily.file.list) # Number of daily files

if (n.daily.files == 0) {							# Is directory empty?
  Log.error(paste('Daily matchup directory',
    config$matchups_tmp_indir,
    'seems to be empty'))
} else {

  Log.info(paste('There are', n.daily.files,
    'daily matchup files to be read for sensor', config$sensor,
    'and in situ source(s):',
    paste(uu0, collapse = ' - ')))

  # --- Get size of daily matchup files to be read

  tt2 <- file.info(daily.file.list)   # Get information about daily files to be read
  tt3 <- tt2$size                     # Size of each file in bytes

  # --- Assemble data frame with names and sizes
  # --- of daily matchup files to be read.

  daily.file.info <- data.frame(name = daily.file.list, size = tt3,
    insitu.source = uub[uu1])
}

rm(uu0,uu1,uub,uu.temp,tt1,tt2,tt3,use.file); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Save the list of daily files to be read in an R binary file ----

# --- Save the list of daily matchup files in temporary directory.
# --- The list will be saved to a binary object (extension .Rdata).
# --- The object will be stored in 'objects.outdir' directory.
# --- Note that only the file names (ie, not directories) will be stored.

tt1 <- as.Date(Sys.time())

if (config$use_ancillary_data == "FALSE") {
  # DO NOT use ancillary variables (eg, microwave SSTs)
  files.read <- paste(objects.outdir, matchup.object,
    '_file_list_','.Rdata', sep = '')
} else {
  # Use ancillary variables (eg, microwave SSTs)
  files.read <- paste(objects.outdir, matchup.object,
    '_file_list_ancillary','.Rdata', sep = '')
}

save(daily.file.info,
  file = files.read,
  ascii = FALSE, precheck = TRUE)

Log.info(paste('List of daily', config$sensor,
  'matchup files available on', tt1, 'was saved'))

rm(tt1, files.read); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Check completeness of dates for which daily matchup files are available ----

Log.info('BEGIN initial checks of daily matchup files')

tt1 <- basename(as.character(daily.file.list))  # Eliminate directory name

if (config$matchups$format == 'MIA_L2GEN') {
  # We assume that, in the MIA_L2GEN format, the date of the matchup data
  # is encoded as the first 7 characters of the file name (YYYYDDD).
  # If this is NOT the case, change accordingly.
  tt2 <- stringr::str_sub(tt1, start = 1L, end = 7L)
} else if (config$matchups$format == 'GSFC') {
  # We assume that, in the GSFC format, the date of the matchup data
  # is encoded as characters 2-8 of the file name (e.g., AYYYYDDD).
  # If this is NOT the case, change accordingly.
  tt2 <- stringr::str_sub(tt1, start = 2L, end = 8L)
}

# --- Convert part of file names into dates.

file.dates <- lubridate::parse_date_time(tt2, "%Y%j") # dates derived from file name

# --- To identify missing daily matchup files, generate
# --- a complete list of days (from lowest to highest date in dorectory)
# --- and compare it with the list of available dates.

# --- NOTE: If files for dates at the beginning or end of the series
# --- are missing, this will not be detected.

# --- Build name of objects to store missing dates
# --- for each in situ data source.

jj0 <- paste(stringr::str_to_lower(config$insitu_source), collapse = '-')
jj1 <- paste0('complete.dates.', jj0) # Object with complete dates
jj2 <- paste0('missing.dates.', jj0)  # Object with missing dates

# create an object name based on the insitu type(s) in the config.
# This is done by using the assign function to convert the text
# to an object name

assign(jj1, seq(from = min(file.dates), to = max(file.dates), by = 'days'))

tt3 <- get(jj1) %in% file.dates
assign(jj2, get(jj1)[!tt3]) # which dates are missing

if (length(get(jj2)) > 0) {
  Log.warn(paste('There are', length(get(jj2)), 'days without',
    config$insitu_source, 'matchups'))
  Log.warn(paste('missing dates', str_join(get(jj2),collapse=",")))
} else {
  Log.info(paste('No dates seem to be missing for available',
    config$insitu_source, 'matchup files'))
}

#rm(list = ls(pattern = '^complete.dates.'))
#rm(list = ls(pattern = '^missing.dates.'))
rm(jj0,jj1,jj2,tt1,tt2,tt3,file.dates); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Check for possible problems in contents of input daily matchup files ----

# --- First, check for completely empty daily matchup files
# --- that can make the processing fail.
# --- Find the size of all input files and exclude those with size == 0.

tt1 <- daily.file.info$size   # Get size of all daily files in list

if (all(tt1 != 0)) {
  # No empty matchup files
  Log.info('No daily matchup files are EMPTY')

} else if (all(tt1 == 0)) {
  # ALL empty files
  Log.error('ALL daily matchup files are EMPTY')

} else if (any(tt1 == 0)) {
  # SOME empty files
  Log.warn(paste('SOME daily matchup files in directory',
    config$matchups_tmp_indir,
    'are EMPTY (i.e., have length zero)'))

  tt2 <- which(tt1 == 0)				# Which input files have size = 0?
  empty.files <- as.character(daily.file.list[tt2]) # Empty files (size 0)
  Log.warn(paste('There are', length(tt2),'empty input matchup file(s)'))
  Log.warn('They are being eliminated from list of files to be processed')
  Log.info('See list of empty files in object \'empty.files\'')
  daily.file.list <- daily.file.list[-tt2]	# Exclude any empty input files
  n.daily.files <- length(daily.file.list)		  # Final number of non-empty input matchup files
  Log.warn(paste('There are now', n.daily.files,
    'daily files left to be processed'))
  rm(tt2)
}	# End of checking for any non-empty files

rm(tt1)

# --- Second, check that all files have a header.
# The number of header lines must be the same for all files being read
# code assumes if there are multiple header records the last line
# contains the variable names
#
# --- Skip any files that do not have a header.

# TODO: make it smarter so that if can handle different numbers of header lines
# for an in insitu type

header.line <- config$header_record_number # Number of lines for header (from config file
header.problems <- rep(FALSE, length(daily.file.list)) # Set all to FALSE

for (i in 1:n.daily.files) {
  # i <- 1
  tt1 <- scan(daily.file.list[i], what = "buoy.date",
              skip = (header.line - 1), nlines = 1, quiet = TRUE)

  # There is a problem if first record DOES NOT contain string 'buoy.date'
  header.problems[i] <- !(stringr::str_detect(tt1[1], 'buoy.date'))

  if (header.problems[i] == TRUE) {
    Log.warn(paste('Header missing or incorrect in file', daily.file.list[i]))
  } else {
    lines.skip <- header.line - 1 # Why is this done???
  }
}

Log.info(paste('There are', length(header.problems[isTRUE(header.problems)]),
  'files with header problems'))

if (any(header.problems)) {
  Log.warn('Files with header problems are eliminated from list of files to be processed')
  # Select files WITHOUT header issues
  daily.file.list <- daily.file.list[header.problems != TRUE]
  n.daily.files <- length(daily.file.list) # Final number of non-empty input matchup files
  Log.info(paste('There are now', n.daily.files,
    'daily files left to be processed'))
} # End of checking for header problems

# --- Third, find files with records having a number of columns
# --- different from what should be in the matchup.
# --- For each file, count number of columns in each record,
# --- then check that all records in a file have 'lhdr' columns.
# --- For VIIRS and MODIS, lhdr = 164.

# --- Files should have THIS many columns for the sensor being analyzed
# the get_matchup_variable function goes and gets the variable names that
# a sensor should have in the header record. We compare what is in the header names
# in the file to what should be there for that sensor

lhdr <- length(get_matchup_variables(matchup.format = config$matchups$format,
  sensor = config$sensor, ancillary.data = config$use_ancillary_data)) # No. of columns

Log.debug('Counting number of fields in each daily file')

tt3 <- sapply(daily.file.list, FUN = readr::count_fields,
  skip = (config$header_record_number - 1),
  tokenizer = tokenizer_csv())

tt4 <- lapply(tt3, function(x){any(x) != lhdr})
tt5 <- unlist(tt4)
names(tt5) <- NULL

if (any(!tt5)) {
  # Are there any files that have different numbers of columns?
  wrong.files <- daily.file.list[!tt5]
  Log.warn(paste('There are',length(wrong.files),
    'file(s) with varying number of columns'))
  Log.info('See list in object \'wrong.files\'')

  # Eliminate files having numbers of columns not equal to 164
  Log.warn('Files with varying number of columns are being eliminated')
  daily.file.list <- daily.file.list[tt5]
  n.daily.files <- length(daily.file.list)	# Final number of non-empty input files
  Log.warn(paste('There are now', n.daily.files,
    'daily files left to be processed'))
} else {
  Log.info(paste('All', n.daily.files,
    'files seem to have the correct number of columns'))
} # End of checking for files having different numbers of columns

Log.info('END of initial checks of daily matchup files')

rm(tt1,tt2,tt3,tt4,tt5,daily.file.info,header.problems); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Read in the FIRST daily matchup file in the input directory ----
# --- Put matchups in dataframe "orig".

Log.info(paste('BEGIN to read daily', config$sensor, 'matchup files'))

# --- We use package 'readr' from Hadley Wickham because
# --- it has C++ code and it is (supposed to be) much faster than read.table.

# WARNING: We assume all files have a header with column names,
# and that the header is only ONE line.
# If this changes the read will need to be modified...

# --- Before reading, specify data type as character for columns that have
# --- mixed numerics and characters so readr does not get confused.
# --- Note, however, that some variables that need to be defined as 'character'
# --- exist for some formats and not others. An example is sat.time.

if (config$matchups$format == 'MIA_L2GEN') {
  ctypes <- readr::cols(buoy.id = 'c', buoy.date = 'c', buoy.time = 'c',
    l1blutver = 'c', source = 'c', sunside = 'c',
    sat = 'c', sat.date = 'c', sat.time = 'c')
} else if (config$matchups$format == 'GSFC') {
  ctypes <- readr::cols(buoy.id = 'c', buoy.date = 'c', buoy.time = 'c',
    l1blutver = 'c', source = 'c', sunside = 'c',
    sat = 'c')
}

# --- Define field separator for a given matchup format

if (config$matchups$format == 'MIA_L2GEN') {
  csep <- "\t"
} else if (config$matchups$format == 'GSFC') {
  csep <- ","
}

# --- Read FIRST file in list

orig <- readr::read_delim(file = daily.file.list[1],
  delim = csep,
  quote = '\"',
  escape_double = FALSE,
  na = c("-999","-999.0"),
  col_names = TRUE,
  col_types = ctypes,
  skip = (config$header_record_number - 1))

# --- Check that the name of variables read from the file header coincides
# --- with the names that correspond to the sensor being analyzed.

tt1 <- colnames(orig) # Names of variables from header in the file
tt2 <- get_matchup_variables(matchup.format = config$matchups$format,
    sensor = config$sensor, ancillary.data = config$use_ancillary_data) # Variable names for THIS sensor

if (identical(tt1, tt2)) {
  Log.debug('Variable names in header are correct')
} else {
  Log.error(paste('Check variable names in header of file', daily.file.list[1]))
}

# --- Fix column name of the 3 band algo. l2gen calls it sst4
# --- Liz fixed this in the headers but will leave just in case
# if (config$sensor == 'VIIRS') {
#   names(orig) <- str_replace(names(orig),"sst4","sst3b")
# }

# --- Check that the number of fields read coincides with
# --- the variables specified in the "header" object...

if (ncol(orig) != lhdr) {
  Log.error(paste('Problem with number of columns in file', daily.file.list[1]))
}

# --- If configuration file specifies that not all variables will be kept,
# --- filter out variables that we DO NOT want to keep.

if (config$keep_all_vars) {
  # Keep all variables
  Log.debug('Keeping ALL variables in matchups')
} else {
  # DO NOT keep all variables
  Log.debug('Keeping SOME variables in matchups (defined in \'vars.to.keep\'')
  orig <- dplyr::select(orig, one_of(vars.to.keep))
}

# --- Apply GROSS quality filters to first file read.
# --- Tests are formulated so that records are KEPT if tests are TRUE.

orig.n <- nrow(orig)  # Number of records BEFORE gross quality filters

orig <- apply_gross_quality_filters(orig,
  matchup.format = config$matchups$format, sensor = config$sensor)

orig.after <- nrow(orig)

Log.debug(paste('File 1 :', orig.n - orig.after,
  'records were eliminated by gross quality tests'))
Log.debug(paste('File 1 :', orig.after,
  'records passed gross quality tests'))

rm(tt1,tt2,orig.n, orig.after); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- If more than one daily input file, loop through other files ----
# --- Read each of the remaining files, and append the contents
# --- to the first file read.

if (n.daily.files == 1) {

  # Nothing else to do...
  Log.warn('Only one matchup file to read. We are done.')

} else if (n.daily.files > 1) {	# More than one matchup file to read

  # --- Loop through each additional matchup file and append
  # --- data to the vectors created above.

  Log.info(paste('Looping to read', n.daily.files, 'daily files'))

  if (exists("orig2")) {rm(orig2)}		# Empty "orig2" object before reading new data into it...

  for (i in 2:n.daily.files) {			# Process subsequent files
  # for (i in 27:30) {			# Process subsequent files

    cat(paste('Reading daily file', i ,'of', n.daily.files,'...\n'))
    Log.debug(paste('Reading daily file', i ,'of', n.daily.files))

    # --- Read in each annual matchup file after the first one.
    # --- Read data into "orig2" dataframe.

    # WARNING: We assume all files have a header with column names,
    # and that the header is only ONE line.
    # If this changes the read will need to be modified...

    orig2 <- readr::read_delim(file = daily.file.list[i],
      delim = csep,  #config$matchups$field.separator,
      quote = '\"',
      escape_double = FALSE,
      na = c('-999', '-999.0'),
      col_names = TRUE,
      col_types = ctypes,
      skip = (config$header_record_number - 1))

    # --- Check that the name of variables read from the file header coincides
    # --- with the names that correspond to the sensor being analyzed.

    tt1 <- colnames(orig2) # Names of variables from header in the file
    tt2 <- get_matchup_variables(matchup.format = config$matchups$format,
      sensor = config$sensor, ancillary.data = config$use_ancillary_data) # Variable names for THIS sensor

    if (identical(tt1, tt2)) {
      Log.debug('Variable of names in header are correct')
    } else {
      Log.error(paste('Check variable names in header of file', daily.file.list[i]))
    }

    # --- Fix column name of the 3 band algo. l2gen calls it sst4
    # --- Liz fixed this in the headers but will leave just in case
    #if (config$sensor == 'VIIRS') {
    #  names(orig2) <- str_replace(names(orig),"sst4","sst3b")
    #}

    # --- Check that the number of fields read coincides with
    # --- the variables specifed in the "header" object...

    if (ncol(orig2) != lhdr) {
      Log.error(paste('Problem with number of columns in file', daily.file.list[i]))
    }

    # --- If configuration file specifies that not all variables will be kept,
    # --- filter out variables that we DO NOT want to keep.
    
    if (config$keep_all_vars) {
      # Keep all variables
      Log.debug('Keeping ALL variables in matchups')
    } else {
      # DO NOT keep all variables
      Log.debug('Keeping SOME variables in matchups (defined in \'vars.to.keep\'')
      orig2 <- dplyr::select(orig2, one_of(vars.to.keep))
    }

    # --- Apply GROSS quality filters to first file read.
    # --- Tests are formulated so that records are KEPT if tests are TRUE.

    orig.n <- nrow(orig2)  # Number of records BEFORE gross quality filters

    orig2 <- apply_gross_quality_filters(orig2,
      matchup.format = config$matchups$format,sensor = config$sensor)

    orig.after <- nrow(orig2)

    Log.debug(paste('File', i,':', orig.n - orig.after,
      'records were eliminated by gross quality tests'))
    Log.debug(paste('File', i,':', orig.after,
      'records passed gross quality tests'))

    # --- Append newly read file to original file

    orig <- dplyr::bind_rows(orig, orig2)

    rm(orig2); gc()

  }	# End of looping through n of daily files minus 1
}		# End of test for more than one daily file

Log.info(paste('END of reading', config$sensor, 'daily matchup files'))

rm(tt1,tt2,i,orig.n, orig.after)
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# --- NOW THAT WE HAVE FINISHED READING ALL INDIVIDUAL MATCHUP FILES,
# --- PROCESS THE RESULTING DATA FRAME (ORIG).
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
# --- Sort final file with all matchups and eliminate duplicates -----
# --- NOTE: we sort by SST quality because if there are duplicated records we will keep
# --- the record with the highest quality (lowest value), which will be sorted first.
# --- If duplicate records have the same quality SST, THEN pick the one with the smallest
# --- satellite zenith angle.

orig2 <- dplyr::tbl_df(orig) %>%
  dplyr::arrange(buoy.pftime, buoy.lat, buoy.lon, buoy.sst,
    sat.pftime, sat.lat, sat.lon,
    qsst, satz)

# --- Check for duplicate records.
# --- These are records that have the same buoy data, lat and lon, and the same satellite time.

lines.duplicated <- dplyr::select(orig2,
  buoy.pftime, buoy.lat, buoy.lon, buoy.sst,
  sat.pftime, sat.lat, sat.lon) %>%
  duplicated(.)

which.duplicated <- which(lines.duplicated == TRUE)
which.not.duplicated <- which(lines.duplicated == FALSE)

if (any(lines.duplicated)) {
  Log.warn(paste('There ARE', length(which.duplicated),
    'duplicate lines in matchups'))
  Log.info('See indices of duplicated lines in object \'which.duplicated\'')
  # --- Write out data without duplicates to object "orig"
  Log.info(paste(length(which.duplicated),
    'duplicated lines are being eliminated'))
  orig <- orig2[which.not.duplicated, ] # Select non-duplicated records
} else {
  Log.info('No duplicate lines detected')
  orig <- orig2 # Store records in orig
}

rm(lines.duplicated, which.not.duplicated, which.duplicated, orig2); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Change a few variables into character vectors or factors ----

orig$buoy.id <- as.character(orig$buoy.id)				# Buoy ID

# --- Variables that depend on matchup format

if (config$matchups$format == 'MIA_L2GEN') {
  # Variables for MIA_L2GEN format
  orig$sat.id <- as.character(orig$sat.id)  # Satellite ID
  orig$anc.type <- as.character(orig$anc.type)  # Ancillary data type description
  orig$matchup.version <- as.character(orig$matchup.version)	# Matchup version

  # --- Assign labels to buoy quality.
  # --- This field only populated for IQUAM in situ data (for now).
  orig$buoy.qual <- factor(orig$buoy.qual,
    levels = c(0, 1, 2, 9),
    labels = c('no quality','passed','final','junk'))
    
  # --- Assign labels to in situ platform type
  orig$buoy.type <- factor(orig$buoy.type,
    levels = 0:5,
    labels = c('moored buoy', 'drifting buoy', 'ship', 'maeri',
      'isar','argos float'))
    
} else if (config$matchups$format == 'GSFC') {
  # Variables in GSFC matchups
  orig$sat <- as.character(orig$sat)        # Satellite ID
  orig$source <- as.character(orig$source)  # Source of matchups (eg, 'goddard')
  
} else {
  Log.error('Specified matchups\' format not recognized')
}
  
# --- Variables that depend on sensor
  
if (config$sensor == 'VIIRS' | config$sensor == 'MODIS') {
    
  # --- Add prefix 'd' to detector number (for VIIRS and MODIS);
  dd1 <- sort(unique(orig$detector))
  orig$detector <- factor(orig$detector,
      levels = dd1, labels = paste0('d',dd1))
  rm(dd1)
    
  # --- Add prefix 'm' to mirror side (for VIIRS and MODIS);
  orig$mirror <- factor(orig$mirror,
    levels = 1:2, labels = paste0('m', 1:2))
  
} else if (config$sensor == 'AVHRR') {
    
  orig$detector <- NULL
  orig$mirror <- NULL

} else {
  Log.error('Specified sensor not recognized')
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# --- CREATE ADDITIONAL VARIABLES
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
# --- Build a POSIXt object with buoy (in situ) observation time ----

# --- The output format selected conforms to the International Standard ISO 8601
# --- which specifies numeric representations of date and time. (YYYYMMDD HH:MM:SS)
# --- For more info see: http://www.cl.cam.ac.uk/~mgk25/iso-time.html

# --- First create a vector of dates/times with the dates/times
# --- listed in the matchups.

tt1 <- paste(substr(orig$buoy.date, 1, 4),
  substr(orig$buoy.date, 5, 6),
  substr(orig$buoy.date, 7, 8),
  sep = '-')

tt2 <- orig$buoy.time

buoy.timeDate <- lubridate::ymd_hms(paste(tt1, tt2), tz = 'UTC')

# --- Original times are expressed in elapsed seconds since 1 Jan 1981 00:00:00 UTC.
# --- The output format selected conforms to the International Standard ISO 8601
# --- which specifies numeric representations of date and time (YYYYMMDD HH:MM:SS).
# --- For more info see: http://www.cl.cam.ac.uk/~mgk25/iso-time.html

buoy.timeDate2 <- as.POSIXlt(orig$buoy.pftime,
  origin="1981-01-01 00:00:00", tz="UTC")

# Check that times computed from pftime are equal to those from dates and times

tt3 <- all.equal(buoy.timeDate, buoy.timeDate2, tolerance = 1)

if (tt3) {
  Log.info('Buoy dates/times and PF times are consistent')
} else {
  Log.warn('Buoy dates/times and PF times are NOT consistent')
}

rm(tt1, tt2, tt3, xx1, xx2, buoy.timeDate2); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Build a POSIXt object with satellite observation time ----

# --- The output format selected conforms to the International Standard ISO 8601
# --- which specifies numeric representations of date and time. (YYYYMMDD HH:MM:SS)
# --- For more info see: http://www.cl.cam.ac.uk/~mgk25/iso-time.html

if (config$matchups$format == 'MIA_L2GEN') {
  tt1 <- paste(substr(orig$sat.date, 1, 4),
    substr(orig$sat.date, 5, 6),
    substr(orig$sat.date, 7, 8),
    sep = '-')

  tt2 <- orig$sat.time

  sat.timeDate <- lubridate::ymd_hms(paste(tt1, tt2), tz = 'UTC')

  # --- Original times are expressed in elapsed seconds since 1 Jan 1981 00:00:00 UTC.
  # --- The output format selected conforms to the International Standard ISO 8601
  # --- which specifies numeric representations of date and time (YYYYMMDD HH:MM:SS).
  # --- For more info see: http://www.cl.cam.ac.uk/~mgk25/iso-time.html

  sat.timeDate2 <- as.POSIXlt(orig$sat.pftime,
    origin = "1981-01-01 00:00:00", tz = "UTC")

  # Check that times computed from pftime are equal to those from dates and times
  # some pixels on scan lines that start before and end after midnight can be assigned the wrong date
  # as the date is based on the start date of the scan not the true pixel date.
  # Correct the sat date for these cases


  tt3 <- all.equal.POSIXt(sat.timeDate, sat.timeDate2, tolerance = 1)

  if (!is.logical(tt3))  {
    gregorian.time.diff <- as.numeric(sat.timeDate2 -  sat.timeDate)
    midnight.scan <- gregorian.time.diff > 1 & substr(orig$buoy.time,0,2) == "23"
    #table(midnight.scan)
    sat.timeDate[midnight.scan] <- sat.timeDate[midnight.scan] + days(1)
    orig$sat.date[midnight.scan] <- as.character(as.POSIXct(orig$sat.date[midnight.scan],
                                                            format= "%Y%m%d") + days(1), format="%Y%m%d")
  }

  rm(tt3)
  # after correcting sat date for midnight scans recheck that there is not problem for non midnight scan dates
  tt3 <- all.equal.POSIXt(sat.timeDate, sat.timeDate2, tolerance = 1)

  if (is.logical(tt3) & tt3 == "TRUE") {
    Log.info('Satellite dates/times and PF times are consistent')
  } else {
    Log.warn('Satellite dates/times and PF times are NOT consistent')
  }
  } else if (config$matchups$format == 'GSFC') {
    sat.timeDate <- as.POSIXlt(orig$sat.pftime,
                               origin = "1981-01-01 00:00:00", tz = "UTC")
  }

rm(tt1, tt2, tt3, sat.timeDate2); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Compute other useful time-related variables ----
# --- that will be added to the large dataframe.
# --- NOTE: They are all calculated from the BUOY date/time.

moyr <- format(buoy.timeDate,"%b-%Y")   # Month and year (e.g., Jul-2005)
mon <- lubridate::month(buoy.timeDate)  # Month (1-12)
yr <- lubridate::year(buoy.timeDate)    # Year: 2012, 2013,..
doy <- lubridate::yday(buoy.timeDate)   # Day of year (0 to 365[6])
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Assign some variables to categories or intervals ----

# --- Assign latitudes to latitude bands.
# --- NOTE: If definition of latband changes, change boundaries.

lat.boundaries <- c(-90, -40, -20, 0, 20, 40, 90)   # Latitude band boundaries
lat.labels <- c("<=40S","40S+ to 20S","20S+ to Eq",
  "Eq+ to 20N", "20N+ to 40N",">40N")

latband <- ordered(cut(orig$buoy.lat,
  breaks = lat.boundaries, right=TRUE,
  labels = lat.labels))

n.latbands <- length(lat.labels)	  # No. of latitude bands
latband.names <- levels(latband)		# Labels for latband intervals

# --- Check definition of latitude bands

check <- tapply(orig$buoy.lat, INDEX=latband, FUN=range, simplify=T);

# --- Assign satellite zenith angles to satz intervals.
# --- for VIIRS this is where the pixel aggregation changes.

# --- TO DO: Decide on satz intervals for MODIS and VIIRS

satz.boundaries <- c(-70, -48, -32, 0, 32, 48, 70)
satz.labels <- c("<= -48","-48 to -32","-32 to nadir",
  "nadir to +32","+32 to +48","> +48")

satzint <- cut(orig$satz, breaks = satz.boundaries,
  include.lowest = TRUE, right=TRUE,
  labels = satz.labels)

check2 <- tapply(orig$satz, INDEX = satzint, FUN=range, simplify=T)

# -----Assign buoy SST and reference SST to intervals

bsst.boundaries <- seq(from = -2, to = 40, by = 2)

bsstint <- cut(orig$buoy.sst,
  breaks = bsst.boundaries, include.lowest = TRUE)

check3 <- tapply(orig$buoy.sst, INDEX = bsstint, FUN = range, simplify = TRUE)

if (config$matchups$format == 'MIA_L2GEN') {
  refsstint <- cut(orig$ref.type.1.SST,
    breaks = bsst.boundaries, include.lowest=T)
  check4 <- tapply(orig$ref.type.1.SST, INDEX = refsstint, FUN = range, simplify = TRUE)
} else if (config$matchups$format == 'GSFC') {
  refsstint <- cut(orig$cen.ref.type.1.SST,
    breaks = bsst.boundaries, include.lowest=T)
    check4 <- tapply(orig$cen.ref.type.1.SST, INDEX = refsstint, FUN = range, simplify = TRUE)
}

rm(check, check2, check3, check4, lat.boundaries,
  lat.labels, satz.boundaries, satz.labels, bsst.boundaries); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Add variables created above to "orig" data frame ----

orig$sat.timedate <- sat.timeDate
orig$buoy.timedate <- buoy.timeDate
orig$moyr <- moyr
orig$buoy.mon <- mon
orig$buoy.yr <- yr
orig$doy <- doy
orig$latband <-latband
orig$satzint <- satzint
orig$bsstint <- bsstint
orig$refsstint <- refsstint

rm(sat.timeDate, buoy.timeDate,
   moyr, mon, yr, doy, latband, satzint,bsstint,refsstint); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Decode IDs of in situ platform and extract source and type ----

uuu <- decode_insitu_id(orig$buoy.id)

addmargins(xtabs(~ source + platform, data = uuu))

# add row/col summary (default is sum)
round(prop.table(xtabs(~ source + platform, data = uuu)), 4)  # show counts as proportions of total

# --- Add in situ source and type to dataframe 'orig'

orig$insitu.platform <- uuu[, 'platform']
orig$insitu.source <- uuu[, 'source']

rm(uuu)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Add cell numbers for 1-degree and 5-degree grids ----

# --- These numbers may help perform statistics for the matchups
# --- (e.g., number of matchups per cell, etc.).

# --- Create a raster object with 5-degree pixels

crs.string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

grid5deg <- raster::raster(ncol = 72, nrow = 36,
  xmn = -180, xmx = 180,
  ymn = -90, ymx = 90,
  crs = crs.string)

# --- Create a raster object with 1-degree pixels

grid1deg <- raster::raster(ncol = 360, nrow = 180,
  xmn = -180, xmx =180,
  ymn = -90, ymx = 90,
  crs = crs.string)

# --- Write cell numbers as value for the grids.
# --- Cell 1 is the upper left corner and numbers go
# --- across the top line through its end, then
# --- start again in the second line, and so forth.

raster::values(grid5deg) <- 1:raster::ncell(grid5deg) # Cell numbers for 5 deg grid

raster::values(grid1deg) <- 1:raster::ncell(grid1deg) # Cell numbers for 1 deg grid

res(grid5deg)
cellStats(grid5deg, 'max')
getValues(grid5deg, row=1, nrow=2)
cells <- cellFromRowColCombine(grid5deg, 1:2, 1:2)
colFromX(grid5deg, 1)
rowFromY(grid5deg, 3)
coords.5 <- xyFromCell(grid5deg, 1:ncol(grid5deg))

# --- Build a SpatialPoints object with lons and lats of matchups

matchup.coords <- as.matrix(cbind(lon = orig[, 'sat.lon'],
  lat = orig[, 'sat.lat']))

pts <- sp::SpatialPoints(coords = matchup.coords,
  proj4string = sp::CRS(crs.string))

# --- Overlay the SpatialPoints object with matchup locations
# --- over the 5-degree and 1-degree raster grids.
# --- The results are objects that list the cell number
# --- in which a matchup is located.

tt1 <- raster::extract(x = grid5deg, y = pts, method = "simple")
tt2 <- raster::extract(x = grid1deg, y = pts, method = "simple")

orig$cell5deg <- tt1  # Store cell number in 5-degree grid in dataframe orig
orig$cell1deg <- tt2  # Store cell number in 1-degree grid in dataframe orig

rm(crs.string, matchup.coords, coords.5, grid1deg, grid5deg, tt1, tt2); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Add geographic information (eg., ocean basin) to matchup locations ----
# --- The information is from the Natural Earth web site.

# --- Check if ancillary directory exists

if (!dir.exists(config$ancillary_dir)) {
  dir.create(config$ancillary_dir, showWarnings=TRUE)
  Log.info('Creating directory to store ancillary data')
}

# --- Define name of file with geographic information

file <- "ne_10m_geography_marine_polys.shp"  	# Name of file with geographic info
infile <- paste(config$ancillary_dir, file, sep="") # Build complete (long) input file name

if (file.exists(infile)) {
  # File with geographic info already exists in ancillaray data directory
  Log.info('File with geographic info exists. No need to download it')
} else {
  # Files with geographic info do not exist: download from Natural Earth website
  Log.info('Files with geographic info  do not exist: downloading it')
  zip.file <- paste0(config$ancillary_dir,'test.zip')
  NE.url <- 'http://naciscdn.org/naturalearth/10m/physical/ne_10m_geography_marine_polys.zip'
  download.file(NE.url, destfile = zip.file, quiet = FALSE)
  utils::unzip(zipfile = zip.file,
    exdir = stringr::str_sub(config$ancillary_dir, start = 1L, end = -2L),
    junkpaths = TRUE,
    overwrite = TRUE)
  unlink(zip.file, force = FALSE)
  rm(zip.file, NE.url)
}

# # --- Read shape file
#
curr.dir <- getwd()
setwd(config$ancillary_dir)
geo.info <- rgdal::readOGR(".", layer="ne_10m_geography_marine_polys",
  verbose = TRUE)
setwd(curr.dir)
#
# # --- Overlay the matchups over the polygons with geographic information.
# # --- The result is an object that lists the basin in which matchups are located.
#
tt1 <- sp::over(x = pts, y = geo.info)  # Overlay points over areas
geo.location <- tt1$name                # Extract basin information
geo.location <- stringr::str_to_title(geo.location)
geo.location <- stringr::str_replace_all(string = geo.location,
  pattern = '  ', replacement = ' ')

orig$geo.location <- geo.location   # Store in dataframe orig

rm(file, infile, zip.file, NE.url,geo.info,geo.location,
   tt0,tt1,pts,crs.string,curr.dir); gc()
# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
# --- Calculate SST residuals for this sensor ----

# --- These residuals are the same for all sensors, but depend on the
# --- matchup format, as SSTs are expressed in deg C for GSFC format
# --- and in degrees K for MIA_L2GEN format.
  
if (config$matchups$format == 'GSFC' |
      (config$matchups$format == 'MIA_L2GEN' & config$sensor == 'AVHRR')) {
  orig$sst.minus.buoy.sst <- orig$cen.sst - orig$buoy.sst
  orig$sst.minus.ref.type.1.sst <- orig$cen.sst - orig$ref.type.1.SST
} else if (config$matchups$format == 'MIA_L2GEN' & config$sensor == 'VIIRS') {
  orig$sst.minus.buoy.sst <- orig$cen.sst - (orig$buoy.sst + 273.15)
  orig$sst.minus.ref.type.1.sst <- orig$cen.sst - (orig$ref.type.1.SST + 273.15)
} else {
    Log.error('Matchups format or sensor name not correctly specified in configuration')
}
  
# --- These residuals are sensor-dependent
  
if (config$matchups$format == 'GSFC' & config$sensor == 'VIIRS') {
  orig$sst3b.minus.buoy.sst <- orig$cen.sst3b - (orig$buoy.sst)
  orig$sst3b.minus.ref.type.1.sst <- orig$cen.sst3b - (orig$cen.ref.type.1.SST)
} else if (config$matchups$format == 'GSFC' & config$sensor == 'MODIS') {
  orig$sst4.minus.buoy.sst <- orig$cen.sst4 - (orig$buoy.sst)
  orig$sst4.minus.ref.type.1.sst <- orig$cen.sst4 - (orig$cen.ref.type.1.SST)
} else if (config$matchups$format == 'MIA_L2GEN' & config$sensor == 'VIIRS') {
  orig$sst3b.minus.buoy.sst <- orig$cen.sst3b - (orig$buoy.sst + 273.15)
  orig$sst3b.minus.ref.type.1.sst <- orig$cen.sst3b - (orig$ref.type.1.SST + 273.15)
} else if (config$matchups$format == 'MIA_L2GEN' & config$sensor == 'MODIS') {
  orig$sst4.minus.buoy.sst <- orig$cen.sst4 - (orig$buoy.sst + 273.15)
  orig$sst4.minus.ref.type.1.sst <- orig$cen.sst4 - (orig$ref.type.1.SST + 273.15)
} else {
  if (config$sensor != 'AVHRR') {Log.error('Sensor name is not correctly specified')}
}
# ------------------------------------------------------------------------------

Log.info('END creation of additional variables')

# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# --- FINALLY, SAVE INPUT DATA FRAME "ORIG" ----
# --- to a backup data frame and a binary file.
# -----------------------------------------------------------------------------#
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Save "orig" DF into R objects in separate directory ----

Log.info(paste('BEGIN saving matchup objects', matchup.object))
Log.info(paste('Saving stable matchup object', matchup.object))

# --- Copy contents of 'orig' to object defined in 'matchup object'
assign(matchup.object, orig)

# --- Save contents of object named in 'matchup.object'
# --- to a binary file (extension .Rdata)

Log.info('Saving matchups in binary R file')
save(list = matchup.object, file = matchup.bin.file,
  ascii = FALSE, precheck = TRUE)

# load(matchup.txt.file, verbose = TRUE)

# --- Save contents of object named in 'matchup.object'
# --- to an ASCII file (extension txt)

Log.info('Saving matchups in ASCII file')
save(list = matchup.object, file = matchup.txt.file,
  ascii = TRUE, compress = 'gzip', precheck = TRUE)

Log.info('END of saving matchup objects')
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Clean up most objects ----
# --- EXCEPT those objects equal to "orig" or starting
# --- with strings "VIIRS", "MODIS" or "config".

Log.info('BEGIN to clean up')

keep.strings <- c("^config$", "^orig$", "^ptm$",
  "VIIRS", "MODIS", "AVHRR")

tt1 <- (list = setdiff(ls(), lsf.str()))
tt2 <- sapply(keep.strings, grepl, tt1, ignore.case=TRUE)
tt3 <- apply(tt2, 1, any)
tt4 <- tt1[!tt3]

rm(list = tt4)
rm(tt1, tt2, tt3, tt4, keep.strings); gc()

save.image()  # Save .Rdata in working directory
Log.info('END of cleaning up')
# ------------------------------------------------------------------------------

# END of script 01_read_matchups.R
