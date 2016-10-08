
# -----------------------------------------------------------------------------#
# --- Logging functions -----

library(futile.logger)

Log.layout <- function() {
  layout <- '[~l] [~t] [~m]'
  futile.logger::flog.layout(futile.logger::layout.format(layout))
}

Log.threshold <- function(logLevel) {
  futile.logger::flog.threshold(logLevel)
}

Log.debug <- function(message) {
  Log.layout()
  futile.logger::flog.debug(message)
}

Log.info <- function(message) {
  Log.layout()
  futile.logger::flog.info(message)
}

Log.warn <- function(message) {
  Log.layout()
  futile.logger::flog.warn(message)
}

Log.error <- function(message) {
  Log.layout()
  futile.logger::flog.error(message)
}

Log.appender <- function(logfile) {
  futile.logger::flog.appender(appender.file(logfile))
}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function get_matchup_variables() ----
# --- This function returns all variables that should be present in matchups
# --- for a given format.

get_matchup_variables <- function(matchup.format = c('MIA_L2GEN','GSFC'),
  sensor = c('VIIRS','MODIS','AVHRR'),
  ancillary.data = FALSE)  {

  #   matchup.format <- 'MIA_L2GEN'
  #   sensor <- 'VIIRS'

  # --- Check arguments

  if (missing(matchup.format)) {
    Log.error('Argument \'matchup.format\' must be specified')
  } else {
    if (! matchup.format %in% c('MIA_L2GEN', 'GSFC')) {
      Log.error('Matchup format must be \'MIA_L2GEN\' or \'GSFC\'')
    }
  }

  if (missing(sensor)) {
    Log.error('Argument \'sensor\' must be specified')
  } else {
    if (! sensor %in% c('VIIRS','MODIS','AVHRR')) {
      Log.error('Sensor must be one of \'VIIRS\', \'MODIS\', or \'AVHRR\'')
    }
  }

  # List of variables for each format and sensor combination

  if (matchup.format == 'MIA_L2GEN' && sensor == 'VIIRS') {
    # Matchup variables
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for VIIRS sensor.
    matchup.vars <-c("buoy.date","buoy.time","buoy.pftime","buoy.lon",
      "buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd",
      "buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","azimuth","aoi","detector","mirror","sstflg","sst3bflg",
      "l2flg","qsst","qsst3b","sunside","glint",
      "pixel","line","boxsizx","boxsizy",
      "ray.ch1","ray.ch2","aer.ch1","aer.ch2",
      "prt.avg","prt.1","prt.2","prt.3","prt.4",
      "emis.ch3","emis.ch4","emis.ch5","slope.ch3","slope.ch4","slope.ch5",
      "intcp.ch3","intcp.ch4","intcp.ch5",
      "cen.sst","cen.sst3b",
      "cen.630","cen.678","cen.748","cen.850","cen.1380","cen.1610","cen.3750",
      "cen.3959","cen.4050","cen.6715","cen.7325",
      "cen.8550","cen.11000","cen.12000",
      "cen.39.40.ref",
      "med.sst","med.sst3b",
      "med.630","med.678","med.748","med.850","med.1380","med.1610","med.3750",
      "med.3959","med.4050","med.6715","med.7325",
      "med.8550","med.11000","med.12000",
      "med.39.40.ref",
      "min.sst","min.sst3b",
      "min.630","min.678","min.748","min.850","min.1380","min.1610","min.3750",
      "min.3959","min.4050","min.6715","min.7325",
      "min.8550","min.11000","min.12000",
      "min.39.40.ref",
      "max.sst","max.sst3b",
      "max.630","max.678","max.748","max.850","max.1380","max.1610","max.3750",
      "max.3959","max.4050","max.6715","max.7325",
      "max.8550","max.11000","max.12000",
      "max.39.40.ref",
      "avg.sst","avg.sst3b",
      "avg.630","avg.678","avg.748","avg.850","avg.1380","avg.1610","avg.3750",
      "avg.3959","avg.4050","avg.6715","avg.7325",
      "avg.8550","avg.11000","avg.12000",
      "avg.39.40.ref",
      "sd.sst","sd.sst3b",
      "sd.630","sd.678","sd.748","sd.850","sd.1380","sd.1610","sd.3750",
      "sd.3959","sd.4050","sd.6715","sd.7325",
      "sd.8550","sd.11000","sd.12000",
      "sd.39.40.ref",
      "anc.type","anc.wspd","anc.wdir","anc.wv",
      "matchup.version",
      "ref.type.1","ref.type.1.SST")
    # Eliminated "ref.type.2" and "ref.type.2.SST" variables
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'MODIS') {
    # Matchup variables
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for MODIS sensor.
    matchup.vars <- c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd",
      "buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","azimuth","aoi","detector","mirror","sstflg","sst3bflg",
      "l2flg","qsst","qsst3b","sunside","glint",
      "pixel","line","boxsizx","boxsizy",
      "ray.ch1","ray.ch2","aer.ch1","aer.ch2",
      "prt.avg","prt.1","prt.2","prt.3","prt.4",
      "emis.ch3","emis.ch4","emis.ch5","slope.ch3","slope.ch4","slope.ch5",
      "intcp.ch3","intcp.ch4","intcp.ch5",
      "cen.sst",
      "cen.sst4",
      "cen.630","cen.678","cen.748","cen.850","cen.1380","cen.1610","cen.3750",
      "cen.3959","cen.4050","cen.6715","cen.7325",
      "cen.8550","cen.11000","cen.12000",
      "cen.39.40.ref",
      "med.sst","medsst4",
      "med.630","med.678","med.748","med.850","med.1380","med.1610","med.3750",
      "med.3959","med.4050","med.6715","med.7325",
      "med.8550","med.11000","med.12000",
      "med.39.40.ref",
      "min.sst",
      "min.sst4",
      "min.630","min.678","min.748","min.850","min.1380","min.1610","min.3750",
      "min.3959","min.4050","min.6715","min.7325",
      "min.8550","min.11000","min.12000",
      "min.39.40.ref",
      "max.sst",
      "max.sst4",
      "max.630","max.678","max.748","max.850","max.1380","max.1610","max.3750",
      "max.3959","max.4050","max.6715","max.7325",
      "max.8550","max.11000","max.12000",
      "max.39.40.ref",
      "avg.sst","avg.sst4",
      "avg.630","avg.678","avg.748","avg.850","avg.1380","avg.1610","avg.3750",
      "avg.3959","avg.4050","avg.6715","avg.7325",
      "avg.8550","avg.11000","avg.12000",
      "avg.39.40.ref",
      "sd.sst",
      "sd.sst4",
      "sd.630","sd.678","sd.748","sd.850","sd.1380","sd.1610","sd.3750",
      "sd.3959","sd.4050","sd.6715","sd.7325",
      "sd.8550","sd.11000","sd.12000",
      "sd.39.40.ref",
      "anc.type","anc.wspd","anc.wdir","anc.wv",
      "matchup.version",
      "ref.type.1","ref.type.1.SST",
      "ref.type.2","ref.type.2.SST")
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'AVHRR') {
    # Matchup variables
    # for format 'MIA-L2GEN' (aka 'new' matchup format)
    # and for AVHRR sensor.
    matchup.vars <- c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd",
      "buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","azimuth","aoi","detector","mirror","sstflg","sst3bflg",
      "l2flg","qsst","qsst3b","sunside","glint",
      "pixel","line","boxsizx","boxsizy",
      "ray.ch1","ray.ch2","aer.ch1","aer.ch2",
      "prt.avg","prt.1","prt.2","prt.3","prt.4",
      "emis.ch3","emis.ch4","emis.ch5","slope.ch3","slope.ch4","slope.ch5",
      "intcp.ch3","intcp.ch4","intcp.ch5",
      "cen.sst",
      "cen.sst3b",
      "cen.630","cen.678","cen.748","cen.850","cen.1380","cen.1610","cen.3750",
      "cen.3959","cen.4050","cen.6715","cen.7325",
      "cen.8550","cen.11000","cen.12000",
      "cen.39.40.ref",
      "med.sst","med.sst3b",
      "med.630","med.678","med.748","med.850","med.1380","med.1610","med.3750",
      "med.3959","med.4050","med.6715","med.7325",
      "med.8550","med.11000","med.12000",
      "med.39.40.ref",
      "min.sst",
      "min.sst3b",
      "min.630","min.678","min.748","min.850","min.1380","min.1610","min.3750",
      "min.3959","min.4050","min.6715","min.7325",
      "min.8550","min.11000","min.12000",
      "min.39.40.ref",
      "max.sst",
      "max.sst3b",
      "max.630","max.678","max.748","max.850","max.1380","max.1610","max.3750",
      "max.3959","max.4050","max.6715","max.7325",
      "max.8550","max.11000","max.12000",
      "max.39.40.ref",
      "avg.sst","avg.sst3b",
      "avg.630","avg.678","avg.748","avg.850","avg.1380","avg.1610","avg.3750",
      "avg.3959","avg.4050","avg.6715","avg.7325",
      "avg.8550","avg.11000","avg.12000",
      "avg.39.40.ref",
      "sd.sst",
      "sd.sst3b",
      "sd.630","sd.678","sd.748","sd.850","sd.1380","sd.1610","sd.3750",
      "sd.3959","sd.4050","sd.6715","sd.7325",
      "sd.8550","sd.11000","sd.12000",
      "sd.39.40.ref",
      "anc.type","anc.wspd","anc.wdir","anc.wv",
      "matchup.version",
      "ref.type.1","ref.type.1.SST",
      "ref.type.2","ref.type.2.SST")



  } else if (matchup.format == 'GSFC' && sensor == 'VIIRS') {
    # Matchup variables
    # for format 'GSFC' (made at Goddard Space Flight Center)
    # and for VIIRS sensor.
    matchup.vars <- c("buoy.date","buoy.time","sat.pftime",
      "sat.lat","sat.lon",
      "solz","satz","azim","aoi","detector","pixnum","mirror","sunside",
      "spare3","l2comflg",
      "qual","qsst","qsst4","glintflg",
      "cen.sst","cen.sst3b",
      "cen.3750","cen.3959","cen.4050","cen.11000","cen.12000","cen.1380",
      "cen.6715","cen.7325","cen.8550","cen.39.40.ref","cen.678l","cen.spare1",
      "cen.ref.type.1.SST","med.sst","med.sst3b",
      "med.3750","med.3959","med.4050","med.11000","med.12000","med.1380",
      "med.6715","med.7325","med.8550","med.39.40.ref","med.678l","med.spare1",
      "med.ref.type.1.SST",
      "min.sst","min.sst3b",
      "min.3750","min.3959","min.4050",
      "min.11000","min.12000","min.1380","min.6715","min.7325","min.8550",
      "min.39.40.ref","min.678l","min.spare1","min.ref.type.1.SST",
      "max.sst","max.sst3b",
      "max.3750","max.3959","max.4050","max.11000","max.12000","max.1380",
      "max.6715","max.7325","max.8550","max.39.40.ref","max.678l","max.spare1",
      "max.ref.type.1.SST",
      "avg.sst","avg.sst3b",
      "avg.3750", "avg.3959","avg.4050","avg.11000","avg.12000","avg.1380",
      "avg.6715","avg.7325","avg.8550",
      "avg.39.40.ref","avg.678l","avg.spare1","avg.ref.type.1.SST",
      "sd.sst","sd.sst3b",
      "sd.3750","sd.3959","sd.4050","sd.11000","sd.12000",
      "sd.1380","sd.6715","sd.7325", "sd.8550","sd.39.40.ref","sd.678l",
      "sd.spare1","sd.ref.type.1.SST","sat","granule","sstflg","sst4flg",
      "l1blutver","source","buoy.pftime","buoy.lat",
      "buoy.lon","buoy.id","buoy.sst","spare2")
  } else if (matchup.format == 'GSFC' && sensor == 'MODIS') {
    # Matchup variables
    # for format 'GSFC' (made at Goddard Space Flight Center)
    # and for MODIS sensor.
    matchup.vars <- c("buoy.date","buoy.time","sat.pftime","sat.lat","sat.lon",
      "solz","satz","azim","aoi","detector","pixnum","mirror","sunside",
      "l2flg","l2comflg","qual","qsst","qsst4","glintflg",
      "cen.sst","cen.sst4",
      "cen.3750","cen.3959","cen.4050","cen.11000","cen.12000",
      "cen.1380","cen.6715","cen.7325","cen.8550","cen.39.40.ref",
      "cen.678l","cen.spare1","cen.ref.type.1.SST",
      "med.sst","med.sst4",
      "med.3750","med.3959","med.4050","med.11000","med.12000",
      "med.1380","med.6715","med.7325","med.8550","med.39.40.ref",
      "med.678l","med.spare1","med.ref.type.1.SST",
      "min.sst","min.sst4",
      "min.3750","min.3959","min.4050","min.11000","min.12000",
      "min.1380","min.6715","min.7325","min.8550","min.39.40.ref",
      "min.678l","min.spare1","min.ref.type.1.SST",
      "max.sst","max.sst4",
      "max.3750","max.3959","max.4050","max.11000","max.12000",
      "max.1380","max.6715","max.7325","max.8550","max.39.40.ref",
      "max.678l","max.spare1","max.ref.type.1.SST",
      "avg.sst","avg.sst4",
      "avg.3750","avg.3959","avg.4050","avg.11000","avg.12000",
      "avg.1380","avg.6715","avg.7325","avg.8550","avg.39.40.ref",
      "avg.678l","avg.spare1","avg.ref.type.1.SST",
      "sd.sst","sd.sst4",
      "sd.3750","sd.3959","sd.4050","sd.11000","sd.12000",
      "sd.1380","sd.6715","sd.7325","sd.8550","sd.39.40.ref",
      "sd.678l","sd.spare1","sd.ref.type.1.SST",
      "sat","granule","sstflg","sst4flg",
      "l1blutver",
      "source","buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
      "spare2")
    # "amsre.type","amsre.val",
    # "amsr2.type","amsr2.val",
    # "wsat.type","wsat.val"
  } else if (matchup.format == 'GSFC' && sensor == 'AVHRR') {
    # Matchup variables
    # for format 'GSFC' (made at Goddard Space Floght Center)
    # and for AVHRR sensor.
    matchup.vars <- NULL
  }

  # --- Add variable names for ancillary data, such as data
  # --- from AMSRE, AMSR2 and Windsat.

  if (ancillary.data == TRUE) {
    matchup.vars <- c(matchup.vars,
      "amsre.type", "amsre.val",
      "amsr2.type", "amsr2.val",
      "wsat.type", "wsat.val")
  }

  return(matchup.vars)

} # End of function get_matchup_variables()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function keep_matchup_variables() ----
# --- This function returns a list of variables to keep for a given sensor.
# --- The variables kept are all those that should be included for a sensor
# --- (remember that matchups may have columns for variables that do not exist
# --- for a given sensor and are filled with '-999.0' (ie, missing values)).

keep_matchup_variables <- function(matchup.format = NULL,
                                  sensor = NULL, ancillary.data = NULL) {

  # --- Checking arguments
  if (missing(matchup.format)) {
    Log.error('Argument \'matchup.format\' must be specified')
  } else {
    if (! matchup.format %in% c('MIA_L2GEN', 'GSFC', 'MIA_OLD')) {
      Log.error('Matchup format must be \'MIA_L2GEN\' or \'GSFC\'')
    }
  }

  if (missing(sensor)) {
    Log.error('Argument \'sensor\' must be specified')
  } else {
    if (! sensor %in% c('VIIRS','MODIS','AVHRR')) {
      Log.error('Sensor must be one of \'VIIRS\', \'MODIS\', or \'AVHRR\'')
    }
  }

  # --- List of variables to keep for each format and sensor combination

  if (matchup.format == 'MIA_L2GEN' && sensor == 'VIIRS') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for VIIRS sensor.
    keep.vars <-  c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd","buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","detector", "mirror","sstflg","sst3bflg",
      "l2flg","qsst","qsst3b","sunside","glint",
      "pixel","line","boxsizx","boxsizy",
      "cen.sst","cen.sst3b","cen.678","cen.748","cen.1380",
      "cen.1610","cen.3750","cen.4050","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
      "med.sst","med.sst3b","med.678","med.748","med.1380",
      "med.1610","med.3750","med.4050","med.8550","med.11000","med.12000","med.39.40.ref",
      "min.sst","min.sst3b","min.678","min.748","min.1380",
      "min.1610","min.3750","min.4050","min.8550","min.11000","min.12000","min.39.40.ref",
      "max.sst","max.sst3b","max.678","max.748","max.1380",
      "max.1610","max.3750","max.4050","max.8550","max.11000","max.12000","max.39.40.ref",
      "avg.sst","avg.sst3b","avg.678","avg.748","avg.1380",
      "avg.1610","avg.3750","avg.4050","avg.8550","avg.11000","avg.12000","avg.39.40.ref",
      "sd.sst","sd.sst3b","sd.678","sd.748","sd.1380",
      "sd.1610","sd.3750","sd.4050","sd.8550","sd.11000","sd.12000","sd.39.40.ref",
      "anc.type","anc.wspd","anc.wdir","anc.wv",
      "matchup.version",
      "ref.type.1","ref.type.1.SST")
    # Eliminated ref.type.2 and ref.type.2.SST
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'MODIS') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for MODIS sensor.
    keep.vars <- c("sat.date","sat.time","sat.pftime", "sat.lat", "sat.lon",
      "solz", "satz", "mirror","qsst", "qsst4","glint",
      "cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
      "cen.6715","cen.7325","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
      "med.sst","med.sst4","med.678","med.748","med.1380","med.3750","med.3959","med.4050",
      "med.6715","med.8550","med.11000","med.12000",
      "min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
      "min.6715","min.7325","min.8550","min.11000","min.12000",
      "max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
      "max.6715","max.7325","max.8550","max.11000","max.12000",
      "avg.sst","avg.sst4","avg.678","avg.748","avg.1380","avg.3750","avg.3959","avg.4050",
      "avg.6715","avg.7325","avg.8550","avg.11000","avg.12000",
      "sd.sst","sd.sst4","sd.678","sd.748","sd.1380","sd.3750","sd.3959","sd.4050",
      "sd.6715","sd.7325","sd.8550","sd.11000","sd.12000",
      "buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
      "ref.type.1.SST")
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'AVHRR') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for AVHRR sensor.
    keep.vars <- c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
                   "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd",
                   "buoy.wdir","buoy.airtmp",
                   "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
                   "solz","satz","azimuth","aoi","sstflg",
                   "l2flg","qsst","sunside","glint",
                   "pixel","line","boxsizx","boxsizy",
                   "ray.ch1","ray.ch2","aer.ch1","aer.ch2",
                   "prt.avg","prt.1","prt.2","prt.3","prt.4",
                   "emis.ch3","emis.ch4","emis.ch5","slope.ch3","slope.ch4","slope.ch5",
                   "intcp.ch3","intcp.ch4","intcp.ch5",
                   "cen.sst",
                   "cen.630","cen.3750",
                   "cen.3959","cen.11000","cen.12000",
                   "med.sst",
                   "med.3750",
                   "med.11000","med.12000",
                   "min.sst",
                   "min.3750",
                   "min.3959",
                   "min.11000","min.12000",
                   "max.sst",
                   "max.3750",
                   "max.11000","max.12000",
                   "avg.sst","avg.3750",
                   "avg.11000","avg.12000",
                   "sd.sst",
                   "sd.3750",
                   "sd.11000","sd.12000",
                   "anc.type","anc.wspd","anc.wdir","anc.wv",
                   "matchup.version",
                   "ref.type.1","ref.type.1.SST",
                   "ref.type.2","ref.type.2.SST")
  } else if (matchup.format == 'GSFC' && sensor == 'VIIRS') {
    # Matchup variables to KEEP
    # for format 'GSFC'
    # and for VIIRS sensor.
  } else if (matchup.format == 'GSFC' && sensor == 'MODIS') {
    # Matchup variables to KEEP
    # for format 'GSFC' matchup format)
    # and for MODIS sensor.
    keep.vars <- c("buoy.date","buoy.time","sat.pftime",
      "sat.lat","sat.lon",
      "solz","satz","azim","aoi","detector","pixnum","mirror","sunside",
      "l2comflg","qsst","qsst4","glintflg",
      "cen.sst","cen.sst4",
      "cen.3750","cen.3959","cen.4050","cen.11000","cen.12000",
      "cen.1380","cen.6715","cen.7325","cen.8550","cen.39.40.ref",
      "cen.678l",
      "cen.ref.type.1.SST",
      "med.sst","med.sst4",
      "med.3750","med.3959","med.4050","med.11000","med.12000",
      "med.1380","med.6715","med.7325","med.8550",
      "med.678l",
      "min.sst","min.sst4",
      "min.3750","min.3959","min.4050","min.11000","min.12000",
      "min.1380","min.6715","min.7325","min.8550",
      "min.678l",
      "max.sst","max.sst4",
      "max.3750","max.3959","max.4050","max.11000","max.12000",
      "max.1380","max.6715","max.7325","max.8550",
      "max.678l",
      "avg.sst","avg.sst4",
      "avg.3750","avg.3959","avg.4050","avg.11000","avg.12000",
      "avg.1380","avg.6715","avg.7325","avg.8550",
      "avg.678l","avg.spare1","avg.ref.type.1.SST",
      "sd.sst","sd.sst4",
      "sd.3750","sd.3959","sd.4050","sd.11000","sd.12000",
      "sd.1380","sd.6715","sd.7325","sd.8550",
      "sd.678l",
      "sat","granule","sstflg","sst4flg",
      "l1blutver",
      "source","buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
      "spare2")
  } else if (matchup.format == 'GSFC' && sensor == 'AVHRR') {
    # Matchup variables to KEEP
    # for format 'GSFC'
    # and for AVHRR sensor.
    keep.vars <- NULL
  } # End of checking for combinations of matchup format and sensor

  # --- Append variable names for ancillary data, such as data
  # --- from AMSRE, AMSR2 and Windsat.

  if (ancillary.data == TRUE) {
    keep.vars <- c(keep.vars,
      "amsre.type", "amsre.val",
      "amsr2.type", "amsr2.val",
      "wsat.type", "wsat.val")
  }

  return(keep.vars)
} # End of function keep_matchup_variables()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function keep_matchup_variables_collocation() ----
# --- This function returns a list of variables to keep for a given sensor 
# --- when doing a collocation analysis.
# --- The variables kept are all those that are necessary for creating a 
# --- matchup of matchups and doing the actual collocation analysis.
# --- (remember that matchups may have columns for variables that do not exist
# --- for a given sensor and are filled with '-999.0' (ie, missing values)).

keep_matchup_variables_collocation <- function(matchup.format = NULL,
  sensor = NULL, ancillary.data = NULL) {
  
  # --- Checking arguments
  if (missing(matchup.format)) {
    Log.error('Argument \'matchup.format\' must be specified')
  } else {
    if (! matchup.format %in% c('MIA_L2GEN', 'GSFC', 'MIA_OLD')) {
      Log.error('Matchup format must be \'MIA_L2GEN\' or \'GSFC\'')
    }
  }
  
  if (missing(sensor)) {
    Log.error('Argument \'sensor\' must be specified')
  } else {
    if (! sensor %in% c('VIIRS','MODIS','AVHRR')) {
      Log.error('Sensor must be one of \'VIIRS\', \'MODIS\', or \'AVHRR\'')
    }
  }
  
  # --- List of variables to keep for each format and sensor combination
  
  if (matchup.format == 'MIA_L2GEN' && sensor == 'VIIRS') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for VIIRS sensor.
    keep.vars <-  c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd","buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","qsst","qsst3b","sunside","glint",
      "cen.sst","cen.sst3b","cen.3750","cen.4050","cen.11000","cen.12000",
      "med.sst","med.sst3b","med.3750","med.4050",
      "med.11000","med.12000",
      "max.11000","max.12000",
      "min.11000","min.12000",
      "sd.11000","sd.12000",
      "ref.type.1","ref.type.1.SST")
    # Eliminated ref.type.2 and ref.type.2.SST
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'MODIS') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for MODIS sensor.
    keep.vars <- c("sat.date","sat.time","sat.pftime", "sat.lat", "sat.lon",
      "solz", "satz", "mirror","qsst", "qsst4","glint",
      "cen.sst","cen.sst4","cen.678","cen.748","cen.1380","cen.3750","cen.3959","cen.4050",
      "cen.6715","cen.7325","cen.8550","cen.11000","cen.12000","cen.39.40.ref",
      "med.sst","med.sst4","med.678","med.748","med.1380","med.3750","med.3959","med.4050",
      "med.6715","med.8550","med.11000","med.12000",
      "min.sst","min.sst4","min.678","min.748","min.1380","min.3750","min.3959","min.4050",
      "min.6715","min.7325","min.8550","min.11000","min.12000",
      "max.sst","max.sst4","max.678","max.748","max.1380","max.3750","max.3959","max.4050",
      "max.6715","max.7325","max.8550","max.11000","max.12000",
      "avg.sst","avg.sst4","avg.678","avg.748","avg.1380","avg.3750","avg.3959","avg.4050",
      "avg.6715","avg.7325","avg.8550","avg.11000","avg.12000",
      "sd.sst","sd.sst4","sd.678","sd.748","sd.1380","sd.3750","sd.3959","sd.4050",
      "sd.6715","sd.7325","sd.8550","sd.11000","sd.12000",
      "buoy.pftime","buoy.lat","buoy.lon","buoy.id","buoy.sst",
      "ref.type.1.SST")
  } else if (matchup.format == 'MIA_L2GEN' && sensor == 'AVHRR') {
    # Matchup variables to KEEP
    # for format 'MIA_L2GEN' (aka 'new Miami' matchup format)
    # and for AVHRR sensor.
    keep.vars <- c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd",
      "buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","azimuth","aoi","sstflg",
      "l2flg","qsst","sunside","glint",
      "pixel","line","boxsizx","boxsizy",
      "ray.ch1","ray.ch2","aer.ch1","aer.ch2",
      "prt.avg","prt.1","prt.2","prt.3","prt.4",
      "emis.ch3","emis.ch4","emis.ch5","slope.ch3","slope.ch4","slope.ch5",
      "intcp.ch3","intcp.ch4","intcp.ch5",
      "cen.sst",
      "cen.630","cen.3750",
      "cen.3959","cen.11000","cen.12000",
      "med.sst",
      "med.3750",
      "med.11000","med.12000",
      "min.sst",
      "min.3750",
      "min.3959",
      "min.11000","min.12000",
      "max.sst",
      "max.3750",
      "max.11000","max.12000",
      "avg.sst","avg.3750",
      "avg.11000","avg.12000",
      "sd.sst",
      "sd.3750",
      "sd.11000","sd.12000",
      "anc.type","anc.wspd","anc.wdir","anc.wv",
      "matchup.version",
      "ref.type.1","ref.type.1.SST",
      "ref.type.2","ref.type.2.SST")
  } else if (matchup.format == 'GSFC' && sensor == 'VIIRS') {
    # Matchup variables to KEEP
    # for format 'GSFC'
    # and for VIIRS sensor.
  } else if (matchup.format == 'GSFC' && sensor == 'MODIS') {
    # Matchup variables to KEEP
    # for format 'GSFC' matchup format)
    # and for MODIS sensor.
    keep.vars <-  c("buoy.date","buoy.time","buoy.pftime","buoy.lon","buoy.lat",
      "buoy.type","buoy.id","buoy.sst","buoy.qual","buoy.wspd","buoy.wdir","buoy.airtmp",
      "sat.id","sat.date","sat.time","sat.pftime","sat.lon","sat.lat",
      "solz","satz","qsst","qsst3b","sunside","glint",
      "cen.sst","cen.sst3b","cen.3750","cen.4050","cen.11000","cen.12000",
      "med.sst","med.sst3b","med.3750","med.4050",
      "med.11000","med.12000",
      "max.11000","max.12000",
      "min.11000","min.12000",
      "sd.11000","sd.12000",
      "ref.type.1","ref.type.1.SST")
    
  } else if (matchup.format == 'GSFC' && sensor == 'AVHRR') {
    # Matchup variables to KEEP
    # for format 'GSFC'
    # and for AVHRR sensor.
    keep.vars <- NULL
  } # End of checking for combinations of matchup format and sensor
  
  # --- Append variable names for ancillary data, such as data
  # --- from AMSRE, AMSR2 and Windsat.
  
  if (ancillary.data == TRUE) {
    keep.vars <- c(keep.vars,
      "amsre.type", "amsre.val",
      "amsr2.type", "amsr2.val",
      "wsat.type", "wsat.val")
  }
  
  return(keep.vars)
} # End of function keep_matchup_variables()
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------#
# --- Function apply_gross_quality_filters() ----

# --- VIIRS and MODIS brightness tempoeratures are in K
# --- so thresholds are expressed in K also

apply_gross_quality_filters <- function(x, matchup.format = NULL, sensor = NULL) {

  if (missing(matchup.format) | missing(sensor)) {
    Log.error('Argument \'matchup.format and sensor\' must be specified')
  } else {
    if (! matchup.format %in% c('MIA_L2GEN', 'GSFC')) {
      Log.error('Matchup format must be \'MIA_L2GEN\' or \'GSFC\'')
    }
  }

  # --- Apparently brightness temperatures in GSFC format are in degrees C
  # --- Convert the test boundaries deending on the u nit of BTs.

  if (matchup.format == 'GSFC' | (matchup.format == 'MIA_L2GEN' & sensor == "AVHRR") ) {
    bt.lower.limit <- -4
    bt.upper.limit.1 <- 35      # upper limit for 4 mu bands
    bt.upper.limit.2 <- 37      # upper limit for 11-12 mu bands
  } else if (matchup.format == 'MIA_L2GEN' & sensor == "VIIRS") {
    bt.lower.limit <- 269.15
    bt.upper.limit.1 <- 308.15  # upper limit for 4 mu bands
    bt.upper.limit.2 <- 310.15  # upper limit for 11-12 mu bands
  }

  output <- dplyr::tbl_df(x) %>%
    dplyr::filter((buoy.sst >= - 2) & (buoy.sst <= 45) &
      (is.na(cen.3750) |
      ((cen.3750 >= bt.lower.limit) & (cen.3750 <= bt.upper.limit.1))) &
      ((cen.11000 >= bt.lower.limit) & (cen.11000 <= bt.upper.limit.2)) &
      ((cen.12000 >= bt.lower.limit) & (cen.12000 <= bt.upper.limit.2) &
      ((max.11000 - min.11000) <= 1.5) &
      ((max.12000 - min.12000) <= 1.5)))

  return(output)

}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function get_satz_interval() ----
# --- This function splits values of satellite zenith angle
# --- into several intervals.
# --- Intervals are different for VIIRS and MODIS

get_satz_interval <- function(x, sensor = NULL) {

  # Boundaries for satellite zenith angle intervals

  if (sensor == 'VIIRS') {
    satz.boundaries <- c(-70, -48, -32, 0, 32, 48, 70)
    satz.labels <- c("<= -48","-48 to -32","-32 to nadir",
                     "nadir to +32","+32 to +48","> +48")
  } else if (sensor == 'MODIS') {
    satz.boundaries <- c(-70, -40, -30, 0, 30, 40, 70)
    satz.labels <- c("<= -40","-40 to -30","-30 to nadir",
                     "nadir to +30","+30 to +40","> +40")
  } else if (sensor == 'AVHRR') {
    # Matchup variables to KEEP for AVHRR
    satz.boundaries <- NULL
    satz.labels <- 'NOT defined yet'
  } else {
    Log.error('Specified sensor not recognized')
  }

  # --- Cut satellite zenith angle into intervals

  satzint <- cut(x,
                 breaks = satzbands,
                 include.lowest = TRUE,
                 labels = satz.labels)

}   # End of fucntion get_satz_interval()
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Function decode_insitu_id() ----
# --- The old IDs used to have just a number with 5 digits (buoy numbers)
# --- or a string (eg, with a ship name).

decode_insitu_id <- function(id = NULL) {

  # id <- c("IQ_57589","IW_57600","12345","MA_RonBrown") # for testing
  #id <- orig$buoy.id

  # --- Identify which records have "new" buoy IDs.
  # --- They are those which contain an underscore ("_")

  is.new.buoy.ID <- str_detect(id, "_")

  # --- For those records that do not have an underscore (ie, old buoy ID format),
  # --- insert the string "NV_". That is we assume that all IDs with the old format
  # --- come from the Navy data.
  # Note: this is not always true anymore...Liz changed the ID _ on radiometers so
  # now we must also check that there are no characters. Only the IQUAM IQ_ id's could
  # be ships  (unless we were dealing with the special N7 ship MDB dataset).
  # So now I changed the
  # code to only looks for letters in the IQ_ id's.

  # navy because we did not have ships in the GTS stream from navy
  is.navy <- !is.new.buoy.ID & !str_detect(id,
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]")

  # ISAR because we assume that all M-AERI matchups
  # had underscores but some of the ISAR did not
  is.isar <- !is.new.buoy.ID & str_detect(id,
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]")

  id[is.navy ] <- paste("NV_", id[is.navy], sep="")
  id[is.isar ] <- paste("IS_", id[is.isar], sep="")

  # --- Create objects to store source of in situ data and platform type.
  # --- The source can be a database (e.g., "NV" is "Navy", "IQ" is IQUAM data)
  # --- or a particular type of sensor (e.g., "MA" for MAERI).
  # --- If no prefix indicating source is presnet, we assume data come from the Navy,
  # --- so by default the source is "NV". -- this is wrong could also be isar - kak June 2015

  in.situ.source <- rep(NA, length.out = length(id))
  in.situ.platform <- rep(NA, length.out = length(id))

  # --- Use the underscore to split the buoy ID in two parts
  # --- (to the left and right of the "_").
  # --- NOTE: This command takes some time to execute.

  tt1 <- str_split_fixed(id, "_", n = 2)

  # --- Fill in values for in.situ.source

  in.situ.source <- tt1[ ,1]  # in situ source for buoys with new IDs

  table(in.situ.source, useNA="always")

  # --- Now work with the part of buoy IDs to the right of the underscore

  tt2 <- tt1[ ,2]  # in situ platform

  # --- Identify strings that have alphabetic characters in the IQUam dataset, as they
  # --- probably correspond to a ship's name.

  tt3 <- str_detect(tt2,
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]") &
    tt1[, 1] == "IQ"

  # --- Assign platform type "Ship" to records with alphabetic characters but can not be a radiometer

  in.situ.platform[tt3] <- "Ship"

  # --- Work with the ones that do not have characters

  tt4 <- as.numeric(tt2[!tt3])

  # --- Possible problem: some "old format" buoy IDs seem to have more than 5 characters

  tt4b <- str_length(tt4)   # Length of buoy ID string
  tt4c <- which(tt4b > 5)   # Select buoy IDs longer than 5 characters

  in.situ.source[tt4c]

  # table(tt4c)               # Table with frequencies of IDs with more than 5 digits

  # --- Separate types of buoy observations (drifters, moored buoys).
  # --- As the buoy IDs are WMO IDs, the WMO conventions indicate that
  # --- if the last three digits of the buoy ID range
  # --- from 000 to 499  --> MOORED buoys
  # --- from 500 to 999 -->  DRIFTING buoys

  tt5 <- as.numeric(str_sub(tt4, start = -3L))  # Extract last three digits

  tt6 <- ifelse( (tt5 >= 0) & (tt5 <= 499), "MooredBuoy", "DriftingBuoy")

  # --- Assign appropriate platform type  (moored or drifting buoy)
  # --- to records without alphabetic characters

  in.situ.platform[!tt3] <- tt6   # Type of buoy

  # separate out the radiometers from the ship and bouys

  tt7 <- in.situ.source == "NV" |  in.situ.source == "IQ" |
    in.situ.source =="GH" | in.situ.source =="AR"

  in.situ.platform[!tt7] <- "Radiometer"
  # table(in.situ.platform, useNA = "always")

  tt8 <- in.situ.source =="AR"
  in.situ.platform[tt8] <- "Argos Float"

  tt9 <- in.situ.source == "IM"
  in.situ.platform[tt9] <- "IMOS Ship"


  output <- data.frame(id = id, source = in.situ.source,
    platform = in.situ.platform)

  return(output)

  rm(id,is.new.buoy.ID,tt1,tt2,tt3,tt4,tt4b,tt4c,tt5,tt6,tt7); gc()

} # End of fucntion decode_insitu_id()
# ------------------------------------------------------------------------------







# -----------------------------------------------------------------------------#
# --- Function list_archives ----

# --- List all annual compressed archives of matchup files located
# --- in separate input directories (one per year).
# --- NOTE: We assume that ONLY daily matchup files are present within
# --- each annual directory.

list_archives <- function(archive.dir = NULL) {

  # --- Verify that the input directory specified in configuration file exists

  if (! dir.exists(config$matchups_base_indir)) {
    Log.error('Specified archive directory does not exist')
  }

  # --- List existing annual archives of matchup files

  archive.file.list <- list.files(path = config$matchups_base_indir,
    recursive = TRUE,
    all.files = FALSE,
    include.dirs = FALSE,
    full.names = TRUE)

  n.archive.files <- length(archive.file.list)		# Number of compressed anual input files

  if (n.archive.files == 0) {							# Is directory empty?
    Log.error(paste('There are NO annual archive files in',
                    config$matchups_base_indir))
  } else {
    Log.info(paste('There are', n.archive.files,
                   'compressed archive files for sensor',
                   config$sensor))
  }

  return(archive.file.list)

}   # End of list_annual_archives
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function list_daily_files_in_archives ----

# --- List (but do not extract) all daily matchup files
# --- in current annual matchup archives.

list_daily_files_in_archives <- function(arc.file.list = NULL) {

  # arc.file.list <- archive.file.list

  if (length(arc.file.list) < 1) {
    Log.error('List of annual archives has length zero')
  }

  # --- Lost daily files in first annual archive directory

  Log.info(paste('Listing files within compressed annual file',
               1, 'of', length(arc.file.list)))

  file.list1 <- utils::untar(arc.file.list[1], verbose = TRUE, list = TRUE)
  file.list2 <- data.frame(daily = file.list1, annual = arc.file.list[1])

  # --- If more than one annual archive file, loop through them
  # --- to list files.

  if (length(arc.file.list) > 1) {

    for (j in 2:length(arc.file.list)) {
      Log.info(paste('Listing files within compressed annual file',
                     j, 'of', length(arc.file.list)))
      tt1 <- utils::untar(arc.file.list[j], verbose = TRUE, list = TRUE)
      tt2 <- data.frame(daily = tt1, annual = arc.file.list[j])
      file.list2 <- as.data.frame(rbind(file.list2, tt2))
    } # End of looping through n-1 annual archives

  }

  Log.info('Finished listing CURRENT daily matchup files')

  if (nrow(file.list2) > 0) {
    # This is the LIST of all daily files in annual archives
    current.file.list <- as.data.frame(file.list2)
    Log.info(paste('There are', nrow(current.file.list),
                   'CURRENT daily matchup files'))
  } else {
    Log.warn('No daily matchup files were found')
    current.file.list <- NULL
  }

  return(current.file.list)

}   # End of function list_daily_files_in_archives
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Function extract_daily_matchups ----

# --- Uncompress annual matchup files and
# --- store daily files in temporary directory.

extract_daily_matchups <- function(arc.file.list = NULL,
                                   extract.to.dir = NULL,
                                   archive.format = 'tgz') {

  # --- Uncompress all daily files to temporary extraction directory

  for (j in 1:length(arc.file.list)) {
    Log.info(paste('Extracting files from annual archive',
                   basename(arc.file.list[j])))
    if (archive.format == 'tgz') {
      untar(arc.file.list[j],
            verbose = TRUE,
            exdir = extract.to.dir)
    } else {
      Log.error('The only recognized archive format currently is \'tgz\'')
    }
  }

}   # End of function extract_daily_matchups
# ------------------------------------------------------------------------------

