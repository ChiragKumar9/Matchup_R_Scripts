# ---- This script creates a matchup of matchups for a collocation analysis.
# ---- The dplyr::left_join() function is used to do the joining and the
# ---- resulting dataframe is ready for a collocation with all variables in
# ---- the same units and same variable types.

# -----------------------------------------------------------------------------#
# ---- Require necessary packages and prep workspace with AQUA and VIIRS objects ----

#Packages
require(dplyr)

#Objects
AQUA <- load('~/Projects/MODIS/Matchups/Results/objects/MODIS_Aqua_GSFC_SNB_Class_6.4.1_ao_2016_10_01.RData')
VIIRS <- load("~/Projects/MODIS/Matchups/Results/objects/VIIRS_Suomi NPP_MIA_L2GEN_ALL_Class_6.4.1_ao_2016_09_29_with_ancillary.Rdata")
# --------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- Create as.celsius() function ----

# The as.celsius() function takes Kelvin temperatures as input and returns them in celsius
as.celsius <- function(kelvin) {
  celsius <- kelvin - 273.15
  return(celsius)
}
# -------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- Begin checks on matchup objects ----

#Check whether objects aqua and viirs exist

if (!exists("AQUA")) {
  Log.error("Aqua object does not exist.")
}

if (!exists("VIIRS")) {
  Log.error("VIIRS object does not exist")
}
# -------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- Join aqua and viirs objects into one ----

#Convert VIIRS BTs from kelvin to celsius

pattern = c('cen(.+?)', 'med(.+?)', 'max(.+?)', 'min(.+?)')
pattern = paste(pattern, collapse = '|')
for (uuu in seq(from = 1, to = length(names(VIIRS)), by = 1)) {
  uu1 <- names(VIIRS)[uuu]
  if (grepl(pattern, uu1)) {
    VIIRS[uu1] <- as.celsius(VIIRS[uu1])
  }
}

#Convert timedates from POSIXct objects to characters
VIIRS$buoy.timedate <- as.character(VIIRS$buoy.timedate)
AQUA$buoy.timedate <- as.character(AQUA$buoy.timedate)

VIIRS$sat.timedate <- as.character(VIIRS$sat.timedate)
AQUA$sat.timedate <- as.character(AQUA$sat.timedate)

#Join VIIRS and AQUA into one object
tt1 <- dplyr::inner_join(AQUA, VIIRS, by = c("buoy.pftime", "buoy.lon", "buoy.lat", "buoy.id"))
orig_j <- as.data.frame(tt1)

#Convert timedates back from characters to POSIXct objects
orig_j$buoy.timedate.x <- lubridate::ymd_hms(orig_j$buoy.timedate.x)
orig_j$buoy.timedate.y <- lubridate::ymd_hms(orig_j$buoy.timedate.y)

orig_j$sat.timedate.x <- lubridate::ymd_hms(orig_j$sat.timedate.x)
orig_j$sat.timedate.y <- lubridate::ymd_hms(orig_j$sat.timedate.y)


#Save joint object
save(orig_j, "/home/ckk/Projects/MODIS/Matchups/Results/objects/Joined_AQUA_VIIRS_Matchups_No_Filtering.Rdata")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# ---- Filting orig_j ----


#Filter by solz, platform, time difference, and quality
orig_filtered <- dplyr::tbl_df(orig_j) %>%
  dplyr::filter(solz.x >= 90 & solz.y >= 90) %>%
  dplyr::filter(insitu.platform.x != "Ship") %>%
  dplyr::filter(as.numeric(abs(sat.timedate.x - sat.timedate.y)) <= 3600)

#Tables showing spread of qualities for the different sensors
xtabs(~ qsst.x + qsst.y, data = orig_j)

xtabs(~ qsst.x + qsst.y, data = orig_filtered)

# ------------------------------------------------------------------------------


















xtabs(~ qsst.x + qsst.y, data = orig_j)

difftime <- as.numeric(abs(orig_j$sat.timedate.x - orig_j$sat.timedate.y))









# ---- Workspace ----

bank_codes <- seq(from = 1, to = 5, by = 1)
names <- c("Guillermo", "Chirag", "Poonam", "Arvind", "Ishana")
money <- c(10000000, 9, 11, 8, 12)
names.codes <- data.frame(names, bank_codes)
codes.money <- data.frame(bank_codes, money)

bank.inner <- dplyr::inner_join(names.codes, codes.money) #all rows from x where there are matching values in y, keeping just columns from x
#Maybe what we want
#Worth noting that if multiple matches between x and y, all matches will be returned

bank.semi <- dplyr::semi_join(names.codes, codes.money) #all rows from x where there are matching values in y, keeping just columns from x
#Not what we want

bank.left <- dplyr::left_join(names.codes, codes.money) #all rows from x, and all columns from x and y
#Maybe what we want
#Worth noting that if multiple matches between x and y, it will return all of them

bank.anti <- dplyr::anti_join(names.codes, codes.money) #all rows from x where there are not matching values in y
#Not what we want

bank.full <- dplyr::full_join(names.codes, codes.money) #all rows and all columns from both x and y
#Not what we want
