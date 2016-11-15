# --- This script creates a matchup of matchups for a collocation analysis.
# --- dplyr is used to join AQUA and VIIRS matchups and the result is
# --- filtered. The resulting dataframe has only nighttime buoy retrievals,
# --- where AQUA and VIIRS buoy retrievals are less than five minutes apart,
# --- satellite retrievals that are less than 1 hour apart, and the distance
# --- between the satellites and buoy is less than 1 km.

# -----------------------------------------------------------------------------#
# --- Require necessary packages and prep workspace with AQUA and VIIRS objects ----

#Packages
require(dplyr)
require(sp)
require(mapview)
require(geosphere)
#Objects
load("~/Projects/Matchup_R_Scripts/Results/objects/MODIS_Aqua_GSFC_SNB_Class_6.4.1_ao_2016_10_01.RData")
VIIRS <- load("~/Projects/Matchup_R_Scripts/Results/objects/VIIRS_Suomi_NPP_MIA_L2GEN_ALL_Class_6.4.1_ao_2016_09_29_with_ancillary.Rdata")
VIIRS <- `VIIRS_Suomi NPP_MIA_L2GEN_ALL_Class_6.4.1_ao_2016_09_29`
rm(`VIIRS_Suomi NPP_MIA_L2GEN_ALL_Class_6.4.1_ao_2016_09_29`); gc()
# --------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Create as.celsius() function ----

# The as.celsius() function takes Kelvin temperatures as input and returns them in celsius
as.celsius <- function(kelvin) {
  celsius <- kelvin - 273.15
  return(celsius)
}
# -------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Create collocation functions ----
#Bias
#This function computes the systematic error, or bias, between two different observation types. By subtracting the
#means of one observation type from the mean of another observation type, the bias between those two observation 
#types can be computed.

bias <- function(source1, source2, source3) {
  if (!is.null(nrow(source1)) | !is.null(nrow(source2)) | !is.null(nrow(source3)) | length(source1) <= 1 | length(source2) <= 1 | length(source3) <= 1) {
    stop("Only vectors can be passed as arguments in the bias function.")
  }

  bias_source1source3 <- mean(source1) - mean(source3) #Bias between source1 and source2
  
  bias_source2source3 <- mean(source2) - mean(source3) #Bias between source2 and source3
  
  #Return a dataframe of biases that can re-scale each observation type
  biases <- c(bias_source1source3, bias_source2source3)
  names(biases) <- c("bias_source1source3", "bias_source2source3")
  return(biases)
}


#Variance
#This function computes the variance of error in one observation type by computing the variance between the 
#difference in two error types. The bias and mean of two observation types can be used to compute the variance of 
#the difference in the observation types. The variance of differences in the observation type can be used to solve
#directly for the variance of error in a single observation type.

variance <- function(source1, source2, source3) {
  #CHECK INSTEAD WITH as.vector()
  #if (!is.null(nrow(aqua)) | !is.null(nrow(viirs)) | !is.null(nrow(buoy)) | length(aqua) <= 1 | length(viirs) <= 1 | length(buoy) <= 1) {
   # stop("Only vectors can be passed as arguments in the variance function.")
  #}
  
  #Call bias function to re-scale measurements
  biases <- bias(source1, source2, source3)
  
  #PUT A COMMENT
  source1_scaled <- source1 - biases['bias_source1source3']
  source2_scaled <- source2 - biases['bias_source2source3']
  
  #Variance of errors in observation types
  variance_source1 <- mean((source1_scaled - source2_scaled) * (source1_scaled - source3))     #Variance of source1
  variance_source2 <- mean((source1_scaled - source2_scaled) * (source2_scaled - source3))     #Variance of source2
  variance_source3 <- mean((source1_scaled - source3) * (source2_scaled - source3))            #Variance of source3
  
  #Take all the individual variances and return them as a dataframe
  variances <- data.frame(variance_source1, variance_source2, variance_source3)
  colnames(variances) <- c("variance_source1", "variance_source2", "variance_source3")
  return(variances)
}
# -------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Begin checks on matchup objects ----

#Check whether objects aqua and viirs exist

if (!exists("AQUA")) {
  Log.error("Aqua object does not exist.")
}

if (!exists("VIIRS")) {
  Log.error("VIIRS object does not exist")
}
# -------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Join aqua and viirs objects into one ----

#Convert VIIRS BTs from kelvin to celsius
pattern = c('cen(.+?)', 'med(.+?)', 'max(.+?)', 'min(.+?)')
pattern = paste(pattern, collapse = '|')
for (uuu in seq(from = 1, to = length(names(VIIRS)), by = 1)) {
  uu1 <- names(VIIRS)[uuu]
  if (grepl(pattern, uu1)) {
    VIIRS[uu1] <- as.celsius(VIIRS[uu1])
  }
}

VIIRS$sst.minus.buoy.sst <- as.celsius(VIIRS$sst.minus.buoy.sst)

#Convert timedates from POSIXct objects to characters
VIIRS$buoy.timedate <- as.character(VIIRS$buoy.timedate)
AQUA$buoy.timedate <- as.character(AQUA$buoy.timedate)

VIIRS$sat.timedate <- as.character(VIIRS$sat.timedate)
AQUA$sat.timedate <- as.character(AQUA$sat.timedate)

#Change column names of VIIRS and AQUA objects so that when we join, there is no ambiguity
colnames(AQUA) <- paste(colnames(AQUA), "AQUA", sep = ".")
colnames(VIIRS) <- paste(colnames(VIIRS), "VIIRS", sep = ".")
#Don't change names of buoy.date or buoy.id as that is what we join by
colnames(AQUA)[1] = "buoy.date"
colnames(AQUA)[6] = "buoy.id"
colnames(VIIRS)[1] = "buoy.date"
colnames(VIIRS)[7] = "buoy.id"

#Join VIIRS and AQUA into one object
#orig_j <- dplyr::inner_join(AQUA, VIIRS, by = c("buoy.pftime", "buoy.lon", "buoy.lat", "buoy.id"))

orig_j <- dplyr::inner_join(AQUA, VIIRS, by = c("buoy.date", "buoy.id"))

#Convert timedates back from characters to POSIXct objects
orig_j$buoy.timedate.AQUA <- lubridate::ymd_hms(orig_j$buoy.timedate.AQUA)
orig_j$buoy.timedate.VIIRS <- lubridate::ymd_hms(orig_j$buoy.timedate.VIIRS)

orig_j$sat.timedate.AQUA <- lubridate::ymd_hms(orig_j$sat.timedate.AQUA)
orig_j$sat.timedate.VIIRS <- lubridate::ymd_hms(orig_j$sat.timedate.VIIRS)


#Save joint object
save(orig_j, file = "/home/ckk/Projects/Matchup_R_Scripts/Results/objects/Joined_AQUA_VIIRS_Matchups_No_Filtering.Rdata")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Filtering orig_j ----

#Filter by solz, platform, time difference
orig_solz_platform_time <- dplyr::tbl_df(orig_j) %>%
  dplyr::filter(solz.AQUA >= 90 & solz.VIIRS >= 90) %>% #Allow only nighttime retrievals
  dplyr::filter(insitu.platform.AQUA != "Ship") %>% #Allow only buoy retrievals
  dplyr::filter(as.numeric(abs(sat.timedate.AQUA - sat.timedate.VIIRS)) <= 3600) %>% #VIIRS and AQUA passover times must be less than 1hr
  dplyr::filter(as.numeric(abs(buoy.timedate.AQUA - buoy.timedate.VIIRS)) <= 300) %>% #Buoy retrievals for VIIRS and AQUA must be less than 5 min
  dplyr::filter(geosphere::distVincentyEllipsoid(as.matrix(data.frame(sat.lon.AQUA, sat.lat.AQUA)), #Allow only retrievals with distance between sat and buoy less than 1 km
    as.matrix(data.frame(buoy.lon.AQUA, buoy.lat.AQUA))) <= 1000) %>%
  dplyr::filter(geosphere::distVincentyEllipsoid(as.matrix(data.frame(sat.lon.VIIRS, sat.lat.VIIRS)),
    as.matrix(data.frame(buoy.lon.VIIRS, buoy.lat.VIIRS))) <= 1000)

save(orig_solz_platform_time, file = "/home/ckk/Projects/Matchup_R_Scripts/Results/objects/Joined_AQUA_and_VIIRS_Matchups_filtered_by_solz_platform_and_timediff.Rdata")

#Add filtering by quality
orig_filtered <- orig_solz_platform_time %>%
  dplyr::filter(qsst.AQUA == 0 & qsst.VIIRS == 0)

save(orig_filtered, file = "/home/ckk/Projects/Matchup_R_Scripts/Results/objects/Joined_AQUA_and_VIIRS_Matchups_completely_filtered_including_qsst.Rdata")

#Summary of buoy ssts to ensure that VIIRS and AQUA buoy retrievals aren't very different

#In orig without quality filtering
#AQUA
summary(orig_solz_platform_time$buoy.sst.AQUA)
#VIIRS
summary(orig_solz_platform_time$buoy.sst.VIIRS)

#In orig with quality filtering
#AQUA
summary(orig_filtered$buoy.sst.AQUA)
#VIIRS
summary(orig_filtered$buoy.sst.VIIRS)

#De-bias buoy SSTs to match the type of SST MODIS/VIIRS and the buoys gather
orig_solz_platform_time$buoy.sst.AQUA = orig_solz_platform_time$buoy.sst.AQUA - .17
orig_solz_platform_time$buoy.sst.VIIRS = orig_solz_platform_time$buoy.sst.VIIRS - .17

orig_filtered$buoy.sst.AQUA = orig_filtered$buoy.sst.AQUA - .17
orig_filtered$buoy.sst.VIIRS = orig_filtered$buoy.sst.VIIRS - .17
#Tables showing spread of qualities for the different sensors
xtabs(~ qsst.AQUA + qsst.VIIRS, data = orig_j)

xtabs(~ qsst.AQUA + qsst.VIIRS, data = orig_filtered)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Build a SpatialPoints object with lons and lats of matchups ----
matchup.coords <- as.matrix(cbind(lon = orig_filtered[, 'buoy.lon.AQUA'],
  lat = orig_filtered[, 'buoy.lat.AQUA']))

crs.string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

pts <- sp::SpatialPoints(coords = matchup.coords,
  proj4string = sp::CRS(crs.string))

pts <- as.data.frame(pts)
pts <- data.frame(pts, orig_filtered$insitu.platform.AQUA, orig_filtered$buoy.id)
sp::coordinates(pts) <- ~ buoy.lon.AQUA + buoy.lat.AQUA
sp::proj4string(pts) <- sp::CRS(crs.string)

mapview::mapview(pts, alpha = .2, cex = 1)
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------#
# --- Do regressions for each set of obsesrvations ----
#Trying to predict aqua residuals based on viirs residuals
fit_aquaviirsresiduals <- lm(orig_filtered$sst.minus.buoy.sst.AQUA ~ orig_filtered$sst.minus.buoy.sst.VIIRS)

#Try to predict SST in one observation type from another observation type
fit_aquaviirstsst <- lm(orig_filtered$cen.sst.AQUA ~ orig_filtered$cen.sst.VIIRS)

fit_aquabuoysst <- lm(orig_filtered$cen.sst.AQUA ~ orig_filtered$buoy.sst.AQUA)

fit_viirsbuoysst <- lm(orig_filtered$)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Conduct collocation experiments on different populations
#Maybe we can conduct experiments on the regimes we identified last year to see if we can show a second way
#that regimes we identified actually had different residual characteristics than the rest of the the globe???

#Global experiment

#
# ------------------------------------------------------------------------------









# --- Workspace ----

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


#Testing collocation functions
truthx <- seq(1, 1000)

epsi1 <- rnorm(1000, 0, 1)
epsi2 <- rnorm(1000, 0, 2)
epsi3 <- rnorm(1000, 0, 3)

alpha1 <- 5

alpha2 <- 3

alpha3 <- 7

meas1 <- truthx + alpha1 + epsi1

meas2 <- truthx + alpha2 + epsi2

meas3 <- truthx + alpha3 + epsi3





biases <- as.data.frame(t(t(as.matrix(bias(aqua, viirs, buoy))) %*% bias.coefs))


tt1 <- (as.numeric(orig_solz_platform_time$sat.timedate.AQUA - orig_solz_platform_time$sat.timedate.VIIRS) > 3000)