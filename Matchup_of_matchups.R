# --- This script creates a matchup of matchups for a collocation analysis.
# --- dplyr is used to join AQUA and VIIRS matchups and the result is
# --- filtered. The resulting dataframe has only nighttime buoy retrievals,
# --- where buoy retrievals are less than five minutes apart and satellite
# --- retrievals are less than 1 hour apart.

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
bias <- function(x, y, z) {
  if (!is.null(nrow(x)) | !is.null(nrow(y)) | !is.null(nrow(z)) | length(x) <= 1 | length(y) <= 1 | length(z) <= 1) {
    stop("Only vectors can be passed as arguments in the bias function.")
  }
  #Bias between x and y
  bias_xy <- mean(x) - mean(y)
  #Bias between x and y
  bias_xz <- mean(x) - mean(z)
  #Bias between y and z
  bias_zy <- mean(z) - mean(y)
  #Return a dataframe with all biases
  biases <- data.frame(bias_xy, bias_xz, bias_zy)
  colnames(biases) <- c("bias_xy", "bias_xz", "bias_zy")
  return(biases)
}
#TO DO: CREATE MORE DESCRIPTIVE VARIABLE NAMES (i.e aq, vi, b) 
#Variance
variance <- function(x, y, z) {
  if (!is.null(nrow(x)) | !is.null(nrow(y)) | !is.null(nrow(z)) | length(x) <= 1 | length(y) <= 1 | length(z) <= 1) {
    stop("Only vectors can be passed as arguments in the variance function.")
  }
  #Variance of differences in x and y
  var_x <- var(x)
  var_y <- var(y)
  r_xy <- cor(x, y)
  variance_xy <- var_x + var_y - 2*r_xy*sqrt(var_x)*sqrt(var_y)
  #Variance of differences in z and x
  var_x <- var(x)
  var_z <- var(z)
  r_xz <- cor(x, z)
  variance_xz <- var_x + var_z - 2*r_xz*sqrt(var_x)*sqrt(var_z)
  #Variance of differences in y and z
  var_z <- var(z)
  var_y <- var(y)
  r_zy <- cor(z, y)
  variance_zy <- var_z + var_y - 2*r_zy*sqrt(var_z)*sqrt(var_y)
  #Variance of x
  variance_x <- .5*(variance_xy + variance_xz - variance_zy) +
    (r_xy*sqrt(var_x)*sqrt(var_y) + r_xz*sqrt(var_x)*sqrt(var_z) - r_zy*sqrt(var_y)*sqrt(var_z))
  #Variance of y
  variance_xy <- .5*(variance_zy + variance_xy - variance_xz) +
    (r_zy*sqrt(var_z)*sqrt(var_y) + r_xy*sqrt(var_x)*sqrt(var_y) - r_xz*sqrt(var_x)*sqrt(var_z))
  #Variance of z
  variance_xz <- .5*(variance_xz + variance_zy - variance_xy) +
    (r_xz*sqrt(var_x)*sqrt(var_z) + r_zy*sqrt(var_z)*sqrt(var_y) - r_xy*sqrt(var_x)*sqrt(var_y))
  variances <- data.frame(variance_xy, variance_xz, variance_zy)
  colnames(variances) <- c("variance_xy", "variance_xz", "variance_zy")
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

matchup.coords <- as.matrix(cbind(lon = orig_filtered[, 'sat.lon.AQUA'],
  lat = orig_filtered[, 'sat.lat.AQUA']))

crs.string <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

pts <- sp::SpatialPoints(coords = matchup.coords,
  proj4string = sp::CRS(crs.string))

mapview::mapview(pts)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Conduct Collocation Experiments on Different Populations
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
x <- rnorm(100, 10, 1)
y <- rnorm(100, 25, 5)
z <- rnorm(100, 50, 10)

biases <- bias(x, y, z)

variances <- variance(x, y, z)





tt1 <- (as.numeric(orig_solz_platform_time$sat.timedate.AQUA - orig_solz_platform_time$sat.timedate.VIIRS) > 3000)