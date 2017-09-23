# ----------------------------------------------------------------------------------------
# --- Script to re-estimate "latband1" SST algorithm coefficients
# --- for MODIS matchups AFTER identifying ADDITIONAL algorithm terms needed
# --- (e.g., mirror side correction, asymetric satelite zenith angle effect).
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Implement secant function where input x is in degrees -----

library(circular)
secant.deg <- function(x) {1 / (cos(rad(x)))}

#test <- seq(-60, 60, 0.5)
#plot(test, secant.deg(test), type="l")
#plot(test, secant.deg(test) - 1, type="l")
#rm(test)
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Assemble a data frame with variables to be used in regressions ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(
  bsst    = orig$buoy.sst,                      # Buoy SST
  ch11000 = orig$cen.11000,                     # Channel 31 brightness temperature
  BTdiff  = (orig$cen.11000 - orig$cen.12000),  # BT difference MODIS channels 31 and 32
  satz    = orig$satz,                          # Satellite zenith angle
  latband = orig$latband,                       # Latitude band
  mon     = orig$sat.mon,                       # Month
  mirror  = factor(orig$mirror,
    levels=c(1,2), labels=c("side1", "side2"))) # Mirror side (factor)

gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Compute latband1 SSTs for all matchups ----
# --- Fetch the SST coefficients corresponding to each month and latitude band
# --- Note: Many of the steps in this section are necessary because
# --- function merge() seems to get rows out of order, and satellite
# --- quantities and coefficients do not align.

# --- Regenerate object with SST latband1 algorithm coefficients
# --- to be used in calculation of SST values.
# --- NOTE that the constant (coef1) already has
# --- the skin temperature correction incorporated.

flog.info(paste("Computing SST for", config$sensor), name = 'ml')

if (config$sensor == "Aqua") {
  coeffs.SST.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Aqua/collection_6/coeffs/AQUA_collection_6_SST_latband1_coeffs_v6.3_dput_final.txt")
} else if (config$sensor == "Terra") {
  coeffs.SST.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Terra/collection_6/coeffs/TERRA_collection_6_SST_latband1_coeffs_v6.3_dput_final.txt")
}

orig2 <- orig   # Create new working object "orig2"

# --- Add numeric values for mon and latband to "orig2"

orig2 <- data.frame(orig2, mon=orig2$sat.mon, lat=as.numeric(orig2$latband))

# --- Merge the orig2 data frame with the coefficients data frame
# --- Note we are using the join() command un the dplyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame toe be merged.
# --- Nevertheless, because of the way we do things, the result should be cooorect.
# --- For this reason, there shpould not be a need to use

aa2 <- dplyr::left_join(orig2, coeffs.SST.latband1.df, by=c("mon","lat"))

# --- Verify that we have a single SST coefficient value for each month/latband combination

ttt <- tapply(aa2$coeff1, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff2, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff3, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff4, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff5, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff6, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

ttt <- tapply(aa2$coeff7, INDEX=list(aa2$mon, aa2$lat), FUN=function(x) {length(unique(x))})
if (any(ttt != 1))
  flog.error("There is more than one coefficient value per month and latband", name = 'ml')

rm(ttt); gc()


# --- Extract vectors of coefficients

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4
coef5 <- aa2$coeff5
coef6 <- aa2$coeff6
coef7 <- aa2$coeff7

# --- Extract the independent variable used in computing SST

x1 <- aa2$cen.11000  										  # BT31 (brigthness temperature for channel 31)
x2 <- aa2$cen.11000 - aa2$cen.12000			  # BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz											      # satellite zenith angle (no sign)
x5 <- x2 * x3														  # BT31-BT32 * buoy SST
x6 <- (secant.deg(aa2$satz) - 1) * x2     # Secant times BT31-BT32
x7 <- ifelse(aa2$mirror == 1, 0, 1)       # Dummy mirror variable 0 for side1, 1 for side2

# --- Compute "latband1" SSTs and residuals for ALL the data.

SST.latband1 <- coef1 + (coef2 * x1) + (coef3 * x5) + (coef4 * x6) +
  (coef5 * x7) + (coef6 * x4) + (coef7 * (x4 ^ 2))

# --- Compute "latband1" SST residuals as (satellite minus buoy).
# --- NOTE: Be careful to use object "aa2",
# --- otherwise records may be misaligned with "orig".

SST.latband1.res <- SST.latband1 - aa2$buoy.sst		# SST residuals

# --- Compute SST residuals with respect to REFERENCE SST

SST.latband1.refres <- SST.latband1 - aa2$ref.type.1.SST

# --- Replace previously-estimated latband SST and residuals in data frame "aa2".

aa2$SST.latband1 <- SST.latband1 						# Replace preliminary SST values
aa2$SST.latband1.res <- SST.latband1.res 		# Replace preliminary residual values
aa2$SST.latband1.refres <- SST.latband1.refres # Include residuals computed with REF SST

# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("mon","lat","coeff1","coeff2","coeff3","coeff4","coeff5",
  "coeff6","coeff7","N","rsq","sig.red") 	# Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[, tt3]		# Original object with SST latband1 and residuals added

# --- Plots of SST and SST residuals vs. buoy

use <- orig$use.4.coeffs.SST == TRUE &
  !is.na(orig$SST.latband1.res) &
  (orig$SST.latband1.res + 0.17) >= -2.0 &
  (orig$SST.latband1.res + 0.17) <= 2.0

smoothScatter(orig$buoy.sst[use],
  orig$SST.latband1[use],
  xlab="Buoy SST",
  ylab="Satellite SST",
  main=paste0(config$sensor," - Final SST"),
  nbin=60)
abline(-0.17, 1)

smoothScatter(orig$buoy.sst[use],
  orig$SST.latband1.res[use],
  xlab="Buoy SST",
  ylab="Satellite SST Residuals",
  main=paste0(config$sensor," - Final SST residuals"),
  nbin=60)
abline(h=-0.17)

rm(x1,x2,x3,x4,x5,x6,x7,coef1,coef2,coef3,coef4,coef5,coef6,coef7,tt1,tt2,tt3)
rm(SST.latband1, SST.latband1.res, SST.latband1.refres,aa2,orig2,reg.input,i); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- FINALLY, SAVE INPUT DATA FRAME "ORIG" to a backup data frame and abinary file ----
# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (config$sensor == "Terra") {
  TERRA <- orig 			# Store object "orig" in object "TERRA"
} else if (config$sensor == "Aqua") {
  AQUA <- orig 				# Store object "orig" in object "AQUA"
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Export a text file with processed data frame ----
# --- Includes all variables for a sensor.

# --- First, convert buoy and satellite dates/times to character vectors
# --- so that they are written out surrounded by quotes and thus can be read in
# --- as a single string.

orig.write <- orig   	# Create bogus object "orig.write" that will be written out

orig.write$buoy.timedate <- as.character(orig.write$buoy.timedate)
orig.write$sat.timedate <- as.character(orig.write$sat.timedate)

out.data.file <- paste(results.outdir,
  config$sensor, "_matchups_",
  config$matchups$version,
  "_dataframe.txt", sep="")

write.table(orig.write, file=out.data.file, append=FALSE,
  sep=" ", na="NA", row.names=FALSE, col.names=TRUE, quote=TRUE)
gc()

# --- Also write out a text file using dput() that
# --- can be easily regenerated

out.data.file2 <- paste(results.outdir,
  config$sensor, "_matchups_",
  config$matchups$version,
  "_dputoutput.txt", sep="")

dput(orig, file=out.data.file2)
gc()

# uuu <- dget(out.data.file2)

rm(out.data.file, out.data.file2, orig.write); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Clean up most objects   ----
# --- EXCEPT those objects equal to "orig" or starting with string "AQUA" or "TERRA"

tt1 <- objects()
tt2a <- str_detect(tt1, "^orig$")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt2d <- str_detect(tt1, "^config")

tt3 <- tt2a | tt2b | tt2c | tt2d
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt2d,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------





