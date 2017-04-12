# ---------------------------------------------------------------------------------------------
# --- Script to compute "latband1" SST4 values.
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Implement secant function where input x is in degrees -----

secant.deg <- function(x) {1 / (cos(rad(x)))}

#test <- seq(-60, 60, 0.5)
#plot(test, secant.deg(test), type="l")
#plot(test, secant.deg(test) - 1, type="l")
#rm(test)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Assemble a data frame with variables to be used in SST4 calculations ----
# --- DO NOT filter by training records at this point.

reg.input <- data.frame(
  bsst       = orig$buoy.sst,                     # Buoy SST
  ch3959     = orig$cen.3959,                     # BT for MODIS channel 22
  BTdiff     = (orig$cen.3959 - orig$cen.4050),   # BT difference of channels 22 and 23
  satz       = orig$satz,                         # Satellite zenith angle
  latband    = orig$latband,                      # Latitude band
  mon        = orig$sat.mon,                      # Month
  mirror     = factor(orig$mirror,
    levels=c(1,2), labels=c("side1", "side2")))   # Mirror side (factor)

# --- Check rows that have one or more missing values

#tt1 <- apply(reg.input, MARGIN=1, FUN=function(x) {any(is.na(x))})
#table(tt1)

#reg.input <- na.omit(reg.input)     # Select rows without missing values
#rm(tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- COMPUTE LATBAND1 SST4s FOR ALL MATCHUPS ----
# --- Fetch the SST4 coefficients corresponding to each month and latitude band
# --- Note: Many of the steps in this section are necessary because
# --- function merge() seems to get rows out of order, and satellite
# --- quantities and coefficients do not align.

flog.info(paste("Computing SST4 for", config$sensor), name = 'ml')

if (config$sensor == "Aqua") {
  coeffs.SST4.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Aqua/collection_6/coeffs/AQUA_collection_6_SST4_latband1_coeffs_v6.3_dput_final.txt")
} else if (config$sensor == "Terra") {
  coeffs.SST4.latband1.df <- dget("D:/Matchups/MODIS/results/version_6/Terra/collection_6/coeffs/TERRA_collection_6_SST4_latband1_coeffs_v6.3_dput_final.txt")
}

# --- Add numeric values for mon and latband to "orig2"

orig2 <- data.frame(orig, mon=orig$sat.mon, lat=as.numeric(orig$latband))

# --- Merge the orig2 data frame with the coefficients data frame
# --- Note we are using the join() command un the dplyr library.
# --- Unlike merge(), join() does not change the order of the first data.frame toe be merged.
# --- Nevertheless, because of the way we do things, the result should be cooorect.
# --- For this reason, there shpould not be a need to use

aa2 <- dplyr::left_join(orig2, coeffs.SST4.latband1.df, by=c("mon","lat"))
gc()

# --- Verify that we have a single SST4 coefficient value for each month/latband combination

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

# --- Extract vectors of SST4coefficients

coef1 <- aa2$coeff1
coef2 <- aa2$coeff2
coef3 <- aa2$coeff3
coef4 <- aa2$coeff4
coef5 <- aa2$coeff5
coef6 <- aa2$coeff6
coef7 <- aa2$coeff7

# --- Extract the independent variable used in computing SST4

x1 <- aa2$cen.3959  										  # BT31 (brigthness temperature for channel 31)
x2 <- aa2$cen.3959 - aa2$cen.4050			    # BT31 - BT32
x3 <- aa2$buoy.sst											  # Buoy SST
x4 <- aa2$satz										        # satellite zenith angle
x6 <- (secant.deg(aa2$satz) - 1)	        # Secant [*** NOT times BT31-BT32]
x7 <- ifelse(aa2$mirror == 1, 0, 1)       # Dummy mirror variable 0 for side1, 1 for side2

# --- Compute "latband1" SST4s and residuals for ALL the data.

SST4.latband1 <- coef1 + (coef2 * x1) + (coef3 * x2) + 
  (coef4 * x6) + (coef5 * x7) + (coef6 * x4) + (coef7 * (x4 ^ 2))

# --- Compute "latband1" SST4 residuals as (satellite SST4 minus buoy SST).
# --- NOTE: Be careful to use object "aa2" to subtract buoy SST,
# --- otherwise records may be misaligned with "orig".

SST4.latband1.res <- SST4.latband1 - aa2$buoy.sst  	# SST4 residuals

# --- Compute SST4 residuals with respect to REFERENCE SST

SST4.latband1.refres <- SST4.latband1 - aa2$ref.type.1.SST

# --- Replace previously-estimated latband SST4 and residuals in data frame "aa2".

aa2$SST4.latband1 <- SST4.latband1   				# Add preliminary 2 SST4 values
aa2$SST4.latband1.res <- SST4.latband1.res 	# Add preliminary 2 SST4 residual values
aa2$SST4.latband1.refres <- SST4.latband1.refres # Include residuals computed with REF SST

# --- SST4 values are good only at night (solar zenith angle >= 90)
# --- Set non-night SST4 and SST4 residual values to NA

aa2$SST4.latband1 <- ifelse(aa2$solz >= 90, aa2$SST4.latband1, NA)
aa2$SST4.latband1.res <- ifelse(aa2$solz >= 90, aa2$SST4.latband1.res, NA)
aa2$SST4.latband1.refres <- felse(aa2$solz >= 90, aa2$SST4.latband1.refres, NA)
  
# --- Eliminate unwanted columns from object "aa2"

tt1 <- names(aa2)
tt2 <- c("mon","lat","coeff1","coeff2","coeff3","coeff4","coeff5",
  "coeff6","coeff7","rsq","sig.red","N","P") 	# Columns to eliminate
tt3 <- which(!(tt1 %in% tt2))		# Numbers of columns in aa2 to KEEP in "orig"

# --- Save wanted columns in NEW object "orig"

orig <- aa2[, tt3]		# Original object with SST4 latband1 and SST4 residuals added

rm(x1,x2,x3,x4,x6,x7,coef1,coef2,coef3,coef4,coef5,coef6,coef7,tt1,tt2,tt3)
rm(SST4.latband1, SST4.latband1.res, aa2); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Apply corrections for electronics - ONLY for TERRA ----
# --- ONLY FOR TERRA: fix the shift in SST4 values at the beginning of the
# --- record due to electronics.

# --- Data extracted from MCST site: http://mcst.gsfc.nasa.gov/index.php?section=15 .
# --- The MODIS instrument on TERRA began taking data using side - A electronics.
# --- MODIS-TERRA consistently produced data starting on 24 February 2000 except for a few periods.
# --- Science data was processed using B - side electronics between
# --- October 30, 2000 (data day 2000304) and June 15, 2001 (day 2001166). 
# --- MODIS-TERRA experienced a Power Supply 2 (PS2 = electronics side - B) shutdown anomaly
# --- and did not take science data during June 16, 2001 (day 2001166)
# --- to July 2, 2001 (day 2001183). 
# --- When MODIS recovered, it was commanded to take science mode data
# --- using Power Supply 1 and electronics side A. 

if (config$sensor == "Terra") {
  
  # Build vector with dates of changes in electronics on MODIS-TERRA
  tt1a <- as.Date(floor_date(min(orig$sat.timedate), unit="month"))
  tt1b <- as.Date(floor_date(ymd("2000-10-30"), unit="day"))
  tt1c <- as.Date(floor_date(ymd("2001-07-02"), unit="day"))
  tt1d <- as.Date(ceiling_date(max(orig$sat.timedate), unit="month"))
  tt1 <- c(tt1a,tt1b,tt1c,tt1d)
  
  rm(tt1a,tt1b,tt1c,tt1d); gc()
  
  tt2 <- cut(as.Date(orig$buoy.timedate), breaks=tt1, include.lowest=FALSE,
    labels=c("P1","P2","P3"))
  
  table(tt2, useNA="always")
  
  # --- The values below can be used to correct SST4 values, as the residuals seem to be
  # --- fairly stable around the offset value for each epoch.    
  # --- The difference between the median after July 2001 and the previous
  # --- two epochs is about 0.44  and 0.25 degC, respectively. This value needs to be ADDED to
  # --- SST4 values prior to 2001-07-01 in order to correct them. 
 
  # --- The median of SST residuals for each epoch identified is shown below.
  # Preliminary 1 residuals
  #       P1         P2         P3 
  # -0.6174036 -0.4258075 -0.1723426 
  # Preliminary 2 residuals
  #       P1         P2         P3 
  #-0.6164536 -0.4305070 -0.1710983 
  # FINAL residuals
  #       P1         P2         P3 
  #-0.6164102 -0.4305018 -0.1711626 
  
  tt3 <- c(-0.6164102, -0.4305018, -0.1711626)
  names(tt3) <- c("P1", "P2", "P3")  
  
  # --- Apply SST4 corrections
  
  correct1 <- tt3["P3"] - tt3["P1"]     # Final: 0.4452476
  correct2 <- tt3["P3"] - tt3["P2"]     # Final: 0.2593392
  
  SST4.latband1 <- rep(NA, times=length(orig$SST4.latband1))
  SST4.latband1.res <- rep(NA, times=length(orig$SST4.latband1.res))
  SST4.latband1.refres <- rep(NA, times=length(orig$SST4.latband1.res))
  
  # --- Correct SST4 for period 1: Feb 2000 to 30 October 2000
  uu1 <- which(tt2 == "P1")
  SST4.latband1[uu1] <- orig$SST4.latband1[uu1] + correct1
  SST4.latband1.res[uu1] <- orig$SST4.latband1.res[uu1] + correct1
  SST4.latband1.refres[uu1] <- orig$SST4.latband1.refres[uu1] + correct1
  
  # --- Correct SST4 for period 1: 30 October 2000 to 2 July 2001
  uu2 <- which(tt2 == "P2")
  SST4.latband1[uu2] <- orig$SST4.latband1[uu2] + correct2
  SST4.latband1.res[uu2] <- orig$SST4.latband1.res[uu2] + correct2
  SST4.latband1.refres[uu2] <- orig$SST4.latband1.refres[uu2] + correct1
  
  # --- Correct SST4 for period 3: 2 July 2001 to end of record (no correction!)
  uu3 <- which(tt2 == "P3")
  SST4.latband1[uu3] <- orig$SST4.latband1[uu3] + 0.0
  SST4.latband1.res[uu3] <- orig$SST4.latband1.res[uu3] + 0.0
  SST4.latband1.refres[uu3] <- orig$SST4.latband1.refres[uu3] + correct1
  
  # --- Put corrected SST4 values in "orig"
  
  orig$SST4.latband1 <- SST4.latband1
  orig$SST4.latband1.res <- SST4.latband1.res
  orig$SST4.latband1.refres <- SST4.latband1.refres
  
  rm(SST4.latband1, SST4.latband1.res, SST4.latband1.refres, tt2, tt3, uu1,uu2,uu3); gc()
 
} # End of code to adjust for electronics issues on TERRA
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- FINALLY, SAVE INPUT DATA FRAME "ORIG" to a backup data frame and abinary file ----
# ---------------------------------------------------------------------------------------#
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (config$sensor == "Terra") {
  TERRA <- orig   		# Store object "orig" in object "TERRA"
} else if (config$sensor == "Aqua") {
  AQUA <- orig 				# Store object "orig" in object "AQUA"
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Export a text file with processed data frame ----
# --- Includes all variables for a sensor.

# --- Directory where processed data.frame will be placed
results.outdir <- paste0("D:/Matchups/MODIS/results/",
  "version_", config$matchups$version, "/",
  config$sensor, "/collection_",
  config$collection,
  "/results/") 

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
rm(list = tt4)

rm(tt1,tt2a,tt2b,tt2c,tt2d,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------


