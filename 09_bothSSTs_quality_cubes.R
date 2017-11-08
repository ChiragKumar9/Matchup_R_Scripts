# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- Script to calculate hypercubes with SST residual statistics
# --- This calculates statistics for the MAIN SST on a sensor
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

Log.info('Calculating hypercubes...')

config$geophys.var <- "SST2b" # SST to be used

# -----------------------------------------------------------------------------#
# --- Install required R packages ----

if (!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(maps)) {install.packages("maps"); library(maps)}
if (!require(magrittr)) {install.packages("magrittr"); library(magrittr)}
if (!require(raster)) {install.packages("raster"); library(raster)}
if (!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
if (!require(timeDate)) {install.packages("timeDate"); library(timeDate)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(yaml)) {install.packages("yaml"); library(yaml)}
if (!require(futile.logger)) {install.packages("futile.logger"); library(futile.logger)}
if (!require(readr)) {install.packages("readr"); library(readr)}
if (!require(tidyr)) {install.packages("tidyr"); library(tidyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Source common functions ----

source(file = paste0(config$functions_dir, 'common_functions.R'),
  local = FALSE, echo = FALSE, verbose = FALSE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Make sure we have an object called 'orig' -----

# --- The script assumes we have an object called 'orig'
# --- with matchups for the sensor we want to analyze.
# --- If one is not present, check if we can

if (exists('orig')) {
  Log.info('Object \'orig\' exists')
} else {
  # Recreate object orig from saved binary data
  Log.warn("Object \'orig\' does NOT exist")
  objdir <- paste0(config$results_dir,'objects/')
  
  # There may be an object saved... pick one
  # saved.objs <- list.files(path = objdir, pattern = "*.Rdata$")
  # orig <- load(saved.objs[4]) # Change as needed
}
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- WORK ON SST RESIDUALS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 1. Select day/night or only night records ----

# --- Define Day or Night:  
# --- Use 90 degrees as the solar zenith angle threshold separating day and night.
# --- This is for MODIS and VIIRS processing (80 degrees is used for AVHRR).

if (config$sensor == "VIIRS" | config$sensor == "MODIS") {
  nite.threshold <- 90  # Threshold solar zenith angle used to separate day/nite
} else if (config$sensor == "AVHRR") {
  nite.threshold <- 80 # Note different threshold for AVHRR (80 degrees)
} else {
  stop("Error in sensor name. Check configuration file (YML)")
}

# --- Define "day" or "night

day.or.nite <- ifelse(orig$solz >= nite.threshold, "night", "day")
day.or.nite <- ordered(day.or.nite, levels = c("day", "night"),
  labels = c("day", "night"))
orig$day.or.nite <- day.or.nite # Add day or night to 'orig' tibble

table(orig$day.or.nite, useNA = 'always')

# --- Plot proportion of day or night matchups

barplot(prop.table(xtabs(~ orig$day.or.nite)),
  names = names(table(orig$day.or.nite)),
  main = paste(config$sensor, "- Matchups by Day/Night"),
  xlab = "Proportion of Matchups",
  col = "lemonchiffon2", horiz = TRUE, las = 1)
box()

# --- NOTE: For long IR SST (Modis) and SST2b (VIIRS),
# --- hypercube statistics are calculated for both day and night.
# --- Instead, we do not calculate SST4 (MODIS) or SST3b (VIIRS) statistics
# --- for daytime, as there may be contamination in the 3.7 um channel.
# --- If working on SST4 or SST3b, select records for night time only.

if (config$geophys.var == "SST4" | config$geophys.var == "SST3b") {
  # Eliminate daytime records for MWIR SSTs
  orig2 <- dplyr::tbl_df(orig) %>%
    dplyr::filter(day.or.nite == 'night') %>%
    droplevels()
} else if (config$geophys.var == "SST" | config$geophys.var == "SST2b") {
  # Work with day AND night records
  orig2 <- dplyr::tbl_df(orig)
} # End of check for SST4 or SST3b (mid-IR SSTs for MODIS and VIIRS respectively)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 2. Define variable to be used as SST residuals -----

# MODIS LWIR SST residuals
if ((config$sensor == "MODIS") & (config$geophys.var == "SST")) {
  sst.resid <- orig2$sst.minus.buoy.sst
}

# MODIS MWIR SST residuals
if ((config$sensor == "MODIS") & (config$geophys.var == "SST4")) {
  sst.resid <- orig2$sst4.minus.buoy.sst
}

# VIIRS LWIR SST residuals
if ((config$sensor == "VIIRS") & (config$geophys.var == "SST2b")) {
  sst.resid <- orig2$sst.minus.buoy.sst
}

# VIIRS MWIR SST residuals
if ((config$sensor == "VIIRS") & (config$geophys.var == "SST3b")) {
  sst.resid <- orig2$sst3b.minus.buoy.sst
}

Log.info(paste("Using SST residuals for",
  config$sensor,
  config$algorithm$type,
  config$geophys.var, "..."))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 3. Define SST quality levels for which hypercube stats will be calculated ----

Log.info(paste("Looking at quality values for",
  config$sensor, config$algorithm$type, config$geophys.var, "..."))

# --- MODIS LWIR SST
if ((config$sensor == "MODIS") & (config$geophys.var == "SST")) {
  if (config$algorithm$type == "latband1") {
    orig2$qsst.unif <- ordered(orig2$qsst.new)
    quality.levels <- sort(unique(orig2$qsst.new))	# Quality levels to analyze
    orig2$resid.unif <- orig2$sst.minus.buoy.sst
  } else if (config$algorithm$type == "V5") {
    orig2$qsst.unif <- ordered(orig2$qsst)
    quality.levels <- sort(unique(orig2$qsst))	    # Quality levels to analyze
    orig2$resid.unif <- orig2$sst.minus.buoy.sst
  }
}

# --- MODIS MWIR SST
if ((config$sensor == "MODIS") & (config$geophys.var == "SST4")) {
  if (config$algorithm$type == "latband1") {
    orig2$qsst.unif <- ordered(orig2$qsst4.new)
    quality.levels <- sort(unique(orig2$qsst4.new))	  # Quality levels to analyze
    orig2$resid.unif <- orig2$sst4.minus.buoy.sst
  } else if (config$algorithm$type == "V5") {
    orig2$qsst.unif <- ordered(orig2$qsst4)
    quality.levels <- sort(unique(orig2$qsst4))	  # Quality levels to analyze
    orig2$resid.unif <- orig2$sst4.minus.buoy.sst
  }
}

# --- VIIRS LWIR SST
if ((config$sensor == "VIIRS") & (config$geophys.var == "SST2b")) {
  orig2$qsst.unif <- ordered(orig2$qsst)
  quality.levels <- sort(unique(orig2$qsst))	  # Quality levels to analyze
  orig2$resid.unif <- orig2$sst.minus.buoy.sst
}

# --- VIIRS MWIR SST
if ((config$sensor == "VIIRS") & (config$geophys.var == "SST3b")) {
  orig2$qsst.unif <- ordered(orig2$qsst3b)
  quality.levels <- sort(unique(orig2$qsst3b))	  # Quality levels to analyze
  orig2$resid.unif <- orig2$sst3b.minus.buoy.sst
}

table(quality.levels)
table(orig2$qsst.unif)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 4. ADD "skin" bias to SST residuals ----
# --- This will make the residuals be referenced to a skin depth.
# Suppose the SST algorithm and buoy measurements were both perfect.
# In this case, each residual should be -0.17 because
# SST resid = skin satellite SST - buoy SST and
# whatever the SST, the satellite would always be 0.17C cooler.

# From GHRSST User manual (sent by Kay 1 July 2017):  
# The application of the SSES bias provided in the L2P product maintains the
# depth of the satellite SST observation.
# So, if the satellite SST is SSTskin, then the application of the SSES bias
# will provide an improved SSTskin relative to the reference. 

# --- Skin bias is 0.17 for all IR radiometers and wavelengths.
# --- This bias means that the skin is, on average, 0.17 COLDER than
# --- buoy (bulk) SSTs.

skin.offset <- 0.17 	# Skin SSTs are 0.17 degrees COLDER than "bulk" SSTs

Log.info(paste("Adding skin bias to residuals for",
  config$sensor, 
  config$algorithm$type,
  config$geophys.var, "..."))

# Add skin offset to SST residuals
SST.res <- sst.resid + skin.offset	  # SST residuals PLUS skin offset

rm(skin.offset)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- DEFINE FACTORS FOR EACH VARIABLE USED TO DEFINE BINS IN THE HYPERCUBE
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

Log.info('Defining factors for variables in hypercube...')

# -----------------------------------------------------------------------------#
# --- 5. Quarters of the year ----

tt1 <- lubridate::quarter(orig2$sat.timedate, with_year = FALSE)

quarter.of.yr <- ordered(tt1,
  levels = unique(tt1),
	labels = c("Q1","Q2","Q3","Q4"))

table(quarter.of.yr, useNA = 'always') # Table of matchups by quarter

# --- Check quarter values are OK 
check <- orig2 %>%
  dplyr::mutate(qtr = lubridate::quarter(sat.timedate))  %>%
  dplyr::group_by(qtr) %>%
  dplyr::summarize(min = min(sat.timedate),
    max = max(sat.timedate))

barplot(prop.table(xtabs(~ quarter.of.yr)), names=names(table(quarter.of.yr)),
	main = paste(config$sensor,"- Matchups by Quarter of the Year"),
	xlab = "Proportion of Matchups",
	col = "lemonchiffon2", horiz = TRUE, las = 1)
box()

rm(tt1); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 6. Latitude intervals ----

check <- tapply(orig2$buoy.lat, INDEX = orig2$latband,
	FUN = range, simplify = TRUE)

barplot(prop.table(xtabs(~ orig2$latband)),
  names.arg = levels(orig2$latband),
	main = paste(config$sensor,"- Matchups by Latitude Interval"),
	xlab = "Proportion of Matchups",
	col = "lemonchiffon2", horiz = TRUE, las = 1, cex.names = 0.7)
box()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 7. Buoy SST intervals ----

bsstint <- ordered(cut(orig2$buoy.sst,
	breaks = c(-2, 3, 8, 13, 18, 23, 28, max(orig2$buoy.sst, na.rm = TRUE) + 0.1),
	labels = c("-2 to 3C","3+ to 8C","8+ to 13C","13+ to 18C",
	"18+ to 23C","23+ to 28C", "> 28C"), 
	include.lowest = TRUE))

check <- tapply(orig2$buoy.sst,
	INDEX = bsstint,
	FUN = range, simplify = TRUE)

barplot(prop.table(xtabs(~ bsstint)), names.arg=levels(bsstint),
	main = paste(config$sensor,"- Matchups by Buoy SST Interval"),
	xlab = "Proportion of Matchups",
	col = "lemonchiffon2", horiz = TRUE, cex.names = 0.7, las = 1)
box()

quantile(orig2$buoy.sst, probs=c(0, 0.20, 0.40, 0.50, 0.60, 0.80, 1))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 8. Satellite zenith angle intervals ----
# --- NOTE: This considers only ABSOLUTE values of zenith angle

satzint <- ordered(cut(abs(orig2$satz),
	breaks=c(0, 30, 40, 50, max(abs(orig2$satz)) + 5),
	labels=c("0 to 30 deg","30+ to 40 deg","40+ to 50 deg","50+ deg"), 
	include.lowest=TRUE))

check <- tapply(abs(orig2$satz), INDEX = satzint, FUN = range, simplify = TRUE)
				
barplot(prop.table(xtabs(~ satzint)), names.arg = levels(satzint),				
	main = paste(config$sensor,"- Matchups by Satellite Zenith Angle Interval"),
	xlab = "Proportion of Matchups",
	col = "lemonchiffon2", horiz = TRUE, cex.names = 0.5, las = 1)
box()

quantile(abs(orig2$satz), probs = seq(from = 0, to = 1, by = 0.2))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 9. Channel differences ----

# --- MODIS or VIIRS LWIR SST (SST for MODIS, SST2b for VIIRS)
# --- 11 micron BT minus 12 micron BT intervals

if (((config$sensor == "MODIS") | (config$sensor == "VIIRS")) &
    ((config$geophys.var == "SST") | (config$geophys.var == "SST2b"))) {
  
  uu0 <- (orig2$cen.11000 - orig2$cen.12000)
  
  # --- First we create a boxplot of SST residuals 
  # --- using small intervals of BT11 - BT12
  # --- in order to select appropriate bin breaks.
  
  BTdiff.int <- ordered(cut(uu0,
    breaks = c(min(uu0, na.rm=T) - 0.1,
      seq(from = 0, to = (max(uu0, na.rm = TRUE)+ 0.1), by = 0.5))))
  
  table(BTdiff.int, useNA = 'always')
  
  boxplot(split(orig2$resid.unif, BTdiff.int),
    main = paste(config$sensor, "- BT11 - BT12 microns"),
    xlab = "BT11 - BT12",
    ylab = "SST Residuals")
  abline(v = c(1.5, 2.5, 3.5, 8.5))
  
  # --- Now let us define FOUR (BT11 - BT12) intervals, create a factor
  
  BTdiff.int <- ordered(cut(uu0,
    breaks = c(min(uu0, na.rm = T) - 0.1, 0.0, 0.5, 3.5, max(uu0, na.rm=TRUE) + 0.1),
    include.lowest = TRUE),
    labels = c("<= 0 deg", "0+ to 0.5 deg", "0.5+ to 3.5 deg", "> 3.5 deg"))
  
  table(BTdiff.int, useNA = 'always')
  
  check <- tapply(uu0, INDEX = BTdiff.int, FUN = range, simplify = TRUE)
  
  barplot(prop.table(xtabs(~ BTdiff.int)), names.arg = levels(BTdiff.int),				
    main = paste(config$sensor,"- Matchups by 11 micron - 12 micron BT Interval"),
    xlab = "Proportion of Matchups",
    col = "lemonchiffon2", horiz = TRUE, cex.names = 0.8, las = 1)
  box()
  
  rm(uu0); gc()
} # End of BT difference calculation for LWIR SSTs

# --- MODIS MWIR SST (SST4)
# --- We use standardized (BT39 - BT40) intervals (used for SST4)
# --- We use the difference between channels 22 - 23 corrected by sza.

if (config$sensor == "MODIS" & config$geophys.var == "SST4") {
  
  BTdiff.int <- ordered(cut(orig2$cen.39.40.ref.new,
    breaks=c(min(uu0, na.rm=T)-0.01, -0.5, 0.0, 0.5, max(uu0, na.rm=TRUE)+0.01),
    labels=c("< -0.5C", "-0.5+ to 0.0C", "0.0+ to 0.5C", ">0.5C"),
    include.lowest=TRUE))
  
  check <- tapply(uu0, INDEX = BTdiff.int, FUN = range, simplify = TRUE)
  
  barplot(prop.table(xtabs(~ BTdiff.int)), names.arg=levels(BTdiff.int),				
    main = paste(config$sensor,"- Matchups by BT3.9-BT4.0 Std. Diff. Intval"),
    xlab = "Proportion of Matchups",
    col = "lemonchiffon2", horiz = TRUE, cex.names = 0.7, las = 1)
  box()
  
} # End of BT difference calculation for MODIS MWIR SST (SST4)
  
# --- VIIRS MWIR SST (SST3B)
# --- We use the difference between channels at 3.7 and 12 micrometers,
# --- VIIRS channels 12 and 16, respectively.

if (config$sensor == "VIIRS" & config$geophys.var == "SST3b") {
  
  uu0 <- (orig2$cen.3750 - orig2$cen.12000)
  
  # --- First we create a boxplot of SST residuals 
  # --- using small intervals of BT3.7 - BT12
  # --- in order to select appropriate bin breaks.
  
  BTdiff.int <- ordered(cut(uu0,
    breaks = c(min(uu0, na.rm=T) - 0.1,
      seq(from = 0, to = (max(uu0, na.rm = TRUE)+ 0.1), by = 1.0))))
  
  table(BTdiff.int, useNA = 'always')
  
  boxplot(split(orig2$resid.unif, BTdiff.int),
    main = paste(config$sensor, "- BT3.7 - BT12 microns"),
    xlab = "B3.7 - BT12",
    ylab = "SST Residuals")
  abline(v = c(0, 1.5, 4.5, 10))
  
  # --- Now use four intervals
  
  BTdiff.int <- ordered(cut(uu0,
    breaks=c(min(uu0, na.rm = TRUE) - 0.01, 0.0,  1.5, 4.5, max(uu0, na.rm=TRUE)+0.01),
    labels=c("< 0C", "0+ to 1.5C", "1.5+ to 4.5C", ">4.5C"),
    include.lowest=TRUE))
  
  check <- tapply(uu0, INDEX = BTdiff.int, FUN = range, simplify = TRUE)
  
  barplot(prop.table(xtabs(~ BTdiff.int)), names.arg=levels(BTdiff.int),				
    main = paste(config$sensor,"- Matchups by BT3.9-BT12 Std. Diff. Intval"),
    xlab = "Proportion of Matchups",
    col = "lemonchiffon2", horiz = TRUE, cex.names = 0.7, las = 1)
  box()
  
} # End of BT difference calculation for VIIRS MWIR SST (SST3b)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- COMBINATION OF VARIABLES THAT DEFINE BINS IN THE HYPERCUBE
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 10. Generate data frame with set of coordinates for each bin ----
# --- All possible coordinate combinations are listed.

n.of.bins <- length(levels(orig2$day.or.nite)) *
  length(levels(orig2$qsst.unif)) *
  length(levels(quarter.of.yr)) * 
  length(levels(orig2$latband)) *
  length(levels(bsstint)) *
  length(levels(satzint)) *
  length(levels(BTdiff.int))

Log.info(paste('Number of hypercube bins is', n.of.bins,'...'))

ttt <- data.frame(expand.grid(list(
  levels(orig2$day.or.nite),
  levels(orig2$qsst.unif),
  levels(quarter.of.yr),
  levels(orig2$latband),
  levels(bsstint),
  levels(satzint),
  levels(BTdiff.int))))

colnames(ttt) <- c("day.or.nite", "qsst", "quarter.of.yr",
  "latband", "bsstint", "satzint", "BTdiff.int")

bin.coords <- dplyr::tbl_df(ttt) %>%
  dplyr::arrange(day.or.nite, qsst, quarter.of.yr, latband,
    bsstint, satzint, BTdiff.int)

rm(ttt); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 11. Generate "quality.cube.input" data frame to be used as input ---- 
# --- for calculation of hypercube quantities.

tt1 <- data.frame(
  day.or.nite = orig2$day.or.nite,
  qsst = orig2$qsst.unif,
  quarter.of.yr = quarter.of.yr,
	latband = orig2$latband,
	bsstint = bsstint,
	satzint = satzint,
	BTdiff.int = BTdiff.int,
  SST.res = SST.res)

quality.cube.input <- dplyr::tbl_df(tt1) %>%
  dplyr::arrange(day.or.nite, qsst, quarter.of.yr, latband, bsstint,
    satzint, BTdiff.int)

rownames(quality.cube.input) <- NULL

rm(tt1); gc()	
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 12. Calculate statistics of SST residuals for each bin ----

tt1 <- quality.cube.input %>%
  dplyr::group_by(day.or.nite, qsst, quarter.of.yr, latband, bsstint,
    satzint, BTdiff.int) %>%
  dplyr::summarize(res.mean = mean(SST.res, na.rm = TRUE),
    res.sd = sd(SST.res, na.rm = TRUE),
    res.median = median(SST.res, na.rm = TRUE),
    res.mad = mad(SST.res, na.rm = TRUE),
    res.rsd = IQR(SST.res, na.rm = TRUE) / 1.348,
    res.N = n())

# --- Object 'tt1' only has lines for bins that are not empty.
# --- Join with 'bin.coords' to get all possible bin coordinates.

hypercube.stats <- dplyr::left_join(bin.coords, tt1) %>%
  dplyr::mutate(res.N = dplyr::if_else(is.na(res.N), 0L, res.N)) %>%
  dplyr::arrange(day.or.nite, quarter.of.yr, latband,
    bsstint, satzint, BTdiff.int, qsst) %>%
  dplyr::mutate(res.sd = ifelse(res.N < 2, NA, res.sd),
    res.mad = ifelse(res.N < 2, NA, res.mad),
    res.rsd = ifelse(res.N < 2, NA, res.rsd))

Log.info(paste('Hypercube statistics object has',
  nrow(hypercube.stats), 'rows...'))

# --- The lines below were eliminated after discussions with Kay,
# --- Sue and Peter. The elimination is to be compliant with new
# --- GHRSST protocols. [2017-05-05]
# --- DEPRECATED: Because the bias values (res.mean and res.median) listed in the cube
# --- are those that ADDED to original data yield zero bias,
# --- we multiply these quantities by -1.
# hypercube.stats$res.mean <- hypercube.stats$res.mean * -1 
# hypercube.stats$res.median <- hypercube.stats$res.median * -1
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 13. TO DO: Decide if we use a fill-in value for empty bins ----

# --- For now we do not use any fill-in values.
# --- Conversation with Kay, 1 July 2017.
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 14.Write out hypercube results ----
# --- First, write out statistics for day AND night, and for ALL quality levels

cube.outdir <- paste0(config$results_dir,'hypercubes/') # Dir for output file

outfile <- paste(cube.outdir, config$sensor,
  "_", config$matchups$file.version,
  "_", config$geophys.var,
  "_", config$algorithm$type,
  "_", config$algorithm$algo.coeffs.version,
  "_DAYandNIGHT_all_quals.txt",
  sep = "")

write.table(hypercube.stats,
  file = outfile, append = FALSE,
  sep = "\t", na = "NA",
  row.names = FALSE, col.names = TRUE, quote = FALSE)

# --- Now write out stats separately for day and night, and for each quality level

for (i in levels(orig2$day.or.nite)) {
  for(j in quality.levels) {
  
    # Build output file name
    outfile <- paste(cube.outdir,
      config$sensor,
      "_", config$matchups$file.version,
      "_", config$geophys.var,
      "_", config$algorithm$type,
      "_", config$algorithm$algo.coeffs.version,
      "_", i,
      "_qual_", j, ".txt",
      sep = "")
    cat(paste(outfile,"\n"))
  
    # Select bins for day/nite and a given quality level 
    uuu <- dplyr::tbl_df(hypercube.stats) %>%
      dplyr::filter(qsst == j & day.or.nite == i) %>%
      dplyr::select(-qsst) # Eliminate qsst column
  
    # Write out statistics
    write.table(uuu, file=outfile, append=FALSE,
      sep="\t", na="NA", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
  } # End of looping through quality levels (index j)
} # End of looping through day or night (index i)

rm(tt1, cube.outdir, outfile, i, j); gc()
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- GRAPHICS DESCRIBING BIN OCCUPATION
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 15. Calculate bin occupation statistics for each quality level -----

# --- Table with number of bins occupied and empty
tt3 <- hypercube.stats %>%
  dplyr::mutate(occupied = dplyr::if_else(res.N == 0, 'occupied', 'empty')) %>%
  dplyr::group_by(qsst, occupied) %>%
  dplyr::summarize(N = n()) %>%
  tidyr::spread(occupied, N) %>%
  tidyr::complete(occupied, fill = list(occupied = 0, empty = 0))

total.bins <- tt3$empty + tt3$occupied

if (length(unique(total.bins)) != 1) {
  Log.error("Total number of bins differs among quality levels...\n")
}

# --- Barchart of proportion of empty and occupied bins by quality level

tt4 <- hypercube.stats %>%
  dplyr::mutate(occupied = dplyr::if_else(res.N == 0, 'occupied', 'empty')) %>%
  dplyr::group_by(qsst, occupied) %>%
  dplyr::summarize(N = n()) %>%
  dplyr::mutate(prop = (N / unique(total.bins)) * 100)

ggplot2::ggplot(data = tt4, ggplot2::aes(x = qsst, y = prop, fill = occupied)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::coord_flip() +
  ggplot2::ggtitle(paste(config$sensor, "hypercube bins")) +
  ggplot2::labs(x = 'SST Quality', y = 'Percentage of Bins') +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_discrete(name = "")
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 16. Plot histograms of numbers of matchups in non-empty bins ----

tt3 <- hypercube.stats %>%
  dplyr::filter(res.N > 0) %>%
  dplyr::select(res.N)

uuu <- max(tt3$res.N)

ggplot2::ggplot(data = tt3, ggplot2::aes(x = res.N)) +
  ggplot2::geom_histogram(breaks = c(0, 100, 200, 500, seq(1000, uuu, by = 2000)),
    col = "grey50",  fill = "wheat") +
  ggplot2::labs(x = "Number of matchups per bin", y = "Number of bins") +
  ggplot2::ggtitle("Number of matchups per bin") +
  ggplot2::geom_vline(xintercept = 200) + 
  ggplot2::theme_bw()

rm(tt3, uuu)
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------#
# --- 17. Clean up all objects EXCEPT ones we want to keep ----
# --- Do not delete objects with names equal to "orig" or
# --- starting with string "AQUA" or "TERRA" or "config".

tt1 <- objects()
tt2a <- str_detect(tt1, "^orig$")
tt2b <- str_detect(tt1, "^AQUA")
tt2c <- str_detect(tt1, "^TERRA")
tt2d <- str_detect(tt1, "^config")
tt2e <- str_detect(tt1, "^VIIRS")
tt3 <- tt2a | tt2b | tt2c | tt2d | tt2e
tt4 <- tt1[!tt3]
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt2d,tt2e,tt3,tt4); gc()
# ------------------------------------------------------------------------------


