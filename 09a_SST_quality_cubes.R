# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- Script to calculate hypercubes with SST residual statistics
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------#
# --- Define quality levels to be analyzed ----

if (config$geophys.var == "SST") {
  # MODIS
  if (config$algorithm$type == "latband1") {
    Log.info(paste("Looking at quality values for",
      config$sensor, config$algorithm$type,"SSTs..."))
    quality.levels <- sort(unique(orig$qsst.new))	  # Quality levels to analyze
  } else if (config$algorithm$type == "V5") {
    Log.info(paste("Looking at quality values for",
      config$sensor, config$algorithm$type,"SSTs..."))
    quality.levels <- as.numeric(names(table(orig$qsst)))	  # Quality levels to analyze
  }
} else if (config$geophys.var == "SST2b") {
  # VIIRS
  if (config$algorithm$type == "latband") {
    Log.info(paste("Looking at quality values for",
      config$sensor, config$algorithm$type,"SSTs..."))
    quality.levels <- sort(unique(orig$qsst))	  # Quality levels to analyze
  }
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# --- DEFINE FACTORS FOR EACH VARIABLE USED TO DEFINE BINS IN THE HYPERCUBE
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 1. Day or Night ----   
# --- N.B. Use 90 degrees as the solar zenith angle threshold separating day and night.
# --- This is for MODIS and VIIRS processing (80 degrees is used for AVHRR).

# --- NOTE: We no longer calculate hypercube statistics for daytime.
# --- This dimension has been removed from the hypercubes.
# --- Select records for night time only.

orig2 <- dplyr::tbl_df(orig) %>%
  dplyr::filter(solz >= 90) 
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 2. Quarters of the year ----

tt1 <- quarters(orig2$sat.timedate, abb=T)

quarter.of.yr <- ordered(tt1,
  levels = c("Q1","Q2","Q3","Q4"),
	labels = c("Q1","Q2","Q3","Q4"))
rm(tt1)

check <- orig2 %>%
  dplyr::mutate(qtr = quarters(sat.timedate, abb = FALSE))  %>%
  dplyr::group_by(qtr) %>%
  dplyr::summarize(min = min(sat.timedate),
    max = max(sat.timedate))

barplot(prop.table(xtabs(~ quarter.of.yr)), names=names(table(quarter.of.yr)),
	main=paste(config$sensor,"- Matchups by Quarter of the Year"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, las=1)
box()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 3. Latitude intervals ----

latband <- orig2$latband

check <- tapply(orig2$buoy.lat, INDEX=latband,
	FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ latband)), names.arg=levels(latband),
	main=paste(config$sensor,"- Matchups by Latitude Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, las=1, cex.names=0.7)
box()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 4. Buoy SST intervals ----

bsstint <- ordered(cut(orig2$buoy.sst,
	breaks= c(-2, 3, 8, 13, 18, 23, 28, 45),
	labels=c("-2 to 3C","3+ to 8C","8+ to 13C","13+ to 18C",
	"18+ to 23C","23+ to 28C", "> 28C"), 
	include.lowest=TRUE))

check <- tapply(orig2$buoy.sst,
	INDEX=bsstint,
	FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ bsstint)), names.arg=levels(bsstint),
	main=paste(config$sensor,"- Matchups by Buoy SST Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()

quantile(orig2$buoy.sst, probs=c(0, 0.20, 0.40, 0.60, 0.80, 1))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 5. Satellite zenith angle intervals ----
# --- NOTE: This considers only ABSOLUTE values of zenith angle

satzint <- ordered(cut(abs(orig2$satz),
	breaks=c(0, 30, 40, 50, max(abs(orig2$satz))+5),
	labels=c("0 to 30 deg","30+ to 40 deg","40+ to 50 deg","50+ deg"), 
	include.lowest=TRUE))

check <- tapply(abs(orig2$satz), INDEX=satzint, FUN=range, simplify=TRUE)
				
barplot(prop.table(xtabs(~ satzint)), names.arg=levels(satzint),				
	main=paste(config$sensor,"- Matchups by Satellite Zenith Angle Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.5, las=1)
box()

quantile(abs(orig2$satz), probs=seq(from=0, to=1, by=0.25))
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- 6. 11 micron BT minus 12 micron BT intervals (used for SST) ----

uu0 <- (orig2$cen.11000 - orig2$cen.12000)

BTdiff.int <- ordered(cut(uu0,
	breaks= c(min(uu0, na.rm=T) - 0.1, 0.0, 0.7, 2.0, max(uu0, na.rm=TRUE) + 0.1),
	labels=c("< 0.0C", "0.0 to 0.7C", "0.7+ to 2.0C", "> 2.0C"),
	include.lowest=TRUE))

check <- tapply(uu0, INDEX=BTdiff.int, FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ BTdiff.int)), names.arg=levels(BTdiff.int),				
	main=paste(config$sensor,"- Matchups by 11micron - 12micron BT Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.5, las=1)
box()

rm(uu0); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Generate data frame with set of coordinates for each bin ----
# --- All possible coordinate combinations are listed.
# --- The bin coordinates as currently defined  yield 2688 bins
# --- (this is after eliminating the day/night dimension).
# --- This number comes from: 4 x 6 x 7 x 4 x 4,
# --- the number of levels for each dimension).

ttt <- data.frame(expand.grid(list(
	levels(quarter.of.yr),
	levels(latband),
	levels(bsstint),
	levels(satzint),
	levels(BTdiff.int),
  levels(as.factor(orig2$qsst)))))

colnames(ttt) <- c("quarter.of.yr",
	"latband","bsstint", "satzint", "BTdiff.int", "qsst")

bin.coords <- dplyr::tbl_df(ttt) %>%
  dplyr::arrange(quarter.of.yr, latband,
    bsstint, satzint, BTdiff.int, qsst)

rm(ttt); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- ADD "skin" bias to SST estimates ----
# --- The mean and median values listed in the hypercube are the quantities that
# --- MUST BE ADDED to the original data to make them unbiased.
# --- The first step BEFORE calculating statistics is to remove the -0.17 degC bias
# --- previously added to make "skin SSTs"

skin.offset <- 0.17 	# Skin SSTs are 0.17 degrees lower than "bulk" SSTs

if (config$algorithm$type == "latband") {
	Log.info(paste("Adding skin bias to residuals for",
	  config$sensor, 
	  config$algorithm$type,
	  "SSTs..."))
	SST.res <- orig2$sst.minus.buoy.sst + skin.offset	  # SST latband1 residuals
}

rm(skin.offset)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Generate "quality.cube.input" data frame to be used as input ---- 
# --- for calculation of hypercube quantities.

tt1 <- data.frame(quarter.of.yr = quarter.of.yr,
	latband = latband,
	bsstint = bsstint,
	satzint = satzint,
	BTdiff.int = BTdiff.int,
	qsst = as.factor(orig2$qsst),
  SST.res = SST.res)

quality.cube.input <- dplyr::tbl_df(tt1) %>%
  dplyr::arrange(quarter.of.yr, latband, bsstint,
    satzint, BTdiff.int)

rownames(quality.cube.input) <- NULL

rm(tt1); gc()	
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Calculate statistics of SST residuals for each bin ----

tt1 <- quality.cube.input %>%
  dplyr::group_by(quarter.of.yr, latband, bsstint,
    satzint, BTdiff.int, qsst) %>%
  dplyr::summarize(res.mean = mean(SST.res, na.rm = TRUE),
    res.sd = sd(SST.res, na.rm = TRUE),
    res.median = median(SST.res, na.rm = TRUE),
    res.mad = mad(SST.res, na.rm = TRUE),
    res.rsd = IQR(SST.res, na.rm = TRUE) / 1.348,
    res.N = n())

# --- Object 'tt1' only has lines for bins that are not empty.
# --- Join with 'bin.coords' to get all possible bin coordinates.

hypercube.stats <- dplyr::left_join(bin.coords, tt1) %>%
  dplyr::mutate(res.N.2 = if_else(is.na(as.double(res.N)),
    0, as.double(res.N))) %>%
  dplyr::select(-res.N) %>%
  dplyr::rename(res.N = res.N.2)
  
# --- Because the bias values (res.mean and res.median) listed in the cube
# --- are those that ADDED to original data yield zero bias,
# --- we multiply these quantities by -1.

hypercube.stats$res.mean <- hypercube.stats$res.mean * -1 
hypercube.stats$res.median <- hypercube.stats$res.median * -1
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Write out hypercube results ----
# --- First, write out statistics for ALL quality levels

cube.outdir <- paste0(config$results_dir,'hypercubes/') # Dir for output file

outfile <- paste(cube.outdir, config$sensor,
  "_", config$matchups$file.version,
  "_", config$geophys.var,
  "_", config$algorithm$type,
  "_", config$algorithm$algo.coeffs.version,
  "_all_quals.txt", sep = "")

write.table(hypercube.stats, file=outfile, append=FALSE,
  sep="\t", na="NA", row.names=FALSE, col.names=TRUE, quote=FALSE)

# --- Write out stats for quality levels 0 to 3

for(i in 0:3) {
  
  outfile <- paste(cube.outdir, config$sensor,
    "_", config$matchups$file.version,
    "_", config$geophys.var,
    "_", config$algorithm$type,
    "_", config$algorithm$algo.coeffs.version,
    "_qual_", i, ".txt", sep = "")
  
  uuu <- dplyr::tbl_df(hypercube.stats) %>%
    dplyr::filter(qsst == i)
  
  write.table(uuu, file=outfile, append=FALSE,
    sep="\t", na="NA", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
} # End of looping through quality levels

rm(tt1, cube.outdir, outfile); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- Calculate bin occupation statistics for each quality level -----

# --- Table with number of bins occupied and empty
tt3 <- hypercube.stats %>%
  dplyr::mutate(occupied = if_else(res.N == 0, 'occupied', 'empty')) %>%
  dplyr::group_by(qsst, occupied) %>%
  dplyr::summarize(N = n()) %>%
  tidyr::spread(occupied, N)

# --- Barchart of proportion of empty and occupied bins by quality level

tt4 <- hypercube.stats %>%
  dplyr::mutate(occupied = if_else(res.N == 0, 'occupied', 'empty')) %>%
  dplyr::group_by(qsst, occupied) %>%
  dplyr::summarize(N = n()) %>%
  dplyr::mutate(prop = (N / 2688) * 100)

ggplot2::ggplot(data = tt4, ggplot2::aes(x = qsst, y = prop, fill = occupied)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ggtitle(paste(config$sensor, "hypercube bins")) +
  ggplot2::labs(x = 'SST Quality', y = 'Percentage of Bins') +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_discrete(name="")
# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Clean up all objects EXCEPT those with names ----
# --- equal to "orig" or starting with string "AQUA" or "TERRA" or "config"

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
