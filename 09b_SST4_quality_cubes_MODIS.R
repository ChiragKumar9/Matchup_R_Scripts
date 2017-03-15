# ---------------------------------------------------------------------------------
# --- Script to calculate MODIS hypercubes with SST4 residual statistics
# ---------------------------------------------------------------------------------

config$geophys.var <- "SST4"

# ---------------------------------------------------------------------------------------#
# --- Define quality levels to be analyzed ----

if (config$geophys.var == "SST4") {
  if (config$algorithm$type == "latband1") {
    cat("Looking at quality values for", config$algorithm$type,"SST4s...\n")
    qsst <- orig$qsst4.new
    quality.levels <- as.numeric(names(table(orig$qsst4.new)))    # Quality levels to analyze
  } else if (config$algorithm$type == "V5") {
    cat("Looking at quality values for", config$algorithm$type,"SST4s...\n")
    qsst <- orig$qsst4
    quality.levels <- as.numeric(names(table(orig$qsst4)))		# Quality levels to analyze
  }
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create matrix to store cube bin statistics ----

cube.statistics <- as.data.frame(matrix(NA, nrow=(max(quality.levels) + 1), ncol=12,
	dimnames=list(NULL, c("geophys.var","sst4.algo","matchup version",
	"quality.to.process","empty.cube.bins",
	"occupied.cube.bins","q10","q25","q50","q75","q90","max"))))
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# --- DEFINE FACTORS FOR EACH VARIABLE USED TO DEFINE BINS IN THE HYPERCUBE ----
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- 1. Day or Night  ----  
# --- N.B. Use 80 degrees as the solar zenith angle threshold separating day and night.
# --- This is different from the MODIS processing (90 degrees is used for MODIS).

# --- NOTE: We no longer calculate statistics for daytime.
# --- This dimension has been removed from the hypercubes.

# --- Select records for night time only.
# --- No hypercube statistics are computed for daytime records.  

orig2 <- subset(orig, solz >= 90)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- 2. Quarters of the year ----

tt1 <- quarters(orig2$sat.timedate, abb=T)

quarter.of.yr <- ordered(tt1,
  levels=c("Q1","Q2","Q3","Q4"),
	labels=c("Q1","Q2","Q3","Q4"))
rm(tt1)

check <- tapply(orig2$sat.timedate, INDEX=quarter.of.yr, FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ quarter.of.yr)), names=names(table(quarter.of.yr)),
	main=paste(config$sensor,"- Matchups by Quarter of the Year"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- 3. Latitude intervals ----

latband <- orig2$latband

check <- tapply(orig2$buoy.lat, INDEX=latband,
	FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ latband)), names.arg=levels(latband),
	main=paste(config$sensor,"- Matchups by Latitude Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
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
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
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
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- 6. standardized (BT22 - BT23)  intervals (used for SST4) ----
# --- We use the (BT22 - BT23) difference corected by satellite zenith angle,
# --- that is the orig2inal BT difference minus the fitted value as a function of sza.

uu0 <- orig2$cen.39.40.ref.new

BTdiff.int <- ordered(cut(uu0,
  breaks=c(min(uu0, na.rm=T)-0.01, -0.5, 0.0, 0.5, max(uu0, na.rm=TRUE)+0.01),
  labels=c("< -0.5C", "-0.5+ to 0.0C", "0.0+ to 0.5C", ">0.5C"),
  include.lowest=TRUE))

check <- tapply(uu0, INDEX=BTdiff.int, FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ BTdiff.int)), names.arg=levels(BTdiff.int),				
	main=paste(config$sensor,"- Matchups by BT22-BT23 Std. Diff. Intrval."),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()

rm(uu0); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Generate data frame with set of coordinates for each bin. ----
# --- All possible coordinate combinations are listed.
# --- The bin coordinates as currently defined  yield 5736 bins
# --- (2 x 4 x 6 x 7 x 4 x 4, the number of levels for each dimension).

ttt <- data.frame(expand.grid(list(
	levels(quarter.of.yr),
	levels(latband),
	levels(bsstint),
	levels(satzint),
	levels(BTdiff.int) )))

colnames(ttt) <- c("quarter.of.yr",
	"latband","bsstint", "satzint", "BTdiff.int")

tt1 <- order(ttt$quarter.of.yr, ttt$latband,
	ttt$bsstint, ttt$satzint, ttt$BTdiff.int,
	decreasing=FALSE)

bin.coords <- ttt[tt1, ]

rownames(bin.coords) <- NULL

rm(ttt, tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Convert SST4 residuals into bulk SST4 values ----
# --- The mean and median values listed in the hypercube are the quantities that
# --- MUST BE ADDED to the orig2inal data to make them unbiased.
# --- The first step BEFORE calculating statistics is to remove the -0.17degC bias
# --- previously added to make "skin SSTs"

skin.offset <- 0.17 	# Skin SST4s are 0.17 degrees lower than "bulk" SST4s

if (config$algorithm$type == "latband1") {
	cat("Looking at residuals for", config$algorithm$type,"SST4s...\n")
	SST4.res <- orig2$SST4.latband1.res + skin.offset		# SST4 latband1 residuals
	qsst4 <- orig2$qsst4.new												    # SST4 latband1 quality levels
} else if (config$algorithm$type == "V5") {
	cat("Looking at residuals for",config$algorithm$type,"SST4s...\n")
	SST4.res <- orig2$SST4.V5.res + skin.offset				  # SST4 V5 residuals
	qsst4 <- orig2$PF5.qsst 														# SST4 V5 quality levels
}

rm(skin.offset)
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Generate "quality.cube.input" data frame to be used as input ---- 
# --- for calculation of hypercube quantities.

quality.cube.input <- data.frame(
	quarter.of.yr = quarter.of.yr,
	latband = latband,
	bsstint = bsstint,
	satzint = satzint,
	BTdiff.int = BTdiff.int,
	SST4.res = SST4.res,
	qsst4=qsst4)

tt1 <- order(quality.cube.input$quarter.of.yr,
	quality.cube.input$latband, quality.cube.input$bsstint,
	quality.cube.input$satzint, quality.cube.input$BTdiff.int,
	decreasing=FALSE)
	
quality.cube.input  <- quality.cube.input[tt1, ]

rownames(quality.cube.input) <- NULL

rm(tt1); gc()	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- LOOP through desired quality levels to process... ----

#  quality.to.process <- 0		# TO DEBUG

for (quality.to.process in quality.levels) {				

	cat("Producing modis SST4 hypercube for quality level ", quality.to.process,"...\n");
		
	# -------------------------------------------------------------------#
	# --- CALCULATE BIN STATISTICS
	# --- For each bin (defined by the unique combination of coordinates),
	# --- calculate statistics: mean, StdDev, median, MAD and  count
	
  tt1 <- aggregate(SST4.res ~
    BTdiff.int + satzint + bsstint + latband +
    quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst4 == quality.to.process),
    FUN=mean, na.action=na.omit, simplify=TRUE)
	
	tt2 <- aggregate(SST4.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
		data=quality.cube.input,
    subset=(qsst4 == quality.to.process),
		FUN=sd, na.action=na.omit, simplify=TRUE)
	
	tt3 <- aggregate(SST4.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
	  data=quality.cube.input,
    subset=(qsst4 == quality.to.process),
		FUN=median, na.action=na.omit, simplify=TRUE)
	
	tt4 <- aggregate(SST4.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst4 == quality.to.process),
    FUN=mad, na.action=na.omit, simplify=TRUE)

	tt5 <- aggregate(SST4.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst4 == quality.to.process),
    FUN=length, na.action=na.omit, simplify=TRUE)
		
	# --- Join all data frames
	
  aa1 <- join(bin.coords, tt1,
    by=c("quarter.of.yr","latband",
    "bsstint","satzint","BTdiff.int"), type="left", match="all")
  
	aa2 <- join(aa1, tt2,
	 by=c("quarter.of.yr","latband",
	 "bsstint","satzint","BTdiff.int"), type="left", match="all")
  
	aa3 <- join(aa2, tt3,
	 by=c("quarter.of.yr","latband",
	 "bsstint","satzint","BTdiff.int"), type="left", match="all")
	
  aa4 <- join(aa3, tt4,
    by=c("quarter.of.yr","latband",
    "bsstint","satzint","BTdiff.int"), type="left", match="all")
	
	aa5 <- join(aa4, tt5,
    by=c("quarter.of.yr","latband",
	  "bsstint","satzint","BTdiff.int"), type="left", match="all")
  
	colnames(aa5) <- c("quarter.of.yr","latband",
	  "bsstint","satzint","BTdiff.int",
	  "ResMean","ResStdDev","ResMedian","ResMAD","ResCount")
  
  
 	# --- For the field that contains count of matchups in a bin,
	# --- replace "NA" with 0 (zero).

	aa5$ResCount <- ifelse(is.na(aa5$ResCount), 0, aa5$ResCount);

	# --- Because the bias values (ResMean and ResMedian) listed in the cube
	# --- are those that ADDED to original data yield zero bias,
	# --- we multiply these quantities by -1.

	aa5$ResMean <- aa5$ResMean * -1 
	aa5$ResMedian <- aa5$ResMedian * -1

	# ----------------------------------------------------------------#
	# --- Calculate statistics for the hypercube for this quality level

	empty.cube.bins <- length(aa5$ResCount[aa5$ResCount==0])
	occupied.cube.bins <- length(aa5$ResCount[aa5$ResCount > 0])
	
  if (empty.cube.bins + occupied.cube.bins != nrow(aa5))
    stop("Error in calculating empty or occupied bins...\n")
  
  qqq <- quantile(aa5$ResCount[aa5$ResCount > 0],
    prob=c(0.10, 0.25, 0.50, 0.75, 0.90, 1.00))

	qq2 <- c(config$geophys.var, config$algorithm$type, 
    config$matchups$version, quality.to.process,
    empty.cube.bins, occupied.cube.bins,qqq)

	cube.statistics[(quality.to.process + 1), ] <- qq2	# Store statistics for this quality level

	rm(empty.cube.bins, occupied.cube.bins,qqq,qq2)
	# ------------------------------------------------------------------#

	# ------------------------------------------------------------------#
	# --- Write out cube results...

	outfile <- paste(cube.outdir, config$sensor,
	  "_", config$matchups$version,
	  "_", config$geophys.var,
	  "_", config$algorithm$type,
	  "-", config$algorithm$coefficients.version,
	  "_cube_qual_", quality.to.process,
	  ".txt", sep="")

	write.table(aa5, file=outfile, append=FALSE,
		sep="\t", na="NA", row.names=FALSE, col.names=TRUE, quote=FALSE)
	# --------------------------------------------------------------------#

	# --------------------------------------------------------------------#
	# --- Do plots of bin occupation and statistics

	#uu1 <- hexbin(aa5$ResMean, aa5$ResMedian)
	#gplot.hexbin(uu1, style="colorscale",
	#	colorcut=c(0.00,0.20,0.40,0.60,0.80, 1.00),
	#	colramp=function(n){brewer.pal(5,"YlOrRd")},
	#	border="grey70",
	#	xlab="Mean of residuals for each bin",
	#	ylab="Median of residuals for each bin",
	#	main=paste(config$sensor,"Hypercube - Bin Means & Medians - Q =",quality.to.process))
	
}	# End of looping through quality levels
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- After looping through quality levels to process, write out cube statistics ----

outfile <- paste(cube.outdir, config$sensor,
  "_", config$matchups$version,
  "_", config$geophys.var,
  "_", config$algorithm$type,
  "-", config$algorithm$coefficients.version,
  "_cube_STATISTICS.txt", sep="")

write.table(cube.statistics, file=outfile, append=FALSE,
	sep=" ", na="NA", row.names=FALSE, col.names=TRUE, quote=FALSE)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Clean up all objects EXCEPT those with names ----
# --- equal to "orig" or starting with string "AQUA" or "TERRA

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













aa6 <- subset(aa5, ResCount > 0)

aa7 <- as.numeric(cut(aa6$ResMedian, breaks=c(-3.5, -2, -1, 0, 1, 2, 3.5)))


brewer.pal(length(table(aa7)), "Reds")


tmPlot(aa6,
	index=c("day.or.nite","quarter.of.yr","latband","bsstint","satzint","int45"),
	vSize="ResCount",
	vColor="ResMedian", 
 algorithm="squarified")


tab

library(treemap)

uuu <- data.frame(qqq=cube.statistics$quality.to.process,
  occ = as.numeric(cube.statistics$occupied.cube.bins))

tmPlot(uuu,
			 index="qqq",
			 vSize="occ",
			 algorithm="squarified")




map.market(	group=c("day.or.nite", "quarter.of.yr"), area=aa6$ResCount)



data <- read.csv("http://datasets.flowingdata.com/post-data.txt")
install.packages("portfolio")
library(portfolio)
map.market(id=data$id, area=data$views, group=data$category, color=data$comments, main="FlowingData Map")

tmPlot(dtf, index, vSize, vColor = NULL, type = "value",
			 title = NA, subtitle = NA, algorithm = "pivotSize",
			 sortID = "-size", palette = NA, range = NA,
			 vColorRange = NULL, fontsize.title = 14,
			 fontsize.labels = 11, fontsize.legend = 12,
			 lowerbound.cex.labels = 0.4, inflate.labels = FALSE,
			 bg.labels = ifelse(type %in% c("value", "linked", "categorical"), "#CCCCCCAA", NA),
			 force.print.labels = FALSE,
			 position.legend = ifelse(type %in% c("categorical", "index"), "right", ifelse(type == "linked", "none", "bottom")),
			 aspRatio = NA, na.rm = FALSE)



# four comparison treemaps
tmPlot(sbsData, 
			 index=c("section", "subsection"), 
			 vSize=c("employees09", "value added09", "turnover09", "salaries09"), 
			 vColor=c("employees08", "value added08", "turnover08", "salaries08"),
			 type="comp")

