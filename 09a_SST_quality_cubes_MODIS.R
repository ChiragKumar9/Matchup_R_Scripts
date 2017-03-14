# ---------------------------------------------------------------------------------
# --- Script to calculate MODIS hypercubes with SST residual statistics
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Define quality levels to be analyzed ----

if (config$geophys.var == "SST") {
  if (config$algorithm$type == "latband1") {
    cat("Looking at quality values for", config$algorithm$type,"SSTs...\n")
    qsst <- orig$qsst.new
    quality.levels <- as.numeric(names(table(orig$qsst.new)))	  # Quality levels to analyze
  } else if (config$algorithm$type == "V5") {
    cat("Looking at quality values for", config$algorithm$type,"SSTs...\n")
    qsst <- orig$qsst
    quality.levels <- as.numeric(names(table(orig$qsst)))		# Quality levels to analyze
  }
}
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Create matrix to store cube bin statistics ----

cube.statistics <- as.data.frame(matrix(NA, nrow=(max(quality.levels)+1), ncol=12,
	dimnames=list(NULL, c("geophys.var","sst.algo","matchup version",
	"quality.to.process","empty.cube.bins",
	"occupied.cube.bins","q10","q25","q50","q75","q90","max"))))
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# --- DEFINE FACTORS FOR EACH VARIABLE USED TO DEFINE BINS IN THE HYPERCUBE
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- 1. Day or Night ----   
# --- N.B. Use 90 degrees as the solar zenith angle threshold separating day and night.
# --- This is different from the MODIS processing (80 degrees is used for AVHRR).

# --- NOTE: We no longer calculate statistics for daytime.
# --- This dimension has been removed from the hypercubes.

# --- Select records for night time only.
# --- No hypercube statistics are computed for daytime records.  

orig2 <- subset(orig, solz >= 90)
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
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
	col="lemonchiffon2", horiz=TRUE, las=1)
box()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- 3. Latitude intervals ----

latband <- orig2$latband

check <- tapply(orig2$buoy.lat, INDEX=latband,
	FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ latband)), names.arg=levels(latband),
	main=paste(config$sensor,"- Matchups by Latitude Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, las=1, cex.names=0.7)
box()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
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
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
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
	col="lemonchiffon2", horiz=TRUE, cex.names=0.6, las=1)
box()

quantile(abs(orig2$satz), probs=seq(from=0, to=1, by=0.25))
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- 6. BT31 - BT32  intervals (used for SST) ----

uu0 <- (orig2$cen.11000 - orig2$cen.12000)

BTdiff.int <- ordered(cut(uu0,
	breaks= c(min(uu0, na.rm=T) - 0.1, 0.0, 0.7, 2.0, max(uu0, na.rm=TRUE) + 0.1),
	labels=c("< 0.0C", "0.0 to 0.7C", "0.7+ to 2.0C", "> 2.0C"),
	include.lowest=TRUE))

check <- tapply(uu0, INDEX=BTdiff.int, FUN=range, simplify=TRUE)

barplot(prop.table(xtabs(~ BTdiff.int)), names.arg=levels(BTdiff.int),				
	main=paste(config$sensor,"- Matchups by BT31 - BT32 Interval"),
	xlab="Proportion of Matchups",
	col="lemonchiffon2", horiz=TRUE, cex.names=0.7, las=1)
box()

rm(uu0); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
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
	levels(BTdiff.int) )))

colnames(ttt) <- c("quarter.of.yr",
	"latband","bsstint", "satzint", "BTdiff.int")

tt1 <- order(ttt$quarter.of.yr, ttt$latband,
	ttt$bsstint, ttt$satzint, ttt$BTdiff.int,
	decreasing=FALSE)

bin.coords <- ttt[tt1, ]
dim(bin.coords)

rownames(bin.coords) <- NULL

rm(ttt, tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- ADD "skin" bias to SST estimates ----
# --- The mean and median values listed in the hypercube are the quantities that
# --- MUST BE ADDED to the orig2inal data to make them unbiased.
# --- The first step BEFORE calculating statistics is to remove the -0.17degC bias
# --- previously added to make "skin SSTs"

skin.offset <- 0.17 	# Skin SSTs are 0.17 degrees lower than "bulk" SSTs

if (config$algorithm$type == "latband1") {
	cat("Adding skin bias to residuals for", config$algorithm$type,"SSTs...\n")
	SST.res <- orig2$SST.latband1.res + skin.offset	  # SST latband1 residuals
	qsst <- orig2$qsst.new												    # SST latband1 quality levels
}

rm(skin.offset)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Generate "quality.cube.input" data frame to be used as input ---- 
# --- for calculation of hypercube quantities.

quality.cube.input <- data.frame(quarter.of.yr = quarter.of.yr,
	latband = latband,
	bsstint = bsstint,
	satzint = satzint,
	BTdiff.int = BTdiff.int,
	SST.res = SST.res,
	qsst=qsst)

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
  #  quality.to.process <- 0 
	cat("Producing MODIS SST hypercube for quality level ", quality.to.process,"...\n");
	
  
	# --------------------------------------------------------------------#
	# --- CALCULATE BIN STATISTICS
	# --- For each bin (defined by the unique combination of coordinates),
	# --- calculate statistics: mean, StdDev, median, MAD and  count
	
  tt1 <- aggregate(SST.res ~
    BTdiff.int + satzint + bsstint + latband +
    quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst == quality.to.process),
    FUN=mean, na.action=na.omit, simplify=TRUE)
	
	tt2 <- aggregate(SST.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
		data=quality.cube.input,
    subset=(qsst == quality.to.process),
		FUN=sd, na.action=na.omit, simplify=TRUE)
	
	tt3 <- aggregate(SST.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
	  data=quality.cube.input,
    subset=(qsst == quality.to.process),
		FUN=median, na.action=na.omit, simplify=TRUE)
	
	tt4 <- aggregate(SST.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst == quality.to.process),
    FUN=mad, na.action=na.omit, simplify=TRUE)

	tt5 <- aggregate(SST.res ~
		BTdiff.int + satzint + bsstint + latband +
		quarter.of.yr,
    data=quality.cube.input,
    subset=(qsst == quality.to.process),
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

  # --- HERE WE WOULD eliminate bin statistics (making them NA)
  # --- for bins with count below the desired threshold.
  
  
	# --- Because the bias values (ResMean and ResMedian) listed in the cube
	# --- are those that ADDED to original data yield zero bias,
	# --- we multiply these quantities by -1.

	aa5$ResMean <- aa5$ResMean * -1 
	aa5$ResMedian <- aa5$ResMedian * -1

	# -----------------------------------------------------------------#
	# --- Calculate statistics for the hypercube for this quality level

	empty.cube.bins <- length(aa5$ResCount[aa5$ResCount==0])
	occupied.cube.bins <- length(aa5$ResCount[aa5$ResCount > 0])
	
  if (empty.cube.bins + occupied.cube.bins != nrow(aa5))
    stop("Error in calculating empty or occupied bins...\n")
  
  qqq <- quantile(aa5$ResCount[aa5$ResCount > 0],
    prob=c(0.10, 0.25, 0.50, 0.75, 0.90, 1.00))

	qq2 <- c(config$geophys.var, config$algorithm$type,
	  config$matchups$version, quality.to.process,
		empty.cube.bins, occupied.cube.bins, qqq)

	cube.statistics[(quality.to.process + 1), ] <- qq2	# Store statistics for this quality level

	rm(empty.cube.bins, occupied.cube.bins, qqq, qq2)
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
	# ------------------------------------------------------------------#

	# ------------------------------------------------------------------#
	# --- Do plots of bin occupation and statistics

	#uu1 <- hexbin(aa5$ResMean, aa5$ResMedian)
	#gplot.hexbin(uu1, style="colorscale",
	#	colorcut=c(0.00,0.20,0.40,0.60,0.80, 1.00),
	#	colramp=function(n){brewer.pal(5,"YlOrRd")},
	#	border="grey70",
	#	xlab="Mean of residuals for each bin",
	#	ylab="Median of residuals for each bin",
	#	main=paste(sensor,"Hypercube - Bin Means & Medians - Q =",quality.to.process))
	
}	# End of looping through quality levels
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
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

























# ---------------------------------------------------------------------------------------#
# --- Explore how many matchups we need to have stable statistics in a hypercube bin ----

# First we select hypercube bins with many matchups (> 500) and high variability

aa6 <- subset(aa5, ResCount > 500)

range(aa6$ResStdDev)
uuu <- which(aa6$ResStdDev > 0.41)   # Bin with high variance

aa6[uuu, ]



#quarter.of.yr   latband       bsstint       satzint   BTdiff.int      ResMean ResStdDev
#Q2              20N+ to 40N   23+ to 28C    50+ deg   0.7+ to 2.0C -0.005519675 0.4321768



tt1 <- quarters(orig2$sat.timedate, abb=T)
quarter.of.yr <- ordered(tt1,
  levels=c("Q1","Q2","Q3","Q4"),
  labels=c("Q1","Q2","Q3","Q4"))

latband <- orig2$latband

bsstint <- ordered(cut(orig2$buoy.sst,
  breaks= c(-2, 3, 8, 13, 18, 23, 28, 45),
	labels=c("-2 to 3C","3+ to 8C","8+ to 13C","13+ to 18C",
	"18+ to 23C","23+ to 28C", "> 28C"), 
	include.lowest=TRUE))

satzint <- ordered(cut(abs(orig2$satz),
  breaks=c(0, 30, 40, 50, max(abs(orig2$satz))+5),
	labels=c("0 to 30 deg","30+ to 40 deg","40+ to 50 deg","50+ deg"), 
	include.lowest=TRUE))

uu0 <- (orig2$cen.11000 - orig2$cen.12000)
BTdiff.int <- ordered(cut(uu0,
  breaks= c(min(uu0, na.rm=T) - 0.1, 0.0, 0.7, 2.0, max(uu0, na.rm=TRUE) + 0.1),
	labels=c("< 0.0C", "0.0 to 0.7C", "0.7+ to 2.0C", "> 2.0C"),
	include.lowest=TRUE))

# --- Select a certain bin for quality 0

use <- quarter.of.yr == "Q2" & latband == "20N+ to 40N" &
  bsstint == "23+ to 28C" & satzint == "50+ deg" &
  BTdiff.int == "0.7+ to 2.0C" & qsst == 0

ssts <- subset(orig2, subset=use,
  select=SST.latband1.res)
ssts <- ssts[,1] + 0.17   # 534 SST values

hist(ssts, breaks=seq(-2.5, 2.5, 0.5),
  col="cadetblue",
  xlab="SST residuals",
  main="Distribution of residuals in bin studied")
box()


# Compute sample statistics and bootstrapped statistics

## mean(ssts)     0.005519675   # Sample mean
## sd(ssts)       0.4321768     # Sample SD
## median(ssts)  -0.01661458    # sample median
## mad(ssts)      0.4420788     # sample MAD
## length(ssts) 534             # sample size

round(mad(ssts), 4)



library(boot)

boot.mean <- function(x, i) {mean(x[i])}
boot.median <- function(x, i) {median(x[i])}
boot.sd <- function(x, i) {sd(x[i])}
boot.mad <- function(x, i) {mad(x[i])}

boot.sst.mean <- boot(ssts, statistic=boot.mean, R=10000)
boot.sst.median <- boot(ssts, statistic=boot.median, R=10000)
boot.sst.sd <- boot(ssts, statistic=boot.sd, R=10000)
boot.sst.mad <- boot(ssts, statistic=boot.mad, R=10000)

plot(boot.sst.median)
plot(boot.sst.mean)
plot(boot.sst.sd)
plot(boot.sst.mad)


tt1 <- boot.ci(boot.sst.mean, conf=0.95, type="all")
boot.int.mean <- tt1$bca[4:5]

tt1 <- boot.ci(boot.sst.median, conf=0.95, type="all")
boot.int.median <- tt1$bca[4:5]

tt2 <- boot.ci(boot.sst.sd, conf=0.95, type="all")
boot.int.sd <- tt2$bca[4:5]

tt2 <- boot.ci(boot.sst.mad, conf=0.95, type="all")
boot.int.mad <- tt2$bca[4:5]


# Take samples of different sizes

sample.sizes <- seq(from=10, to=500, by=10)

n.of.samples <- 500

sampling.results.mean <- matrix(NA, ncol=length(sample.sizes), nrow=n.of.samples)
rownames(sampling.results.mean) <- 1:n.of.samples

sampling.results.median <- matrix(NA, ncol=length(sample.sizes), nrow=n.of.samples)
rownames(sampling.results.median) <- 1:n.of.samples

sampling.results.sd <- matrix(NA, ncol=length(sample.sizes), nrow=n.of.samples)
rownames(sampling.results.sd) <- 1:n.of.samples

sampling.results.mad <- matrix(NA, ncol=length(sample.sizes), nrow=n.of.samples)
rownames(sampling.results.mad) <- 1:n.of.samples



for (i in 1:n.of.samples) {  
  for (j in 1:length(sample.sizes)) {
        
    tt1 <- sample(ssts, size=sample.sizes[j], replace=TRUE)
    sampling.results.mean[i, j] <- mean(tt1)
    sampling.results.median[i, j] <- median(tt1)
    sampling.results.sd[i, j] <- sd(tt1)
    sampling.results.mad[i, j] <- mad(tt1)

  }
}




# Plot results form sampling

boxplot(sampling.results.mean,
  whisklty=1, col="cadetblue",
  names=sample.sizes,
  xlab="No. of matchups in bin (sample size)",
  ylab="Estimated mean of SST residuals",
  main="Mean of SST residuals")

abline(h=mean(ssts), col="tomato")
abline(h=boot.int.mean[1], col="orange")
abline(h=boot.int.mean[2], col="orange")


boxplot(sampling.results.median,
  whisklty=1, col="cadetblue",
  names=sample.sizes,
  xlab="No. of matchups in bin (sample size)",
  ylab="Estimated median of SST residuals",
  main="Median of SST residuals")

abline(h=median(ssts), col="tomato")
abline(h=boot.int.median[1], col="orange")
abline(h=boot.int.median[2], col="orange")


boxplot(sampling.results.sd,
  whisklty=1, col="cadetblue",
    names=sample.sizes,
  xlab="No. of matchups in bin (sample size)",
  ylab="Estimated SD of SST residuals",
  main="SD of SST residuals")

abline(h=sd(ssts), col="tomato")
abline(h=boot.int.sd[1], col="orange")
abline(h=boot.int.sd[2], col="orange")


boxplot(sampling.results.mad,
  whisklty=1, col="cadetblue",
    names=sample.sizes,
  xlab="No. of matchups in bin (sample size)",
  ylab="Estimated MAD of SST residuals",
  main="MAD of SST residuals")

abline(h=mad(ssts), col="tomato")
abline(h=boot.int.mad[1], col="orange")
abline(h=boot.int.mad[2], col="orange")









# --- Quantify how many of the samples have means or SDs
# --- outside bootstrapped 95% confidence intervals

# Function to count how many samples have statistics OUTSIDE 95% CIs

is.outside.ci <- function(x, ci.l, ci.u) {
  uu1 <- ifelse(x < ci.l | x > ci.u, TRUE, FALSE)
  res <- length(uu1[uu1 == TRUE]) / length(uu1)
  return(res)
}

uu2 <- apply(sampling.results.mean, MARGIN=2, FUN=is.outside.ci,
  ci.l=boot.int.mean[1], ci.u=boot.int.mean[2])

plot(sample.sizes, uu2,
  type="h", lwd=2, col="grey50",
  ylim=c(0,0.8),
  xlab="Sample size (matchups per bin",
  ylab="Proportion of samples with MEAN outside CIs",
  main="Simulated means")
abline(h=0.05, col="tomato")


uu3 <- apply(sampling.results.median, MARGIN=2, FUN=is.outside.ci,
  ci.l=boot.int.median[1], ci.u=boot.int.median[2])

plot(sample.sizes, uu3,
  type="h", lwd=2, col="grey50",
  ylim=c(0,0.8),
  xlab="Sample size (matchups per bin",
  ylab="Proportion of samples with MEDIAN outside CIs",
  main="Simulated medians")
abline(h=0.05, col="tomato")


uu4 <- apply(sampling.results.sd, MARGIN=2, FUN=is.outside.ci,
  ci.l=boot.int.sd[1], ci.u=boot.int.sd[2])

plot(sample.sizes, uu4,
  type="h",lwd=2, col="grey50",
  ylim=c(0,0.8),
  xlab="Sample size (matchups per bin",
  ylab="Proportion of samples with SD outside CIs",
  main="Simulated Standard deviations")
abline(h=0.05, col="tomato")


uu5 <- apply(sampling.results.mad, MARGIN=2, FUN=is.outside.ci,
  ci.l=boot.int.mad[1], ci.u=boot.int.mad[2])

plot(sample.sizes, uu5,
  type="h",lwd=2, col="grey50",
  ylim=c(0,0.8),
  xlab="Sample size (matchups per bin",
  ylab="Proportion of samples with MAD outside CIs",
  main="Simulated MADs")
abline(h=0.05, col="tomato")




# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#


aa6 <- subset(aa5, ResCount > 100)



aa7 <- aa6[,c(2:4,10)]

treemap(aa7,
  index=c("latband","bsstint","satzint"),
  type="index",
	vSize="ResCount",
	vColor="latband", 
  algorithm="squarified",
  overlap.labels=0.75)






aa7 <- as.numeric(cut(aa6$ResMedian, breaks=c(-3.5, -2, -1, 0, 1, 2, 3.5)))


brewer.pal(length(table(aa7)), "Reds")




tab



tmPlot(cube.statistics,
			 index="quality.to.process",
			 vSize="occupied.cube.bins",
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

