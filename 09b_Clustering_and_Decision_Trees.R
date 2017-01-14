# We perform clustering on SST Matchups to identify geophysically similar regions
# of the hypercube. Clustering allows quantifying the expected residual of a 
# retrieval based on the median ± IQR of the cluster the retrieval is part of.
# Decision Trees allow visualization of what combinations of geophyisical variables
# led to very high and low residual points.

# ------------------------------------------------------------------------------#
# ---- Prep workspace -----------------------------------------------------------
# Import necessary packages

require(dplyr)
require(readr)
require(circular)
require(mclust)
require(rpart)
require(randomForest)
require(scatterplot3d)
require(ggplot2)
require(ROCR)
require(misc3d)
require(hypervolume)
require(DeducerExtras)
require(Deducer)
require(rpart.plot)
require(ggplot2)

# Define secant function
secant.deg <- function(x) {1 / (cos(rad(x)))}

# Define robust scaling function
# 3 Methods: Scaling with median and MAD, median and IQR, and mean normalization
scale.robust <- function(x, method) {
  if (method == "MAD") {
    for (i in seq(1, ncol(x))) {
      df <- as.matrix(x[, i])
      subtract <- median(df)
      divide <- mad(df)
      x[, i] <- scale(df, center = subtract, scale = divide)
    }
    return(x)
  }
  if (method == "IQR") {
    for (i in seq(1, ncol(x))) {
      df <- as.matrix(x[, i])
      subtract <- median(df)
      divide <- IQR(df)
      x[, i] <- scale(df, center = subtract, scale = divide)
    }
    return(x)
  }
  # Standard is mean normalization
  if (method == "standard") {
    x <- as.matrix(x)
    x_scaled <- scale(x, center = TRUE, scale = TRUE)
    return(x_scaled)
  }
  else {
    stop("Method given was not 'MAD', 'IQR', or 'standard'.")
  }
}

# Read AQUA Matchups file (2002-2010) - from 01read_matchups.R script
load('/home/ckk/Projects/Matchup_R_Scripts/Results/objects/MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2016_11_26_with_ancillary.Rdata')
orig <- `MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2016_11_26`
rm(`MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2016_11_26`)
#--------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Prep matchups and generate features for analysis ------------------------

# Filter for only nighttime data - avaoid sunglint contamination
orig_solz <- dplyr::tbl_df(orig) %>% 
  dplyr::filter(solz >= 90)

# Select variables - variables to generate terms in NLSST and other variables that
# are useful in determining SST retrieval accuracy 
orig2 <- dplyr::tbl_df(orig_solz) %>%
  dplyr::select(satz,
    cen.11000,
    cen.12000,
    sd.11000,
    sd.12000,
    buoy.sst,
    cen.ref.type.1.SST,
    cen.sst,
    buoy.lat,
    min.11000,
    max.11000,
    med.11000,
    min.12000,
    max.12000,
    med.12000,
    qsst)

# Define features - 3 terms in NLSST
x1 <- orig2$cen.11000  										  # BT31 (brigthness temperature for channel 31)
x2 <- orig2$cen.11000 - orig2$cen.12000		  # BT31 - BT32
x3 <- orig2$cen.ref.type.1.SST						  # First Guess SST
x4 <- orig2$satz											      # satellite zenith angle (no sign)
x2 <- x2 * x3														    # BT31-BT32 * buoy SST
x3 <- (secant.deg(orig2$satz) - 1) * x2     # Secant times BT31-BT32
lat <- orig2$buoy.lat                       # Buoy Latitude
sd11 <- orig2$sd.11000                      # SD11
sd12 <- orig2$sd.12000
range11 <- orig2$max.11000 - orig2$min.11000 # Range 11
range12 <- orig2$max.12000 - orig2$min.12000
diff.med.min11 <- orig2$med.11000 - orig2$min.11000 # Med minus min 11 - diff between 0 and 2nd quartile
diff.med.min12 <- orig2$med.12000 - orig2$min.12000
SST.resid <- (orig2$buoy.sst + .17) - orig2$cen.sst # Add .17 for average nighttime buoy bias - conversion to skin SST
qsst <- orig2$qsst

#x7 <- ifelse(orig2$mirror == 1, 0, 1)      # Dummy mirror variable 0 for side1, 1 for side2

# Create df of features
orig3 <- dplyr::tbl_df(orig2) %>%
  dplyr::mutate(x1 = cen.11000,
    x2 = cen.11000 - cen.12000,
    x3 = cen.ref.type.1.SST,
    x4 = satz,
    x2 = x2 * x3,
    x3 = ((secant.deg(satz) - 1) * x2),
    SST.resid = buoy.sst - cen.sst,
    lat = buoy.lat,
    sd11 = sd.11000,
    sd12 = sd.12000,
    range11 = max.11000 - min.11000,
    range12 = max.12000 - min.12000,
    diff.med.min11 = med.11000 - min.11000,
    diff.med.min12 = med.12000 - min.12000, 
    SST.resid = (buoy.sst-.17) - cen.sst) %>% 
  dplyr::select(x1, x2, x3, lat, sd11, sd12, range11, range12, diff.med.min11, diff.med.min12, SST.resid)

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Test varying number of clusters and features ----

# Testing different cluster and feature choices
cluster.choices <- c(10, 20, 30, 40, 50, 100, 150, 200, 250, 500)
feature.num.choices <- seq(3, 8, 1)
evaluation.withinss <- data.frame(rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6),
                                  rep(999, 6))
median.best <- data.frame(rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6),
                          rep(999, 6))
IQR.best <- data.frame(rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6),
                       rep(999, 6))
median.IQR.match <- data.frame(rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6),
                               rep(999, 6))
colnames(evaluation) <- c("3f", "4f", "5f", "6f", "7f", "8f")
rownames(evaluation) <- c("10c", "20c", "30c", "40c", "50c", "75c", "100c", "150c", "200c", "250c", "500c")
for (nfeatures in feature.num.choices) {
  for (nclusters in clusterchoices) {
    # Scale and apply clustering
    scaled_orig <- scale.robust(as.matrix(orig3[ , 1:nfeatures]), "IQR")
    fit <- kmeans(scaled_orig, nclusters)
    # Store total within sum of squares for later analysis
    evaluation.withinss[match(nfeatures, feature.num.choices), match(nclusters, clusterchoices)] <- fit$tot.withinss
    # Compute median of residuals in best cluster and store for later analysis
    qqq <- data.frame(orig3[ , 1:nfeatures], fit.cluster = fit$cluster)
    meds <- tapply(qqq$SST.resid, qqq$fit.cluster, median)
    median.best.clus <- min(meds)
    median.best[match(nfeatures, feature.num.choices), match(nclusters, clusterchoices)] <- median.best.clus
    med.index <- match(median.best.clus, meds)
    # Compute IQR of residuals in best cluster and store for later analysis
    IQRs <- tapply(qqq$SST.resid, qqq$fit.cluster, IQR)
    IQR.best.clus <- min(IQRs)
    IQR.best[match(nfeatures, feature.num.choices), match(nclusters, clusterchoices)] <- IQR.best.clus
    IQR.index <- match(IQR.best.clus, IQRs)
    # Check that the lowest median cluster is also the cluster with the lowest IQR
    median.IQR.match[match(nfeatures, feature.num.choices), match(nclusters, clusterchoices)] <- median.index == IQR.index
  }
}

# Plot resiults
# TO DO...

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Cluster matchups then explore clusters/groups, esp. resid distribution ----

# First apply scaling to improve convergence speeds
to.drop <- c("sd12")
orig3 <- orig3[ , !(names(orig3)) %in% to.drop]
tt1 <- as.matrix(orig3)
scaled_orig <- scale.robust(tt1, "IQR")

# Creating Test and Train Sets
proportion.in.train.set <- .8
row.numbers <- seq(from = 1, to = nrow(scaled_orig), by = 1)
size.train.set <- floor(proportion.in.train.set * nrow(scaled_orig))
set.seed(256)
train.set.row.numbers <- sort(sample(row.numbers, size = size.train.set, replace = FALSE))
tt2 <- row.numbers %in% train.set.row.numbers

# Using an elbow-plot, it was determined that 50 clusters was optimal
# Fit k-means to only train set
# Leave test set for later evaluation of clustering
fit <- kmeans(scaled_orig[tt2, ], 50)
orig_clustering <- data.frame(orig3[tt2, ], SST.resid = SST.resid[tt2], fit$cluster)

# Info on each cluster - used as evaluation metrics    
means <- tapply(X = orig_clustering$SST.resid, INDEX = orig_clustering$fit.cluster, FUN = mean)
medians <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, median)
sds <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, sd)
IQRs <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, IQR)
lengths <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, length)

# Histogram of the SST.resid for all clusters
lattice::histogram(~SST.resid | factor(fit2.cluster), data = orig_clustering, breaks = seq(-12, 30))

# Good and bad clusters
# Good = five clusters with lowest median residual
# Bad = five clusters with highest median residual
good.clusters <- c(match(quantile(medians, seq(0, 1, 0.1))[1:5], quantile(medians, seq(0, 1, 0.1))))
bad.clusters <- c(match(quantile(medians, seq(0, 1, 0.1))[46:50], quantile(medians, seq(0, 1, 0.1))))

# For Plotting - can color code classes
class_cluster <- factor(ifelse(orig_clustering$fit2.cluster %in% good.clusters,
  'Good',
  ifelse(orig_clustering$fit2.cluster %in% bad.clusters, 'Bad', 'So-so')))

# Just to identify good
good <- factor(ifelse(orig_clustering$fit2.cluster %in% good.clusters,
  'Good',
  'NotGood'))

# Just to identify bad
bad <- factor(ifelse(orig_clustering$fit2.cluster %in% bad.clusters,
  'Bad',
  'NotBad'))

# Good or bad based on residual

group_resid <- factor(ifelse(abs(orig_clustering$SST.resid <= .3), "Good", "NotGood"))
orig_clustering <- data.frame(orig3[tt2, ], fit2$cluster, class_cluster, good, bad, group_resid)

# Explore each group and gain an intuition for residual distribution in each group
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='So-so'])>.3)
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Bad'])>.3)
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Good'])>.3)

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Fit Decision Trees to 'good' and 'bad' groups ----

# Decision Trees to identify what condition (geophysical variables) effect whether
# a retrieval is "good" or "bad"
# DT for good group
good_tree <- rpart::rpart(good ~ x1 + x2 + x3 + lat + sd11 + range11 + range12 + diff.med.min11 + diff.med.min12,
  data = orig_clustering, minsplit = 15)

# Plot trees
rpart.plot::prp(good_tree, uniform=TRUE,
  main="Classification Tree for Good Clusters")

# Evaluate the trees' variable cutoff values - test how well the tries predict the group
# for good points
ggp <- predict(good_tree,
  orig_clustering,
  type = 'class',
  na.action = na.pass)

ggcm <- caret::confusionMatrix(data = ggp, reference = orig_clustering$good, positive = "Good")

# DT for bad group
bad_tree <- rpart::rpart(bad ~ x1 + x2 + x3 + lat + sd11 + range11 + range12 + diff.med.min11 + diff.med.min12,
  data = orig_clustering)

# Plot trees
rpart.plot::prp(bad_tree, uniform=TRUE,
  main="Classification Tree for Bad Clusters")

# Do the same prediction as above for bad points
bgp <- predict(bad_tree,
  orig_clustering,
  type = 'class',
  na.action = na.pass)

bgcm <- caret::confusionMatrix(data = bgp, reference = orig_clustering$good, positive = "Bad")

# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------#
# ---- Use clustering to create a residual range for each retrieval ----

# Test whether residual falls into a residual range defined by the cluster the retrieval fell into
# The residual range is defined as median +/- IQR of  residuals in the cluster the retrieval is part of
# Do on test data --> k-means was NOT fit with this data

orig_test <- data.frame(scaled_orig[!tt2, ], SST.resid = SST.resid[!tt2])
correctbias <- rep(999, length(predictions))

for (i in seq(1, length(predictions))) {
  medianpredict <- median(orig_clustering$SST.resid[orig_clustering$fit2.cluster == predictions[i]])
  IQRpredict <- IQR(orig_clustering$SST.resid[orig_clustering$fit2.cluster == predictions[i]])
  if (medianpredict + IQRpredict >= orig_test$SST.resid[i] & medianpredict - IQRpredict <= orig_test$SST.resid[i]) {
    correctbias[i] <- TRUE
  }
  else {
    correctbias[i] <- FALSE
  }
}

table(correctbias)

# TO DO: Test how accurate a window of median +/- 1 deg would be
# --------------------------------------------------------------------------------