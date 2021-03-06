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
#require(DeducerExtras) #TO DO: Figure out how to install on linux...
#require(Deducer)
require(rpart.plot)
require(ggmap)
require(caret)
require(clue)
require(plot3D)

ggplot <- function(...) {ggplot2::ggplot(...) + theme_bw()}

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

# Read AQUA Matchups file - from 01read_matchups.R script
load('/home/ckk/Projects/Matchup_R_Scripts/Results/objects/MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2017_02_23_with_ancillary.Rdata')
orig <- `MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2017_02_23`
rm(`MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2017_02_23`)

# Exact Google Object
#orig <- readr::read_csv('/home/ckk/Projects/Matchup_R_Scripts/Results/objects/orig_filtered_google_recreation.csv')

#--------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Prep matchups and generate features for analysis ------------------------

# Filter for only nighttime data - avoid diurnal heating
orig$buoy.timedate <- as.character(orig$buoy.timedate)
orig$sat.timedate <- as.character(orig$sat.timedate)
orig <- dplyr::tbl_df(orig) %>% 
  #dplyr::filter(buoy.timedate <= lubridate::ymdhms('2010-05-08 23:57:59')) %>% # For Google recreation
  dplyr::filter(solz >= 90)

# Select variables - variables to generate terms in NLSST and other variables that
# are useful in determining SST retrieval accuracy 
orig2 <- dplyr::tbl_df(orig) %>%
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
    SST.resid = (buoy.sst + .17) - cen.sst) %>% 
  dplyr::select(x1, x2, x3, lat, sd11, sd12, range11, range12, diff.med.min11, diff.med.min12, SST.resid)

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Explore distribution of matchups ----

# Spatial distribution
mp <- NULL
mapWorld <- borders("world", colour = "gray50", fill = "gray70") # create a layer of borders
mp <- ggplot() +  mapWorld +
  ggplot2::scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  ggplot2::scale_y_continuous(breaks = seq(from = -90, to = 90, by = 30)) +
  coord_fixed(ratio = 1)

# WARNING: Be careful as this worked with ggplot2 2.1.0 on Linux
# Try to work on different machines with varying versions of ggplot2

# Now layer the buoys on top with hexagonal binning
mp <- mp + 
  ggplot2::stat_binhex(data = orig,
    aes(x = buoy.lon, # Use hexagonal binning command and give x and y input
      y = buoy.lat,
      fill = cut(..count.., c(0, 1000, 2000, 3000, 4000, 5000, 10000, Inf))), # Divides matchups per bin into discrete chunks and colors likewise
    binwidth = c(10, 10)) +
  mapWorld + labs(x = NULL, y = NULL) +
  #scale_fill_hue('value') + # Standard colors with discrete chunking
  scale_fill_brewer(palette = 'YlOrRd') + # Change colors to Yellow, Orange, and Red - many diff 
  guides(fill = guide_legend(title = "Number of Matchups")) +
  ggtitle('Spatial Distribution of Matchups')

ggplot2::ggsave(filename = 'point_distribution_modis.pdf', device = 'pdf',
  width = 8, height = 6, units = 'in')

# Temporal distribution

orig$sat.timedate <- lubridate::ymd_hms(orig$sat.timedate)
orig$buoy.timedate <- lubridate::ymd_hms(orig$buoy.timedate)
yy <- lubridate::year(orig$buoy.timedate)     # Year
mm <- lubridate::month(orig$buoy.timedate)    # Month

tt1 <- cbind(yy, mm)

tt2 <- dplyr::tbl_df(tt1) %>%
  dplyr::group_by(yy, mm) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(yy, mm)

yy.vals <- sort(unique(yy))
mm.vals <- sort(unique(mm))
tt3 <- expand.grid(mm = mm.vals, yy = yy.vals)

tt4 <- dplyr::left_join(tt3, tt2) %>%
  dplyr::select(yy, mm, n)

brks <- pretty(tt4$n, n = 5)
#brks <- floor(quantile(tt4$n, probs = seq(0, 1, 0.25), na.rm = TRUE))

tt4$n <- cut(tt4$n, breaks = brks)
tt4$mm <- ordered(tt4$mm, labels = month.abb)
tt4$yy <- ordered(tt4$yy)

hm <- ggplot2::ggplot(data = tt4, aes(mm, yy)) +
  ggplot2::geom_tile(aes(fill = n), colour = "white") +
  ggplot2::scale_fill_brewer(type = "seq", palette = 'YlOrRd', direction = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title = "Number of Matchups")) +
  ggtitle('Temporal Distribution of Matchups')
  ggplot2::ggsave("Temporal_heatmap_modis.pdf", device = 'pdf',
    width = 8, height = 6, units = 'in')

# Residual distribution in the 3D hypervolume
ccc <- RColorBrewer::brewer.pal(n = 8, name = 'YlOrRd')

plot3D::scatter3D(x = orig3$x1, y = orig3$x2, z = orig3$x3, colvar = abs(SST.resid),
  pch = '.', cex = 2, bty = 'g', theta = 252.8, phi = 20,
  col = plot3D::ramp.col(col = c(ccc[1], ccc[4:8])),
  main = 'Location of Retrievals in the 3D Space and their Residuals', xlab = 'x1', ylab = 'x2', zlab = 'x3', ticktype = 'detailed',
  clab = c('abs(SST Residual)'))

ggplot2::ggplot() +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) +
  ggtitle('Distribution of Residuals') + 
  ggplot2::ggsave("Residual_histogram.pdf", device = 'pdf',
    width = 8, height = 6, units = 'in')

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Test varying number of clusters and features ----

# Testing different cluster and feature choices
cluster.choices <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200)
feature.num.choices <- seq(3, 10, 1)
evaluation.withinss <- data.frame(matrix(NA, length(cluster.choices), length(feature.num.choices)))
median.best <- data.frame(matrix(NA, length(cluster.choices), length(feature.num.choices)))
IQR.best <- data.frame(matrix(NA, length(cluster.choices), length(feature.num.choices)))
median.IQR.match <- data.frame(matrix(NA, length(cluster.choices), length(feature.num.choices)))

colnames(evaluation.withinss) <- c("3f", "4f", "5f", "6f", "7f", "8f", "9f", "10f")
rownames(evaluation.withinss) <- c("10c", "20c", "30c", "40c", "50c", "60c", "70c", "80c", "90c", "100c", "150c", "200c")
colnames(median.best) <- c("3f", "4f", "5f", "6f", "7f", "8f", "9f", "10f")
rownames(median.best) <- c("10c", "20c", "30c", "40c", "50c", "60c", "70c", "80c", "90c", "100c", "150c", "200c")
colnames(IQR.best) <- c("3f", "4f", "5f", "6f", "7f", "8f", "9f", "10f")
rownames(IQR.best) <- c("10c", "20c", "30c", "40c", "50c", "60c", "70c", "80c", "90c", "100c", "150c", "200c")
colnames(median.IQR.match) <- c("3f", "4f", "5f", "6f", "7f", "8f", "9f", "10f")
rownames(median.IQR.match) <- c("10c", "20c", "30c", "40c", "50c", "60c", "70c", "80c", "90c", "100c", "150c", "200c")

for (nfeatures in feature.num.choices) {
  for (nclusters in cluster.choices) {
    # Scale and apply clustering
    scaled_orig <- scale.robust(as.matrix(orig3[ , 1:nfeatures]), "IQR")
    fit <- kmeans(scaled_orig, nclusters)
    # Store total within sum of squares for later analysis
    evaluation.withinss[match(nclusters, cluster.choices), match(nfeatures, feature.num.choices)] <- fit$tot.withinss
    # Compute median of residuals in best cluster and store for later analysis
    qqq <- data.frame(orig3[ , 1:nfeatures], SST.resid = orig3$SST.resid, fit.cluster = fit$cluster)
    meds <- tapply(qqq$SST.resid, qqq$fit.cluster, median)
    median.best.clus <- min(abs(meds))
    median.best[match(nclusters, cluster.choices), match(nfeatures, feature.num.choices)] <- median.best.clus
    med.index <- match(median.best.clus, meds)
    # Compute IQR of residuals in best cluster and store for later analysis
    IQRs <- tapply(qqq$SST.resid, qqq$fit.cluster, IQR)
    IQR.best.clus <- IQRs[med.index]
    IQR.best[match(nclusters, cluster.choices), match(nfeatures, feature.num.choices)] <- IQR.best.clus
    # Check that the lowest median cluster is also the cluster with the lowest IQR
    paste(nfeatures + 3, "features and ", nclusters, "clusters.")
  }
}

# Plot resiults
# TO DO...

# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------#
# ---- Cluster matchups then explore resid distribution of clusters/groups ----

# First apply scaling to improve convergence speeds
to.drop <- c("")
orig3 <- orig3[ , !(names(orig3)) %in% to.drop]
tt1 <- as.matrix(orig3[ , !(names(orig3)) %in% c('SST.resid')])
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
orig_clustering <- data.frame(orig3[tt2, ], fit$cluster)

# Info on each cluster - used as evaluation metrics    
means <- tapply(X = orig_clustering$SST.resid, INDEX = orig_clustering$fit.cluster, FUN = mean)
medians <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, median)
sds <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, sd)
IQRs <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, IQR)
lengths <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, length)

# Histogram of the SST.resid for all clusters
lattice::histogram(~SST.resid | factor(fit.cluster), data = orig_clustering, breaks = seq(-12, 30))

ggplot2::ggplot(data = orig_clustering, aes(x = SST.resid)) +
  geom_histogram(breaks = seq(-12, 30)) +
  facet_wrap(~fit.cluster) +
  ggtitle('Residual Distribution by Cluster') +
  ggsave(filename = 'Residual_distribution_cluster.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# Good and bad groups
# Good = five clusters with lowest median residual
# Bad = five clusters with highest median residual
best.values <- medians[medians <= quantile(medians, seq(0, 1, 0.1))[2]]
worst.values <- medians[medians >= quantile(medians, seq(0, 1, 0.1))[10]]
good.clusters <- c(match(best.values, medians))
bad.clusters <- c(match(worst.values, medians))
best.median.cluster <- match(min(medians), medians)
worst.median.cluster <- match(max(medians), medians)

# For Plotting - can color code classes
class_cluster <- factor(ifelse(orig_clustering$fit.cluster %in% good.clusters,
  'Good',
  ifelse(orig_clustering$fit.cluster %in% bad.clusters, 'Bad', 'Neither good nor bad')))

# Just to identify good
good <- factor(ifelse(orig_clustering$fit.cluster %in% good.clusters,
  'Good',
  'NotGood'))

# Just to identify bad
bad <- factor(ifelse(orig_clustering$fit.cluster %in% bad.clusters,
  'Bad',
  'NotBad'))

# Good or bad based on residual

group_resid <- factor(ifelse(abs(orig_clustering$SST.resid <= .3), "Good", "NotGood"))
orig_clustering <- data.frame(orig3[tt2, ], fit$cluster, class_cluster, good, bad, group_resid)

xtabs(~ good + group_resid)

# Histogram of lowest median cluster
ggplot2::ggplot(data = orig_clustering[orig_clustering$fit.cluster == best.median.cluster, ]) +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) +
  #ggtitle('Residual Distribution of Lowest Residual Median Cluster') +
  ylim(c(0, 40000)) +
  ggsave(filename = 'Residual_distribution_best_cluster.pdf', device = 'pdf',
width = 8, height = 6, units = 'in')

# Histogram of highest median cluster
ggplot2::ggplot(data = orig_clustering[orig_clustering$fit.cluster == worst.median.cluster, ]) +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) +
  ylim(c(0, 20000)) +
  #ggtitle('Residual Distribution of Highest Residual Median Cluster') +
  ggsave(filename = 'Residual_distribution_worst_cluster.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# Explore each group and gain an intuition for residual distribution in each group
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Neither good nor bad'])>.3)
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Bad'])>.3)
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Good'])>.3)

# Hist of good group
ggplot2::ggplot(data = orig_clustering[orig_clustering$class_cluster == 'Good' , ]) +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) +
  ggtitle('Residual Distribution of the Good Group') + 
  ggsave(filename = 'Residual_distribution_good_group.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# Statistical summary of good group
summary(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Good'])
IQR(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Good'])

# Hist of bad group
ggplot2::ggplot(data = orig_clustering[orig_clustering$class_cluster == 'Bad' , ]) +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) + 
  ggtitle('Residual Distribution of Bad Group') +
  ggsave(filename = 'Residual_distribution_bad_group.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# Statistical summary of bad group
summary(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Bad'])
IQR(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Bad'])


# Hist of neither good nor bad group
ggplot2::ggplot(data = orig_clustering[orig_clustering$class_cluster == 'Neither good nor bad' , ]) +
  geom_histogram(aes(x = SST.resid), breaks = seq(-12, 30)) + 
  ggtitle('Residual Distribution of Neither Good nor Bad Group') +
  ggsave(filename = 'Residual_distribution_neither_good_nor_bad_group.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# Statistical summary of neither good nor bad group
summary(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Neither good nor bad'])
IQR(orig_clustering$SST.resid[orig_clustering$class_cluster == 'Neither good nor bad'])

# Location of groups in 3D hypervolume
# TO DO
# WORK IN PROGRESS...
plot3D::scatter3D(x = orig3[tt2, ]$x1,
  y = orig3[tt2, ]$x2,
  z = orig3[tt2, ]$x3,
  colvar = as.integer(orig_clustering$class_cluster),
  pch = '.', cex = 2, bty = 'g', theta = 252.8, phi = 20, alpha = 0.6,
  main = 'Location of Residual Groups in 3D Space', xlab = 'x1', ylab = 'x2', zlab = 'x3',
  ticktype = 'detailed',
  col = c(ccc[2], ccc[5], ccc[8]),
  colkey = list(at = c(2, 3, 4), side = 4, 
    addlines = TRUE, length = 0.5, width = 0.5,
    labels = c("Neither Good nor Bad", "Bad", "Good")))

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

bgcm <- caret::confusionMatrix(data = bgp, reference = orig_clustering$bad, positive = "Bad")

# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------#
# ---- Use clustering to create a residual range for each retrieval ----

# Test whether residual falls into a residual range defined by the cluster the retrieval fell into
# The residual range is defined as median +/- IQR of  residuals in the cluster the retrieval is part of
# Do on test data --> k-means was NOT initially fit with this data (i.e. cluster centroids were not
# fit to incorporate this data)

orig_test <- data.frame(scaled_orig[!tt2, ])
predictions_cl <- clue::cl_predict(fit, orig_test)
correctbias <- rep(999, length(predictions_cl))
medianpredict <- rep(999, length(predictions_cl))
IQRpredict <- rep(999, length(predictions_cl))
windowmin <- rep(999, length(predictions_cl))
windowmax <- rep(999, length(predictions_cl))

for (i in seq(1, length(predictions_cl))) {
  medianpredict[i] <- median(abs(orig_clustering$SST.resid[orig_clustering$fit.cluster == predictions_cl[i]]))
  IQRpredict[i] <- IQR(abs(orig_clustering$SST.resid[orig_clustering$fit.cluster == predictions_cl[i]]))
  windowmin[i] <- medianpredict[i] - IQRpredict[i]
  windowmax[i] <- medianpredict[i] + IQRpredict[i]
  if (windowmax[i] >= SST.resid[!tt2][i] & windowmin[i] <= SST.resid[!tt2][i]) {
    correctbias[i] <- TRUE
  }
  else {
    correctbias[i] <- FALSE
  }
  #cat('Trial: ', i, ' of ', length(predictions_cl), '\n')
}

table(correctbias)

ccc <- RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd')[3:9]

require(ggplot2)

ggplot2::ggplot() +
  stat_bin2d(aes(x = SST.resid[!tt2], y = medianpredict)) +
  scale_fill_gradientn(colours = ccc) + 
  xlim(-5, 5) + 
  ylim(0, 2) +
 #geom_errorbar(aes(x = SST.resid[!tt2], ymin = windowmin, ymax = windowmax), width = 0.05) +
  ylab("Predicted SST Residual") +
  xlab("Actual SST Residual") +
  ggsave(filename = 'accuracy_predictions_density.pdf', device = 'pdf',
    width = 8, height = 6, units = 'in')

# TO DO: Test how accurate a window of median +/- 1 deg would be
# --------------------------------------------------------------------------------

# -------------------------------------------------------------------------------#
# ---- Explore location and atmospheric trends in good and bad groups ----
# First plot locations of good and bad groups on a map
# Get longitude from orig
long <- orig$buoy.lon
orig_clustering <- data.frame(orig_clustering, long = long[tt2])

# 'Good' matchups
# Spatial distribution
mp <- NULL
mapWorld <- borders("world", colour = "gray50", fill = "gray70") # create a layer of borders
mp <- ggplot() +  mapWorld +
  ggplot2::scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  ggplot2::scale_y_continuous(breaks = seq(from = -90, to = 90, by = 30)) +
  coord_fixed(ratio = 1)

# WARNING: Be careful as this worked with ggplot2 2.1.0 on Linux
# Try to work on different machines with varying versions of ggplot2

# Now layer the buoys on top with hexagonal binning
mp <- mp + 
  ggplot2::stat_binhex(data = orig_clustering[class_cluster == 'Good', ],
    aes(x = long, # Use hexagonal binning command and give x and y input
      y = lat,
      fill = cut(..count.., c(0, 500, 1000, 2000, 3000, 4000, 5000, Inf))), # Divides matchups per bin into discrete chunks and colors likewise
    binwidth = c(10, 10)) +
  mapWorld + labs(x = NULL, y = NULL) +
  #scale_fill_hue('value') + # Standard colors with discrete chunking
  scale_fill_brewer(palette = 'YlOrRd') + # Change colors to Yellow, Orange, and Red - many diff 
  guides(fill = guide_legend(title = "Number of Matchups")) +
  ggtitle('Spatial Distribution of Good Matchups')

# 'Bad' Matchups
# Spatial distribution
mp <- NULL
mapWorld <- borders("world", colour = "gray50", fill = "gray70") # create a layer of borders
mp <- ggplot() +  mapWorld +
  ggplot2::scale_x_continuous(breaks = seq(from = -180, to = 180, by = 60)) +
  ggplot2::scale_y_continuous(breaks = seq(from = -90, to = 90, by = 30)) +
  coord_fixed(ratio = 1)

# WARNING: Be careful as this worked with ggplot2 2.1.0 on Linux
# Try to work on different machines with varying versions of ggplot2

# Now layer the buoys on top with hexagonal binning
mp <- mp + 
  ggplot2::stat_binhex(data = orig_clustering[class_cluster == 'Bad', ],
    aes(x = long, # Use hexagonal binning command and give x and y input
      y = lat,
      fill = cut(..count.., c(0, 500, 1000, 2000, 3000, 4000, 5000, Inf))), # Divides matchups per bin into discrete chunks and colors likewise
    binwidth = c(10, 10)) +
  mapWorld + labs(x = NULL, y = NULL) +
  #scale_fill_hue('value') + # Standard colors with discrete chunking
  scale_fill_brewer(palette = 'YlOrRd') + # Change colors to Yellow, Orange, and Red - many diff 
  guides(fill = guide_legend(title = "Number of Matchups")) +
  ggtitle('Spatial Distribution of Bad Matchups')

# --------------------------------------------------------------------------------

# -------------------------------------------------------------------------------#
# ---- Explore different clustering algorithms ----

# --------------------------------------------------------------------------------

