#This code performs clustering on SST Matchups to identify geophysically similar regions of the hypercube.
#Clustering allows quantifying the expected residual of a retrieval based on the median ± IQR of the cluster the retrieval is part of
#Decision Trees were created to visualize what combinations of geophyisical variables led to very high and low residual points


#Import necessary packages
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

#Define secant function
secant.deg <- function(x) {1 / (cos(rad(x)))}

#Define robust scaling function
#3 Methods: Scaling with median and MAD, median and IQR, and mea
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
  #Standard is 
  if (method == "standard") {
    x <- as.matrix(x)
    x_scaled <- scale(x, center = TRUE, scale = TRUE)
    return(x_scaled)
  }
  else {
    stop("Method given was not 'MAD', 'IQR', or 'standard'.")
  }
}

#-------------------------------------------------------------------------------

#Read Matchups file (2002-2010)
orig <- readr::read_csv("/home/ckk/Projects/Matchup_R_Scripts/Results/objects/MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2016_01_23.csv")

#Filter for only nighttime data
orig_solz <- dplyr::tbl_df(orig) %>% 
  dplyr::filter(solz >= 90)

#2: Check orig dataframe for correct filteration solz >= 90
#if (min(orig_filtered$solz, na.rm = TRUE) >= 90) {
#  print("Orig is filtered correctly.")
#} else {
#  stop("Orig is filtered incorrectly. Re-filter and try again.")
#}

#Select variables
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

#Define features
x1 <- orig2$cen.11000  										  # BT31 (brigthness temperature for channel 31)
x2 <- orig2$cen.11000 - orig2$cen.12000		  # BT31 - BT32
x3 <- orig2$cen.ref.type.1.SST						  # First Guess SST
x4 <- orig2$satz											      # satellite zenith angle (no sign)
x2 <- x2 * x3														    # BT31-BT32 * buoy SST
x3 <- (secant.deg(orig2$satz) - 1) * x2     # Secant times BT31-BT32
lat <- orig2$buoy.lat                       # Buoy Latitude
sd11 <- orig2$sd.11000                      #SD11
sd12 <- orig2$sd.12000
range11 <- orig2$max.11000 - orig2$min.11000
range12 <- orig2$max.12000 - orig2$min.12000
diff.med.min11 <- orig2$med.11000 - orig2$min.11000
diff.med.min12 <- orig2$med.12000 - orig2$min.12000
SST.resid <- (orig2$buoy.sst-.17) - orig2$cen.sst #Subtract .17 for average buoy bias
qsst <- orig2$qsst

#x7 <- ifelse(orig2$mirror == 1, 0, 1)      # Dummy mirror variable 0 for side1, 1 for side2

#Create df of features
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

#Testing different cluster number choices
clusterchoices <- c(10, 20, 30, 40, 50, 100)
evaluation <- data.frame(rep(999, 6), rep(999, 6), rep(999, 6), rep(999, 6), rep(999, 6), rep(999, 6))
colnames(evaluation) <- c("3f", "4f", "5f", "6f", "7f", "8f")
rownames(evaluation) <- c("10c", "20c", "30c", "40c", "50c", "100c")
for (nfeatures in seq(3, 8, 1)) {
  for (nclusters in clusterchoices) {
    scaled_orig <- scale.robust(as.matrix(orig3[ ,1:nfeatures]), "IQR")
    fit <- kmeans(scaled_orig, nclusters)
    evaluation[nfeatures-2, match(nclusters, clusterchoices)] <- fit$tot.withinss
  }
}



#Clustering
#Scaling
tt1 <- as.matrix(orig3)
scaled_orig <- scale.robust(tt1, "IQR")

#Creating Test and Train Sets
proportion.in.train.set <- .8
row.numbers <- seq(from = 1, to = nrow(scaled_orig), by = 1)
size.train.set <- floor(proportion.in.train.set * nrow(scaled_orig))
set.seed(256)
train.set.row.numbers <- sort(sample(row.numbers, size = size.train.set, replace = FALSE))
tt2 <- row.numbers %in% train.set.row.numbers

#Using an elbow-plot, it was determined that 50 clusters was optimal
fit <- kmeans(scaled_orig[tt2, ], 50)
orig_clustering <- data.frame(orig3[tt2, ], SST.resid = SST.resid[tt2], fit$cluster)

#Info on each cluster    
means <- tapply(X = orig_clustering$SST.resid, INDEX = orig_clustering$fit.cluster, FUN = mean)
medians <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, median)
sds <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, sd)
MADs <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, mad)
lengths <- tapply(orig_clustering$SST.resid, orig_clustering$fit.cluster, length)

#Histogram of the SST.resid for all clusters
lattice::histogram(~SST.resid | factor(fit2.cluster), data = orig_clustering, breaks = seq(-12, 30))

#Good and bad clusters
#Good = five clusters with lowest median residual
#Bad = five clusters with highest median residual
good.clusters <- c(28, 42, 38, 3, 39)
bad.clusters <- c(36, 24, 21, 8, 44)

#For Plotting
class_cluster <- factor(ifelse(orig_clustering$fit2.cluster %in% good.clusters,
  'Good',
  ifelse(orig_clustering$fit2.cluster %in% bad.clusters, 'Bad', 'So-so')))

#Just to identify good
good <- factor(ifelse(orig_clustering$fit2.cluster %in% good.clusters,
  'Good',
  'NotGood'))

#Just to identify bad
bad <- factor(ifelse(orig_clustering$fit2.cluster %in% bad.clusters,
  'Bad',
  'NotBad'))

#Good or bad based on residual

group_resid <- factor(ifelse(abs(orig_clustering$SST.resid <= .3), "Good", "NotGood"))

orig_clustering <- data.frame(orig3[tt2, ], fit2$cluster, class_cluster, good, bad, group_resid)

#Understanding each group
table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='So-so'])>.3)

table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Bad'])>.3)

table(abs(orig_clustering$SST.resid[orig_clustering$class_cluster=='Good'])>.3)


#Decision Trees to identify what condition determine whether a point is "good" or "bad"
good_tree <- rpart::rpart(good ~ x1 + x2 + x3 + lat + sd11 + range11 + range12 + diff.med.min11 + diff.med.min12,
  data = orig_clustering, minsplit = 15)

rpart.plot::prp(good_tree, uniform=TRUE,
  main="Classification Tree for Good Clusters")

#ggp <- predict(good_tree,
#  orig_clustering,
#  type = 'class',
#  na.action = na.pass)

#ggcm <- caret::confusionMatrix(data = ggp, reference = orig_clustering$good, positive = "Good")


bad_tree <- rpart::rpart(bad ~ x1 + x2 + x3 + lat + sd11 + range11 + range12 + diff.med.min11 + diff.med.min12,
  data = orig_clustering)

rpart.plot::prp(bad_tree, uniform=TRUE,
  main="Classification Tree for Bad Clusters")


gbp <- predict(bad_tree,
  orig_clustering,
  type = 'class',
  na.action = na.pass)

gbcm <- caret::confusionMatrix(data = gbp, reference = orig_clustering$good, positive = "Bad")

#Testing whether residual falls into a residual range defined by the point the cluster fell into
orig_test <- data.frame(scaled_orig[!tt2, ], SST.resid = SST.resid[!tt2])

correctbias <- rep(999, length(predictions))

for (i in seq(1, length(predictions))) {
  medianpredict <- median(orig_clustering$SST.resid[orig_clustering$fit2.cluster == predictions[i]])
  IQRpredict <- IQR(orig_clustering$SST.resid[orig_clustering$fit2.cluster == predictions[i]])
  if (medianpredict + IQRpredict >= orig_test$SST.resid[i] & medianpredict - IQRpredict <= orig_test$SST.resid[i]) {
    correctbias[i] <- TRUE
  }
  else {
    correctbias_onedeg[i] <- FALSE
  }
}





















correctrange <- ifelse(means[predictions] - sds[predictions] <= orig_test$SST.resid[predictions] & orig_test$SST.resid[predictions] <= means[predictions] + sds[predictions], TRUE, FALSE)













removetail_tree <- rpart::rpart(group_resid ~ x1 + x5 + x6 + lat + sd11 + diff11 + diff12 + meddiff11,
  orig_clustering[orig_clustering$good == 'Good', ])

tail_removed <- predict(removetail_tree,
  orig_clustering[orig_clustering$good == 'Good', ],
  type = 'class',
  na.action = na.pass)

hist(orig_clustering$SST.resid[tail_removed == 'Good'])



bestmean <- min(means)
allpositive <- all(ifelse(means >= 0, TRUE, FALSE))
bestindex <- match(bestmean, means)
bestsd <- sds[bestindex]
bestlength <- lengths[bestindex]

sd_clusters[j] <- bestsd
means_cluster[j] <- bestmean
positive_cluster[j] <- allpositive
lengths_cluster[j] <- bestlength






















good2 <- factor(ifelse(predictions %in% good.clusters,
  'Good',
  'NotGood'))

bad2 <- factor(ifelse(predictions %in% bad.clusters,
  'Bad',
  'NotBad'))

orig_test <- data.frame(orig3[!tt1, ], predictions, good2, bad2)









#Cluster Exploration - See how many points are within 'good' window
cluster_sizes <- matrix(NA, 50, 2)
mean_good <- mean(orig_clustering$SST.resid[orig_clustering$class_cluster=='Good'])
sd_good <- sd(orig_clustering$SST.resid[orig_clustering$class_cluster=='Good'])

for (i in seq(1, 50, 1)) {
  cluster_sizes[i, 1] <- table((orig_clustering$SST.resid[orig_clustering$fit2.cluster==toString(i)])>mean_good+sd_good | (orig_clustering$SST.resid[orig_clustering$fit2.cluster==toString(i)]) < mean_good-sd_good)[1]
  cluster_sizes[i, 2] <- table((orig_clustering$SST.resid[orig_clustering$fit2.cluster==toString(i)])>mean_good+sd_good | (orig_clustering$SST.resid[orig_clustering$fit2.cluster==toString(i)]) < mean_good-sd_good)[2]
}

