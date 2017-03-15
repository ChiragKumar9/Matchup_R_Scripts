#Require necesary packages and define secant function
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


secant.deg <- function(x) {1 / (cos(rad(x)))}
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
  if (method == "standard") {
    x <- as.matrix(x)
    x_scaled <- scale(x, center = TRUE, scale = TRUE)
    return(x_scaled)
  }
  else {
    stop("Method given was not 'MAD', 'IQR', or 'standard'.")
  }
}
# ----------------------------------------------------------------------------------------

orig <- readr::read_csv("/Users/parora/Projects/MODIS/Matchups/Results/objects/MODIS_Aqua_GSFC_ALL_Class_6.4.1_ao_2016_01_23.csv")

orig_filtered <- dplyr::tbl_df(orig) %>% 
  dplyr::filter(solz >= 90)

#2: Check orig dataframe for correct filteration solz >= 90
if (min(orig_filtered$solz, na.rm = TRUE) >= 90) {
  print("Orig is filtered correctly.")
} else {
  stop("Orig is filtered incorrectly. Re-filter and try again.")
}

orig2 <- dplyr::tbl_df(orig_filtered) %>%
  dplyr::select(satz, cen.11000, cen.12000, sd.11000, sd.12000, buoy.sst, cen.ref.type.1.SST, cen.sst, buoy.lat)

# --- 3: Use dplyr to select only the variables that I will need for algorithm, don't put in intercept term ----
#orig2 <- dplyr::select(orig, satz, cen.11000, cen.12000, buoy.sst) #Collect variables that will go into SST algorithm
#orig2 <- dplyr::tbl_df(orig) %>% dplyr::select(satz, cen.11000, cen.12000, buoy.sst)
x1 <- orig2$cen.11000  										  # BT31 (brigthness temperature for channel 31)
x2 <- orig2$cen.11000 - orig2$cen.12000		  # BT31 - BT32
x3 <- orig2$cen.ref.type.1.SST						  # First Guess SST
x4 <- orig2$satz											      # satellite zenith angle (no sign)
x5 <- x2 * x3														    # BT31-BT32 * buoy SST
x6 <- (secant.deg(orig2$satz) - 1) * x2     # Secant times BT31-BT32
lat <- orig2$buoy.lat                       # Buoy Latitude
sd11 <- orig2$sd.11000                      #SD11
sd12 <- orig2$sd.12000
SST.resid <- orig2$buoy.sst - orig2$cen.sst #Residual

#x7 <- ifelse(orig2$mirror == 1, 0, 1)      # Dummy mirror variable 0 for side1, 1 for side2

orig3 <- dplyr::tbl_df(orig2) %>%
  dplyr::mutate(x1 = cen.11000,
    x2 = cen.11000 - cen.12000,
    x3 = cen.ref.type.1.SST,
    x4 = satz,
    x5 = x2 * x3,
    x6 = ((secant.deg(satz) - 1) * x2),
    SST.resid = buoy.sst - cen.sst,
    lat = buoy.lat,
    sd11 = sd.11000,
    sd12 = sd.12000) %>% 
  dplyr::select(x1, x5, x6, lat, sd11, sd12)


tt1 <- as.matrix(orig3)

scaled_orig <- scale.robust(tt1, "IQR")

#Clustering
#Elbow Plot
wss <- (nrow(scaled_orig)-1)*sum(apply(scaled_orig,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled_orig,
  centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares") 

#Actual fitting of clustering model
nclusters <- 5
fit <- kmeans(scaled_orig, nclusters) #5 cluster solution
# get cluster means
clustermeans <- aggregate(scaled_orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
tt3 <- data.frame(scaled_orig, SST.resid, qsst = orig_filtered$qsst, fit$cluster)
lattice::histogram(~SST.resid | factor(fit.cluster), data = tt3, breaks = seq(from=-12, to=30, by=1), layout = c(5,1))
orig_dt <- data.frame(tt1, SST.resid,
  qsst = orig_filtered$qsst,
  fit$cluster)
orig_dt <- data.frame(tt1,
  SST.resid = SST.resid,
  qsst = orig_filtered$qsst,
  fit$cluster,
  qual = factor(ifelse(abs(orig_dt$SST.resid) <= 1, 'Good', 'Bad')))
  #grossbad = factor(ifelse(orig_dt$fit.cluster == 4 | orig_dt$fit.cluster ==  5 | orig_dt$fit.cluster == 7 | orig_dt$fit.cluster == 13 | orig_dt$fit.cluster ==  18, "Horrible", "So-so")),
  #grossgood = factor(ifelse(orig_dt$fit.cluster == 1 |orig_dt$fit.cluster ==  9 | orig_dt$fit.cluster == 10 | orig_dt$fit.cluster == 16, "ReallyGood", "So-so")))

means <- tapply(X = orig_dt$SST.resid, INDEX = orig_dt$fit.cluster, FUN = mean)
medians <- tapply(orig_dt$SST.resid, orig_dt$fit.cluster, median)
sds <- tapply(orig_dt$SST.resid, orig_dt$fit.cluster, sd)
MADs <- tapply(orig_dt$SST.resid, orig_dt$fit.cluster, mad)
lengths <- tapply(orig_dt$SST.resid, orig_dt$fit.cluster, length)

#pp1 <- sort(sample(1:nrow(orig_dt), size = 50000, replace = FALSE))

orig_dt_2 <- orig_dt[pp1, ]

#pp2 <- sort(sample(1:nrow(orig_dt), size = 50000, replace = FALSE))

testset <- orig_dt[pp2, ]

#Decision Tree
ssttree <- rpart::rpart(qual ~ x1 + x5 + x6, data = orig_dt_2, minsplit = 30)

bogus1 <- predict(ssttree, testset,
  type = "class",
  na.action = na.pass)

bogus2 <- caret::confusionMatrix(data=bogus1, reference=orig_dt_2$qual, positive="Bad")

grossgoodtree <- rpart::rpart(grossgood ~ x1 + x5 + x6, data = orig_dt_2, minsplit = 30)

ggpredictions <- predict(grossgoodtree,
  testset,
  type = 'class',
  na.action = na.pass)

ggcm <- caret::confusionMatrix(data = ggpredictions, reference = testset$grossgood, positive = "ReallyGood")

grossbadtree <- rpart::rpart(grossbad ~ x1 + x5 + x6, data = orig_dt_2, minsplit = 30)

gbpredictions <- predict(grossbadtree,
  testset,
  type = 'class',
  na.action = na.pass)

gbcm <- caret::confusionMatrix(data = gbpredictions, reference = testset$grossbad, positive = "Horrible")

#plot(tree1, uniform=TRUE,
#  main="Classification Tree for SST.resid")
#text(tree1, use.n=TRUE, all=TRUE, cex=.8)

ssttreerf <- randomForest::randomForest(qual ~ x1 + x5 + x6, data = orig_dt_2, ntree = 1000, nodesize = 15, mtry = 3)

rfpredictions <- predict(ssttreerf, testset, type = 'class', na.action = na.pass)

rfcm <- caret::confusionMatrix(data = rfpredictions, reference = testset$qual, positive = "Bad")


#For 5 clusters, good clusters are: (5 (.77), 2(1.002), 3(1.03)) -- Means
#For 5 clusters, bad clusters are: (1 (5.8), 2 (6.7)) -- Means
#Means for qsst: 0:.19, 1:.43, 2:.87, 3:7.4, 4:3.26
#Some sort of stat on how many in good quality and good cluster are bad and how many in bad quality
#and bad cluster are good points?

#Try to separate really good points from best cluster

bestclusterrf <- randomForest::randomForest(qual ~ x1 + x5 + x6 + lat, data = subset(orig_dt_2, fit.cluster == 5 | fit.cluster == 2 | fit.cluster == 16 | fit.cluster == 18), ntree = 2000, nodesize = 15, mtry = 3)

worstclusterbestrf <- randomForest::randomForest(qual ~ x1 + x5 + x6 + lat, data = subset(orig_dt_2, fit.cluster == 12 | fit.cluster == 17 | fit.cluster == 3), ntree = 2000, nodesize = 15, mtry = 3)

bestqualityrf <- randomForest::randomForest(qual ~ x1 + x5 + x6 + lat, data = subset(orig_dt_2, qsst == 0), ntree = 2000, nodesize = 15, mtry = 3)

worstqualitybestrf <- randomForest::randomForest(qual ~ x1 + x5 + x6 + lat, data = subset(orig_dt_2, qsst == 3), ntree = 2000, nodesize = 15, mtry = 3)


#Play around with RF --
#Play around with abs() -- 
#Play around with threshold levels

#3D Plotting

pp1 <- sort(sample(1:nrow(orig_clustering), size = 50000, replace = FALSE))

orig_clustering_2 <- orig_clustering[pp1, ]

open3d()

rgl::plot3d(x = orig_clustering[abs(orig_clustering$SST.resid) <= .5, 'x1'],
  y = orig_clustering[abs(orig_clustering$SST.resid) <= .5, 'x2'],
  z = orig_clustering[abs(orig_clustering$SST.resid) <= .5, 'x3'], col='green', size=1,
  xlab = 'x1', ylab = 'x2', zlab = 'x3')

rgl::points3d(x = orig_clustering[abs(orig_clustering$SST.resid) >= 8, 'x1'],
  y = orig_clustering[abs(orig_clustering$SST.resid) >= 8, 'x2'],
  z = orig_clustering[abs(orig_clustering$SST.resid) >= 8, 'x3'], col='red', size=1, add = TRUE)

rgl::points3d(x = orig_clustering[abs(orig_clustering$SST.resid) < 8 & abs(orig_clustering$SST.resid) > .5, 'x1'],
  y = orig_clustering[abs(orig_clustering$SST.resid) < 8 & abs(orig_clustering$SST.resid) > .5, 'x2'],
  z = orig_clustering[abs(orig_clustering$SST.resid) < 8 & abs(orig_clustering$SST.resid) > .5, 'x3'], col='purple', size=1)

#rgl::points3d(x = orig_clustering[orig_clustering$qual == 'Bad', 'x1'],
#  y = orig_clustering[orig_clustering$class_cluster == 'So-so', 'x5'],
#  z = orig_clustering[orig_clustering$class_cluster == 'So-so', 'x6'], col="cyan", size=1, add = TRUE)

legend3d("topright", legend = paste(c('abs(error) <= .3', 'abs(error) > .3')), pch = 16, col = rainbow(2), cex=1, inset=c(0.01))

rgl.postscript("test1.pdf","pdf")


rgl::plot3d(x = orig_clustering_2[orig_clustering_2$qsst == '0', 'x1'],
  y = orig_clustering_2[orig_clustering_2$qsst == '0', 'x5'],
  z = orig_clustering_2[orig_clustering_2$qsst == '0', 'x6'], col='red', size=1,
  xlab = 'x1', ylab = 'x5', zlab = 'x6')

for (i in seq(1, 4, 1)) {

  rgl::points3d(x = orig_dt_2[orig_dt_2$qsst == toString(i), 'x1'],
    y = orig_dt_2[orig_dt_2$qsst == toString(i), 'x5'],
    z = orig_dt_2[orig_dt_2$qsst == toString(i), 'x6'], col=rainbow(i+1)[i+1], size=1, add = TRUE)
}

#legend3d("topright", legend = paste(c('0', '1', '2', '3', '4')), pch = 16, col = c('red', 'cyan', 'blue', 'purple', ), cex=1, inset=c(0.02))


rgl::plot3d(x = orig_dt_2[orig_dt_2$fit.cluster == '1', 'x1'],
  y = orig_dt_2[orig_dt_2$fit.cluster == '1', 'x5'],
  z = orig_dt_2[orig_dt_2$fit.cluster == '1', 'x6'], col=rainbow(1), size=1,
  xlab = 'x1', ylab = 'x5', zlab = 'x6', xlim = c(0, 25), ylim = c(-50, 100), zlim = c(-2, 4))

#rgl::points3d(x = clustermeans[, 2], y = clustermeans[, 3], z = clustermeans[, 4], col = 'pink', size = 10)

for (i in seq(2, nclusters, 1)) {
  
  rgl::points3d(x = orig_dt_2[orig_dt_2$fit.cluster == toString(i), 'x1'],
    y = orig_dt_2[orig_dt_2$fit.cluster == toString(i), 'x5'],
    z = orig_dt_2[orig_dt_2$fit.cluster == toString(i), 'x6'], col=rainbow(i)[i], size=1, add = TRUE)
}



#de <- misc3d::kde3d(orig_dt_2$x1, orig_dt_2$x5, orig_dt_2$x6, n = 100)

misc3d::contour3d(de$d, level = 10^(-4),
  x = de$x, y = de$y, z = de$z,
  color = "green", color2 = "gray", add = TRUE, smooth = TRUE)

pp2 <- as.double(de$d[de$d > 0])

#Hypervolme
diff.thresh <- c(.5, 1, 2, 5)
overlaps2 <- matrix(9999, length(diff.thresh), 1)
pp1 <- sort(sample(1:nrow(orig_clustering), size = 50000, replace = FALSE))
minx <- min(orig_clustering[pp1,]$x1)
maxx <- max(orig_clustering[pp1,]$x1)
miny <- min(orig_clustering[pp1,]$x5)
maxy <- max(orig_clustering[pp1,]$x5)
minz <- min(orig_clustering[pp1,]$x6)
maxz <- max(orig_clustering[pp1,]$x6)
axeslim <- list(x = c(minx, maxx), y = c(miny, maxy), z = c(minz, maxz))
for (i in diff.thresh) {
  group_resid <- factor(ifelse(abs(orig_clustering$SST.resid <= i), "Good", "Bad"))
  orig_clustering <- data.frame(orig3, SST.resid, qsst, fit2$cluster, class_cluster, good, bad, group_resid)

  hv1 <- hypervolume::hypervolume(subset(orig_clustering[pp1, ], group_resid=="Good")[,1:3],
    repsperpoint = 3000,
    bandwidth = 0.1,
    quantile = 0,
    name = NULL, 
    verbose = FALSE, warnings = TRUE)
  
  jpeg(paste('/Users/parora/Projects/MODIS/Figures and Assorted Results/Hypervolumes2/',toString(i), 'Good.jpeg', sep=''))
  
  plot(hv1, showcentroid = TRUE, varlims = axeslim)
  
  dev.off()

  #print(estimate_bandwidth(orig_dt_2[,1:3],method="silverman"))
  #print(estimate_bandwidth(orig_dt_2[,1:3],method="plug-in"))
  #print(estimate_bandwidth(orig_dt_2[,1:3],method="cross-validation"))

  hv2 <- hypervolume::hypervolume(subset(orig_clustering[pp1, ], group_resid=="Bad")[,1:3],
    repsperpoint = 3000,
    bandwidth = 0.1,
    quantile = 0,
    name = NULL, 
    verbose = FALSE, warnings = TRUE)
  
  jpeg(paste('/Users/parora/Projects/MODIS/Figures and Assorted Results/Hypervolumes2/',toString(i), 'Bad.jpeg', sep=''))
  
  plot(hv2, showcentroid = TRUE, varlims = axeslim)
  
  dev.off()

  hv_set <- hypervolume::hypervolume_set(hv1, hv2, check_memory = FALSE)

  overlap <- hypervolume::hypervolume_sorensen_overlap(hv_set)
  overlaps2[i, 1] <- overlap
}
#Algorithm Exploration

#Above/Below y = x line
#Summary: Decision Trees = okay, Logreg = perfect
tt2 <- 1:100
tt3 <- tt2 + rnorm(length(tt2), mean = 0, sd = 7)
plot(tt2, tt3)
abline(0,1)
qqq <- ifelse(tt3 > tt2, 1, 0)
plot(tt2[qqq == 1], tt3[qqq == 1], col = 'tomato', pch = 16)
points(tt2[qqq == 0], tt3[qqq == 0], col = 'steelblue', pch = 16)
abline(0,1)
tt4 <- data.frame(tt2, tt3, qqq = factor(qqq))
#Decision Tree
tree2 <- rpart::rpart(qqq ~ tt2 + tt3, tt4, minsplit = 5)
plot(tree2, uniform=TRUE)
text(tree2, use.n=TRUE, all=TRUE, cex=.8)

bogus1 <- predict(tree2, tt4,
  type = "class",
  na.action = na.pass)

cm <- caret::confusionMatrix(data=bogus1, reference=tt4$qqq, positive = '1')
#Random Forest
tree4_rf <- randomForest::randomForest(qqq ~ tt2 + tt3, data = tt4, ntree = 2000, nodesize = 1, mtry = 2)
#Log Reg
logreg1 <- glm(qqq ~ tt2 + tt3, family = binomial(link = 'logit'), data = tt4)

cm <- caret::confusionMatrix(data=ifelse(logreg1$fitted.values >= .5, 1, 0), reference=tt4$qqq, positive = '1')

noise <- tt3 - tt2
x <- 101*(noise) + -6.236
g_x = 1/(1+exp(-x))
plot(x, g_x)
points(x[c(6, 37, 64)], g_x[c(6, 37, 64)], col = 'tomato')
#Adding a new feature, tt5, a prediction of tt3 (random noise + tt2) based on tt2
#Decision trees = much better
tt5 <- lm(tt3 ~ tt2)
tt6 <- data.frame(tt4, tt5 = tt5$fitted.values)
tree3 <- rpart::rpart(qqq ~ tt5 + tt2, tt6, minsplit = 5)
plot(tree3, uniform=TRUE)
text(tree3, use.n=TRUE, all=TRUE, cex=.8)
tree5_rf <- randomForest::randomForest(qqq ~ tt2 + tt3 + tt5, data = tt6, ntree = 5000, nodesize = 1, mtry = 3)

logreg2 <- glm(qqq ~ tt2 + tt3 + tt5, family = binomial(link = 'logit'), data = tt6)

cm <- caret::confusionMatrix(data=ifelse(logreg2$fitted.values >= .5, 1, 0), reference=tt6$qqq, positive = '1')

#Data to more closely resemble SSTs, everything between y = x +/- 3 is a class
#Decision Trees = great, logreg = poor (everything in one class)
tt2 <- runif(n = 1000, min = 3, max = 35)
tt3 <- tt2 + rnorm(length(tt2), mean = 0, sd = 8)
plot(tt2, tt3)

resid <- ifelse(abs(tt3 - tt2) <= 3, 0, 1)
tt4 <- data.frame(tt2, tt3, tt5 = runif(n = 1000, min = 0, max = 1), resid = factor(resid))

plot(tt2, tt3, type = 'n')
points(tt2[tt4$resid == 0], tt3[tt4$resid == 0], col = 'tomato', pch = 16)
points(tt2[tt4$resid == 1], tt3[tt4$resid == 1], col = 'steelblue', pch = 16)

tree6_rf <- randomForest::randomForest(resid ~ tt2 + tt3 + tt5, data = tt4, ntree = 5000, nodesize = 10, mtry = 1)
tree6 <- rpart::rpart(resid ~ tt2 + tt3, minsplit = 10, data = tt4)

bogus6 <- predict(tree6, tt4,
  type = "class",
  na.action = na.pass)

cm <- caret::confusionMatrix(data=bogus6, reference=tt4$resid, positive = '1')

plot(tree6, uniform=TRUE)
text(tree6, use.n=TRUE, all=TRUE, cex=.8)


plot(tt2, tt3, type = 'n')
points(tt2[tt4$resid == 0], tt3[tt4$resid == 0], col = 'tomato', pch = 16)
points(tt2[tt4$resid == 1], tt3[tt4$resid == 1], col = 'steelblue', pch = 16)
abline(h = 35.12544)
abline(h = 2.07)
abline(h = 31.02)
abline(v = 29.33)
abline(v = 31)
abline(h = 2.5)
abline(h = 5.44)
abline(v = 5.2)
abline(v = 5.3)
abline(h = 30.8)
abline(v = 7.04)

#tt2 <- tt2^2
#tt3 <- tt3^2

logreg3 <- glm(resid ~ tt2 * tt3, family = binomial(link = 'logit'), data = tt4)

#logregpoly <- glm(tt4$resid ~ poly(tt4$tt2, 2, raw = TRUE) + poly(tt4$tt3, 2, raw = TRUE), family = binomial(link = 'logit'))

cm <- caret::confusionMatrix(data=ifelse(logreg3$fitted.values >= .5, 1, 0), reference=tt4$resid, positive = '1')

above3 <- table(ifelse((tt3 - tt2) > 3, 0, 1))[1]
belowneg3 <- table(ifelse((tt3 - tt2) < -3, 0, 1))[1]
inband <- table(ifelse(abs(tt3 - tt2) <= 3, 0, 1))[1]

#One vs All Regression
#Logreg = perfect
tt4 <- data.frame(tt2, tt3, resid,
  above = factor(ifelse((tt3 - tt2) > 3, 1, 0)),
  below = factor(ifelse((tt3 - tt2) < -3, 1, 0)),
  band = factor(ifelse(abs(tt3 - tt2) <= 3, 1, 0)))

abovevall <- glm(above ~ tt2 + tt3, family = binomial(link = 'logit'), data = tt4)
belowvall <- glm(below ~ tt2 + tt3, family = binomial(link = 'logit'), data = tt4)

plot(tt2, abs(tt3-tt2))
abline(h = 0)

#Multinomial Regression






#ROC

prob <- predict(logreg3, newdata=tt4, type="response")
pred <- prediction(prob, tt4$resid)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
  tpr=unlist(perf@y.values),
  model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


#Different Clustering Algorithms?
fit <- Mclust(scaled_orig)
plot(fit)
summary(fit)


#Quantifier
good.range.0.CK  <- 0
bad.range.0.CK <- 0
good.range.1.CK <- 0
bad.range.1.CK <- 0
good.range.0.SoA <- 0
bad.range.0.SoA <- 0
good.range.1.SoA <- 0
bad.range.1.SoA <- 0
good.cluster <- TRUE
for (i in seq(1, nrow(tt3))) {
  good.cluster <- tt3[i ,]$fit.cluster <=2 | tt3[i ,]$fit.cluster == 4
  if (tt3[i ,]$SST.resid <= .3 & tt3[i ,]$SST.resid >= -.3 & good.cluster) {
    good.range.0.CK <- good.range.0.CK + 1
  }
  if (tt3[i ,]$SST.resid > .3 | tt3[i ,]$SST.resid < -.3 & good.cluster) {
    bad.range.0.CK <- bad.range.0.CK + 1
  }
  if (tt3[i ,]$SST.resid <= .3 & tt3[i ,]$SST.resid >= -.3 & !good.cluster) {
    good.range.1.CK <- good.range.1.CK + 1
  }
  if (tt3[i ,]$SST.resid > .3 | tt3[i ,]$SST.resid < -.3 & !good.cluster) {
    bad.range.1.CK <- bad.range.1.CK + 1
  }
  if (tt3[i ,]$SST.resid <= .3 & tt3[i ,]$SST.resid >= -.3 & tt3[i ,]$qsst <= 2) {
    good.range.0.SoA <- good.range.0.SoA + 1
  }
  if (tt3[i ,]$SST.resid > .3 | tt3[i ,]$SST.resid < -.3 & tt3[i ,]$qsst <= 2) {
    bad.range.0.SoA <- bad.range.0.SoA + 1
  }
  if (tt3[i ,]$SST.resid <= .3 & tt3[i ,]$SST.resid >= -.3 & tt3[i ,]$qsst >= 3) {
    good.range.1.SoA <- good.range.1.SoA + 1
  }
  if (tt3[i ,]$SST.resid > .3 | tt3[i ,]$SST.resid < -.3 & tt3[i ,]$qsst >= 3) {
    bad.range.1.SoA <- bad.range.1.SoA + 1
  }
}



# Ward Hierarchical Clustering
d <- dist(tt2[5000:10000,], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 






#means <- apply(as.data.frame(orig3), 2, mean)
#sds <- apply(as.data.frame(orig3), 2, sd)
#tt3 <- (as.data.frame(orig3)[, 1] - means[1])/sds[1]



#Scaling Methods
#for (i in ncol(orig3)) {
#  for (j in nrow(orig3)) {
#    orig3[j, i] <- (orig3[j, i] - mean(orig3[[i]]))/sd(orig3[[i]]) #Mean Normalization
    #orig3[j, i] <- (orig3[j, i] - median(orig3[[i]]))/IQR(orig3[[i]]) #Median Normalization
#  }
#}
