# ---------------------------------------------------------------------------------------#
# --- Script to estimate Versionr 5 SST algorithm coefficients
# --- for MODIS Aqua or Terra matchups.
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Import neccessary packages ----
require(dplyr)
require(readr)
require(circular)
secant.deg <- function(x) {1 / (cos(rad(x)))}
# ----------------------------------------------------------------------------------------

# --- 1: Read CSV file with data (try to learn package readr) ----
orig <- readr::read_csv("/Users/parora/Projects/MODIS/Matchups/Results/Objects/MODIS_All_Data_Filtered_qsst_solz_Actual.csv")

#2: Check orig dataframe for correct filteration (qsst == 0, solz > 90), summary() command, table() command, min(na.rm = TRUE) command
if ((max(orig$qsst, na.rm = TRUE) == 0) && (min(orig$solz, na.rm = TRUE) > 90)) {
  print("Orig is filtered correctly.")
} else {
  stop("Orig is filtered incorrectly. Re-filter and try again.")
}
# ---------------------------------------------------------------------------------------

orig2 <- dplyr::tbl_df(orig) %>% 
  dplyr::filter(qsst == 0 && solz > 90) %>%
  dplyr::select(satz, cen.11000, cen.12000, buoy.sst, cen.ref.type.1.SST)

# --- 3: Use dplyr to select only the variables that I will need for algorithm, don't put in intercept term ----
#orig2 <- dplyr::select(orig, satz, cen.11000, cen.12000, buoy.sst) #Collect variables that will go into SST algorithm
#orig2 <- dplyr::tbl_df(orig) %>% dplyr::select(satz, cen.11000, cen.12000, buoy.sst)
x1 <- orig2$cen.11000  										  # BT31 (brigthness temperature for channel 31)
x2 <- orig2$cen.11000 - orig2$cen.12000		  # BT31 - BT32
x3 <- orig2$buoy.sst											  # Buoy SST
x4 <- orig2$satz											      # satellite zenith angle (no sign)
x5 <- x2 * x3														    # BT31-BT32 * buoy SST
x6 <- (secant.deg(orig2$satz) - 1) * x2     # Secant times BT31-BT32
#x7 <- ifelse(orig2$mirror == 1, 0, 1)      # Dummy mirror variable 0 for side1, 1 for side2

orig3 <- dplyr::tbl_df(orig2) %>%
  dplyr::mutate(x1 = cen.11000,
    x2 = cen.11000 - cen.12000,
    x3 = cen.ref.type.1.SST,
    x4 = satz,
    x5 = x2 * x3,
    x6 = (secant.deg(satz) - 1 * x2)) %>% 
  dplyr::select(buoy.sst, x1, x5, x6)

xreg <- lm(buoy.sst ~ x1 + x5 + x6,
  data = orig3,
  na.action=na.omit)

proportion.in.train.set <- .5
row.numbers <- seq(from = 1, to = nrow(orig3), by = 1)
size.train.set <- floor(proportion.in.train.set * nrow(orig3))
set.seed(256)
train.set.row.numbers <- sort(sample(row.numbers, size = size.train.set, replace = FALSE))
tt1 <- row.numbers %in% train.set.row.numbers
train.set <- orig3[tt1, ]
test.set <- orig3[!tt1, ]


xreg2 <- lm(buoy.sst ~ x1 + x5 + x6,
  data = train.set,
  na.action=na.omit)

xreg2.results <- predict(object = xreg2, newdata = test.set)
