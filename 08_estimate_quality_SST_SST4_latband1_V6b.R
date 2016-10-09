


# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- PERFORM INDIVIDUAL QUALITY TESTS
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- BIT 2 - SSTF_BTRANGE ----
# --- Range of brightness temperatures.
# --- The range of acceptable values is narrower than for AVHRR (-4 to 33 degC).

# Sue: upper valid range for all BT's is now 37 during the day and 37 at night
#      and check Bt37, 39 and 40 for sst and sst4
#      Bob says Bt40 may need a lower upper limit so 35 for now
# GP: ch12 is not checked?
# KAK: Cannot test the short wave bands BT in glint area during the day
# they will always fail even when longwave are good, written May 2013.

#*********
# CAUTION FYI NOTE by KAK about glint in OLD format matchups
# versus NEW format (because I can't remember squat):
#***********
# the OLD FORMAT glint value was taken from the wifers glint flag which was set
# if glint was > 0.0001 for ocean color.
# This is a more stringent than MIAMI' glint test for sst of 0.005.
# Glint with a threshold below 0.005 can be found in the wifers hglint flag.
# However, we do not have the wifers hglint flag available in the OLD format, so we will
# use the wifers more restrictive glint test for the time being.
# It is better to NOT set the BTrange in the wider glint region.
# The R tests below use orig$glint < 0.005 it will fortuitively work with the OLD format
# because the glint value will be 0 (not set) for glint < 0.001.
# If this R script is used with NEW format matchups containing the actual glint value
# (not a clear flag) the Rcode will also work as intended.

# Should the "ors" be "ands" in 46-47 and 52-53?

SSTF.BTBAD <- !is.na(orig$cen.11000) & !is.na(orig$cen.12000)  &
  abs(orig$cen.11000) < 999 & abs(orig$cen.12000) < 999

SST4.BTBAD <- !is.na(orig$cen.4050) &
  !is.na(orig$cen.3750) &
  !is.na(orig$cen.3959) & 
  abs(orig$cen.3959)  < 999 &
  abs(orig$cen.3750) < 999 &
  abs(orig$cen.3959) < 999 &
  abs(orig$cen.4050) < 999

table(SSTF.BTBAD, useNA="always")
table(SST4.BTBAD, useNA="always")

# --- BT range checks...
# --- Updated 28 Dec 2013 after long discussions with Kay and Sue.
# --- The latest version was developed by Kay inmore steps but easier
# --- to understand and verify.

# --- Make sure the BT's are valid so that we don't have any "NA's in the final aggregated flag 
BT_valid <- !is.na(orig$cen.4050) & 
  !is.na(orig$cen.3959) & 
  !is.na(orig$cen.3750) & 
  !is.na(orig$cen.11000) & 
  !is.na(orig$cen.12000) 

# --- Nite test: test is true if 11,12, and 3-4um bands are within the valid range 
BT_range_night <- (orig$solz >= 90) & 
  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) & 
  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) & 
  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# --- Day test outside of glint
# --- eg. low glint value test true if the 11,12um bands are within range and
# --- 3-4um band are not cold 
BT_range_day_low_glint <-  ((orig$solz < 90 & orig$glint < 0.005) & 
  (orig$cen.3750 >= -4.0) & 
  (orig$cen.3959 >= -4.0) & 
  (orig$cen.4050 >= -4.0)) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# --- Day test in high glint region where we can't do any test on the 3-4um values.
# True is 11, 12um are within range 
BT_range_day_high_glint <- (orig$solz < 90 & orig$glint > 0.005) & 
  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) & 
  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) 

# if any flag is true and BT's are valid then all is well and BT_range_kay is true 
# in english must have a BT that is not "NA" and any test night,
# or low glint, or high glint is true. 

SSTF.BTRANGE <- BT_valid & (BT_range_night | BT_range_day_low_glint | BT_range_day_high_glint) 

table(SSTF.BTRANGE)
rm(BT_valid, BT_range_night, BT_range_day_low_glint, BT_range_day_high_glint); gc()

# This is what is now in l2gen
#SSTF.BTRANGE3 <- !is.na(orig$cen.4050) &
#  !is.na(orig$cen.3959) &
#  !is.na(orig$cen.3750) &
#  !is.na(orig$cen.11000) &
#  !is.na(orig$cen.12000) &
#  (orig$cen.11000 >= -4.0) & (orig$cen.11000 <= 37.0) &
#  (orig$cen.12000 >= -4.0) & (orig$cen.12000 <= 37.0) &
#  (((orig$solz >= 90) &
#  (orig$cen.3750 >= -4.0) & (orig$cen.3750 <= 37.0) &
#  (orig$cen.3959 >= -4.0) & (orig$cen.3959 <= 37.0) &
#  (orig$cen.4050 >= -4.0) & (orig$cen.4050 <= 35.0)) |
#  (((orig$solz < 90 & orig$glint < 0.005) &
#  (orig$cen.3750 >= -4.0) &
#  (orig$cen.3959 >= -4.0) & 
#  (orig$cen.4050 >= -4.0)) |
#  (orig$solz < 90 & orig$glint > 0.005)))
  
#table(SSTF.BTRANGE3, useNA="always")

SST4.BTRANGE <- (!is.na(orig$cen.4050) &
  !is.na(orig$cen.3959)) &
  ((orig$cen.3959 >= -4.0) &
  (orig$cen.3959 <= 37.0) &
  (orig$cen.4050 >= -4.0) &
  (orig$cen.4050 <= 35.0))

table(SSTF.BTRANGE, useNA="always")
table(SST4.BTRANGE, useNA="always")
gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 3 - SSTF_BTDIFF and SST4_BTDIFF ----
# --- Range of LWIR brightness temperatures.

SSTF.BTDIFF <- !is.na( orig$cen.11000) &
  !is.na(orig$cen.12000) &
  ((orig$cen.11000 - orig$cen.12000) >= 0 &
  (orig$cen.11000 - orig$cen.12000) <= 3.6)

# --- The same, for SWIR channels

SST4.BTDIFF <- !is.na(orig$cen.4050) & !is.na(orig$cen.3959) &
  ((orig$cen.3959 - orig$cen.4050) >= 0) &
  ((orig$cen.3959 - orig$cen.4050) <= 8.0)

table(SSTF.BTDIFF, useNA="always")
table(SST4.BTDIFF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 4 - SSTF_RANGE ----
# --- SST retrievals have to be within geophysically reasonable values.
# Sue: SST and SST4 max is now 40 during the day and 37 at night
# GP: added tests for non-missing SST and SST4.
# GP: Fixed "night" definition to "solz >= 90", Dec 2013.

SSTF.SSTRANGE <- !is.na(orig$SST.latband1) & ((orig$SST.latband1 >= -2.0) &
  (((orig$solz < 90.0) & (orig$SST.latband1 <= 40.0)) |
  ((orig$solz >= 90.0) & (orig$SST.latband1 <= 37.0))))

SST4.SSTRANGE <- !is.na(orig$SST4.latband1) &
  (orig$SST4.latband1 >= -2.0 & 
  ((orig$solz < 90.0 & orig$SST4.latband1 <= 40.0) |
  (orig$solz >= 90.0 & orig$SST4.latband1 <= 37.0) ) )

table(SSTF.SSTRANGE, useNA="always")
table(SST4.SSTRANGE, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 5 - SSTF_SSTREFDIFF ----
# --- Difference between satellite SST and "reference SST" .
# --- Reference SST must be defined (there may be more than one in a  matchup record). 
# --- GSFC says it uses the ABSOLUTE value of the difference... is that so?
# GP: added test for non-missing SST4
# GP: definition of "night" fixed to solz >= 90

refsst <- orig$ref.type.1.SST      # Reynolds reference SST
#refsst <- orig$ref.type.2.SST		# AMSR reference SST

tt1 <- (refsst > -900 & !is.na(refsst))
tt2 <- (orig$SST.latband1  - refsst) >= -3.0
tt3 <- (orig$solz < 90) | ((orig$solz >= 90.0) &
  (orig$SST.latband1 - refsst) <= 5.0)

tt4 <- (orig$sat.lat > 30 | orig$sat.lat < -10)
tt5 <- (orig$sat.lon < -105 | orig$sat.lon > 105)
table(orig$sat.lon < -105)
table(orig$sat.lon > 105)
table(tt5)
outbox <- tt4 | tt5
goodoutbox <- tt1 & tt2 & tt3 & (tt4 | tt5) & !is.na(orig$SST.latband1)
table(goodoutbox)

rm(tt4,tt5)
tt1 <- (refsst > -900 & !is.na(refsst))
tt2 <- (orig$SST.latband1  - refsst) >= -1.25
tt3 <- (orig$solz <= 90) | ((orig$solz > 90.0) &
  (orig$SST.latband1 - refsst) <= 5.0) # day or night and less than 5
tt4.2 <- (orig$sat.lat <= 30 & orig$sat.lat >= -10)
tt5.2 <- (orig$sat.lon >= -105 & orig$sat.lon <= 105)
table(orig$sat.lon >= -105)
table(orig$sat.lon <= 105)
table(tt5.2)
inbox <- tt4.2 & tt5.2
goodinbox <- tt1 & tt2 & tt3 & tt4.2 & tt5.2 & !is.na(orig$SST.latband1)
table(goodinbox)

SSTF.SSTREFDIFF <- goodoutbox | goodinbox

tt1 <- (refsst > -900 & !is.na(refsst))
tt2 <- (orig$SST4.latband1  - refsst) >= -3.0
tt3 <- (orig$solz >= 90.0) & ((orig$SST4.latband1 - refsst) <= 5.0) 
tt4 <- (orig$sat.lat > 30 | orig$sat.lat < -10)
tt5 <- (orig$sat.lon < -105 | orig$sat.lon > 105)
goodoutbox <- tt1 & tt2 & tt3 & (tt4 | tt5) & !is.na(orig$SST4.latband1)

# Threshold for test in tt2 is to apply a more stringent cold test
# in a geographic box likely to contain dust.

tt1 <- (refsst > -900 & !is.na(refsst))
tt2 <- (orig$SST4.latband1 - refsst) >= -1.25
tt3 <- ((orig$solz >= 90.0) & (orig$SST4.latband1 - refsst) <= 5.0) # just check night less than 5 for the SST4 product
tt4 <- (orig$sat.lat <= 30 & orig$sat.lat >= -10)
tt5 <- (orig$sat.lon >= -105 & orig$sat.lon <= 105)
goodinbox <- tt1 & tt2 & tt3 & tt4 & tt5 & !is.na(orig$SST4.latband1)

SST4.SSTREFDIFF <- goodoutbox | goodinbox

table(SSTF.SSTREFDIFF, useNA="always")
table(SST4.SSTREFDIFF, useNA="always")

rm(refsst,tt1,tt2,tt3,tt4,tt5,goodoutbox,goodinbox); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 6 -  SST4DIFF ----
# --- Absolute value of the difference between the long-wave and short-wave SSTs (SST - SST4). 
# Check for too many values that pass

SSTF.SST4DIFF <- !is.na(orig$SST.latband1) &
  !is.na(orig$SST4.latband1) &
  ((orig$solz < 90) |
  ((orig$solz >= 90) & (abs(orig$SST.latband1 - orig$SST4.latband1) <= 0.8)) )

table(SSTF.SST4DIFF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 7 -  SST4_VDIFF ----
# --- VERY large differences in abs(SST - SST4).

SSTF.SST4VDIFF <- !is.na(orig$SST.latband1) &
  !is.na(orig$SST4.latband1) & 
  ((orig$solz < 90) |
  ((orig$solz >= 90) & (abs(orig$SST.latband1 - orig$SST4.latband1) <= 1.0)) )

table(SSTF.SST4VDIFF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 8 - SSTF_BTNONUNIF ----
# --- Range of differences in BT31 and BT32 inside window. 

SSTF.BTNONUNIF <- !is.na(orig$max.11000) &
  !is.na(orig$min.11000)  &
  !is.na(orig$max.12000) &
  !is.na(orig$min.12000) &
  (((orig$max.11000 - orig$min.11000) <= 0.7) &
     ((orig$max.12000 - orig$min.12000) <= 0.7))

# --- Range of differences in BT22 and BT23 inside window

SST4.BTNONUNIF <- !is.na(orig$max.3959) &
  !is.na(orig$min.3959) &
  !is.na(orig$max.4050) &
  !is.na(orig$min.4050) &
  ((orig$max.3959 - orig$min.3959) <= 0.7 &
  (orig$max.4050 - orig$min.4050) <= 0.7)

table(SSTF.BTNONUNIF, useNA="always")
table(SST4.BTNONUNIF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 9 - SSTF_BTVNONUNIF ----
# --- Range of differences in BT31 and BT32 inside window are VERY spatially non-uniform

SSTF.BTVNONUNIF <- !is.na(orig$max.11000) &
  !is.na(orig$min.11000)  &
  !is.na(orig$max.12000) &
  !is.na(orig$min.12000) &
  ((orig$max.11000 - orig$min.11000) <= 1.2 &
  (orig$max.12000 - orig$min.12000) <= 1.2)

# --- Range of differences in BT22 and BT23 inside window

SST4.BTVNONUNIF <- !is.na(orig$max.3959) &
  !is.na(orig$min.3959)  &
  !is.na(orig$max.4050) &
  !is.na(orig$min.4050) &
  ((orig$max.3959 - orig$min.3959) <= 1.2 &
  (orig$max.4050 - orig$min.4050) <= 1.2)

table(SSTF.BTVNONUNIF, useNA="always")
table(SST4.BTVNONUNIF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 10 - SSTF_BT4REFDIFF ----
# --- This test compares the difference between BTs for the SWIR channels (22 and 23)
# --- versus a "reference"value that is estimated as a function of satellite zenith angle.
# --- The difference has been already computed and is stored in the matchups
# --- in field "cen.39.40.ref.new.

SSTF.BT4REFDIFF <- !is.na(orig$cen.39.40.ref.new) &
  ((orig$cen.39.40.ref.new >= -1.1) &
     (orig$cen.39.40.ref.new <= 10.0))

table(SSTF.BT4REFDIFF, useNA="always")
# ----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------#
# --- BIT 11: Proxy for BIT 11 REDNONUNIF was moved further down in R script
#             as all dependencies have not been set at this point. KAK.
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 12 --- High satellite zenith angles - SSTF_HISENZ ----
# --- Check for absoloute values of satellite zenith angle <= 55 degrees.

SSTF.HISENZ <- abs(orig$satz) <= 55.0

table(SSTF.HISENZ, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 13 --- Very high satellite zenith angles  -  SSTF_VHISENZ ----

SSTF.VHISENZ <- abs(orig$satz) <= 75.0

table(SSTF.VHISENZ, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- BIT 14 ---- Diff between satellite SST and "reference SST" - SSTF_SSTREFVDIFF ----
# --- GSFC says it uses the ABSOLUTE value of the difference... is that so?

# NOTE: the threshold in the GSFC doc is -6
# GP : added check for SST4 not missing


refsst <- orig$ref.type.1.SST       # Reynolds reference SST
#refsst <- orig$ref.type.2.SST  	  # AMSR reference SST

SSTF.SSTREFVDIFF <- (!is.na(orig$SST.latband1) & !is.na(refsst)) &
  (refsst > -900) &
  ((orig$SST.latband1 - refsst) >= -5.0) &
  ((orig$solz < 90.0) | ((orig$solz >= 90.0) & orig$SST.latband1 - refsst <= 5.0))

SST4.SSTREFVDIFF <- (!is.na(orig$SST4.latband1) & !is.na(refsst)) &
  (refsst > -900) & 
  ((orig$SST4.latband1  - refsst) >= -5.0) &
  ((orig$solz < 90.0) | ( (orig$solz >= 90.0) & orig$SST4.latband1 - refsst <= 5.0))

table(SSTF.SSTREFVDIFF, useNA="always")
table(SST4.SSTREFVDIFF, useNA="always")  
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- *** PROVISIONAL proxy for OLD matchups from REDNONUNIFORM *** ----

# --- BIT 11 - REDNONUNIF
# --- This test is only valid for daytime, and therefore only relevant to the long-wave SST product. 
# --- Top-of-atmosphere reflectance (rho_t) in the 678-nm band (MODIS band 14) is computed over
# --- the 3x3 pixel area centered on the pixel of interest, where rho_t = pi*Lt/F0/(mu0 t t0 toz) and
# --- Lt is observed TOA radiance, F0 is band-averaged solar irradiance (at day of year),
# --- mu0 is cosine of solar zenith angle,
# --- t0 and t are the diffuse transmittance through a Rayleigh atmosphere (solar path and sensor path), and
# --- toz is the ozone transmittance (inbound and outband).
# --- If the the difference between the maximum value and the minimum value of rho_t in the 9-pixel set
# --- exceeds 0.01, the bit is set.
# --- This bit is also set if 8 or more of the 9 pixels are saturated in the 678-nm band.
# --- In general, such saturation might indicate the presence of clouds, but it may also
# --- indicate the presence of sun glint. 
# --- The long-wave SST is affected by clouds (SST retrieval appears colder than normal),
# --- but not by sun glint. 
# --- To recover the sun glint case, the REDNONUNIF bit is only set if the retrieved SST
# --- is more than 1 deg-C colder than the reference. This secondary requirement works best
# --- in locations with temporally and spatially stable SST conditions, where the low-resolution
# --- sstref and the retrieved SST can be expected to be consistent.
# --- The saturation test is a much more stringent test than the original uniformity test.
# --- The new test is can be summarized as:
# --- set if red band reflectance in the pixel neighborhood is (saturated OR spatially nonuniform) AND
# --- SST retrieval is cold relative to the reference.

# NOTE We will add quantities to the matchups so that this test can be calculated 4 Feb 2011
# we need rho.max and rho.min in matchups
# SSTF.REDNONUNIF <-  (orig$SST.latband1  - orig$reysst) > -1.0 & ((orig$solz > 90.0) | ((orig$solz <= 90.0) & (rho.max - rho.min) <= 0.01)
# KAK note: if you use the NEW format matchups you can just check the rednonunif sstflag bit 
#  e.g. using logical "and" if (sst_flgs and 2048) is True then the rednonunif is bad
#
# SSTF.REDNONUNIF <- (orig$solz > 90.0) | ((orig$solz <= 90.0) & orig$qsst < 2) 
#
# KAK wrote May 2013: the above proxy for SSTF.REDNONUNIF needed to be corrected.
# Pixels can be a 2 for reasons other than bad rednonuniform.
# New logic:
# If a pixel is a ql 2 and the BTNONUNIF is good, then the the RED uniformity must have been bad
# else pixel would have been either a 1 or 0. 
# If a pixel is a ql 3 and passes all the quality 2 tests AND the BTNONUNIFORMITY,
# then it must have been a quality 2 with REDuniformity bad and so demoted to a ql 3.

tt1 <- (orig$qsst == 2 & SSTF.BTNONUNIF)
table(tt1, useNA = "always")

tt2 <- (orig$qsst == 3 &
  orig$img.SST.tree.class == "P-Good" &
  SSTF.BTBAD &
  SSTF.BTRANGE & SSTF.SSTRANGE & 
  SSTF.BTVNONUNIF & SSTF.SSTREFVDIFF &
  SSTF.VHISENZ & SSTF.BTNONUNIF &
  orig$glint < 0.005)
table(tt2, useNA = "always")

RED.UNIF.SET.OLD.TRUE <- tt1 | tt2
table(RED.UNIF.SET.OLD.TRUE, useNA = "always")

SSTF.REDNONUNIF <- (orig$solz >= 90.0) |
  ((orig$solz < 90.0) & !RED.UNIF.SET.OLD.TRUE) # night or day and old reduniformity is NOT bad so all is well

table(SSTF.REDNONUNIF, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# KAK: NEW L2_flg BIT 8  HIGH red reflectance day ----

# OLD format Proxy for high Rhot not in available in the OLD format matchups,
# but can be found in the NEW matchups in L2_flag bit 8.
# (NEW format should change Rscript to  use logical "and" with L2_flg).
# We need the high Rhot information for demotion if day time and sstrefdiff is < -1
# using the original quality from v5 processing, in non glint areas only, find pixels that
# were demoted because of high red reflectance. 
# logic: if a pixel is not in the glint and passed ALL the quality tests for the level above
# (ex. a ql 1 pixel that passes all of the ql0 tests) 
# then it must have been demoted because of the high red reflectance test

SSTF.DEMOTED.REDHIGH.TRUE <- (orig$solz < 90.0) & (orig$SST.latband1 - refsst) < -1 &
  ((orig$qsst == 1 & orig$img.SST.tree.class == "P-Good" & SSTF.BTBAD &
  SSTF.BTRANGE & SSTF.SSTRANGE & SSTF.SSTREFVDIFF & SSTF.SSTREFDIFF & SSTF.BTVNONUNIF &
  SSTF.BTNONUNIF & SSTF.VHISENZ & SSTF.HISENZ & SSTF.REDNONUNIF & orig$glint < 0.005) |
  (orig$qsst == 2 & orig$img.SST.tree.class == "P-Good" & SSTF.BTBAD & SSTF.BTRANGE & SSTF.SSTRANGE & 
  SSTF.REDNONUNIF & SSTF.BTVNONUNIF & SSTF.SSTREFVDIFF &
  SSTF.SSTREFDIFF & SSTF.VHISENZ & orig$glint < 0.005) |
  (orig$qsst == 3 & orig$img.SST.tree.class == "P-Good" & SSTF.BTBAD & SSTF.BTRANGE & SSTF.SSTRANGE & 
  SSTF.BTVNONUNIF & SSTF.SSTREFVDIFF & SSTF.VHISENZ & orig$glint < 0.005)) # if true pixel QL will need to be demoted will need to be demoted
  
table(SSTF.DEMOTED.REDHIGH.TRUE, useNA="always")
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- A few sanity checks ----

tt1 <- (length(SSTF.SSTREFVDIFF[SSTF.SSTREFVDIFF]) > length(SSTF.SSTREFDIFF[SSTF.SSTREFDIFF]))
if(!tt1) {stop("ERROR in tests for differences betweeen SST and reference field...\n")}

tt1 <- (length(SSTF.VHISENZ[SSTF.VHISENZ]) > length(SSTF.HISENZ[SSTF.HISENZ]))
if(!tt1) {stop("ERROR in tests for large satellite zenith angles...\n")}

rm(tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Construct data frames with results of quality tests for SST and SST4 ----

tt1 <- ifelse((orig$solz >= 90), "Night", "Day")

SST.quality.tests <- data.frame(day.or.nite = tt1,
  btbad  = SSTF.BTBAD,
  btrange = SSTF.BTRANGE,
  btdiff = SSTF.BTDIFF,
  sstrange = SSTF.SSTRANGE,
  sstrefdiff = SSTF.SSTREFDIFF,
  sstrefvdiff = SSTF.SSTREFVDIFF,  
  sst4diff = SSTF.SST4DIFF,
  sst4vdiff = SSTF.SST4VDIFF,
  btnonunif = SSTF.BTNONUNIF,
  btvnonunif = SSTF.BTVNONUNIF,
  bt4refdiff = SSTF.BT4REFDIFF,
  rednonunif = SSTF.REDNONUNIF,
  hisenz = SSTF.HISENZ,
  vhisenz = SSTF.VHISENZ,
  sstrefvdiff = SSTF.SSTREFVDIFF,
  demoted.redhigh.true = SSTF.DEMOTED.REDHIGH.TRUE,
  qsst.new = 4)

# --- Note that the SST4 tests include some of the SST tests.

SST4.quality.tests <- data.frame(day.or.nite = tt1,
  btbad  = SST4.BTBAD,
  btrange = SST4.BTRANGE,
  sstrange = SST4.SSTRANGE,
  sstrefdiff = SST4.SSTREFDIFF,
  sstrefvdiff = SST4.SSTREFVDIFF,
  btdiff = SST4.BTDIFF,
  btnonunif = SST4.BTNONUNIF,
  btvnonunif = SST4.BTVNONUNIF,
  hisenz = SSTF.HISENZ,
  vhisenz = SSTF.VHISENZ,
  sst4diff = SSTF.SST4DIFF,
  sst4vdiff = SSTF.SST4VDIFF,
  bt4refdiff = SSTF.BT4REFDIFF,
  qsst4.new = 4)

rm(tt1); gc()
# ----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#
# --- Now use the results from individual quality tests
# --- to build quality levels.
# ---------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------------#
# --- SST NIGHT Quality levels ----

# --- Quality 0

tt0 <- SST.quality.tests[, "day.or.nite"] =="Night" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[, "btbad"] &
  SST.quality.tests[, "btrange"] &
  SST.quality.tests[, "sstrange"] &
  SST.quality.tests[, "bt4refdiff"] &
  SST.quality.tests[, "sstrefvdiff"] &
  SST.quality.tests[, "sstrefdiff"] & 
  SST.quality.tests[, "btvnonunif"] &
  SST.quality.tests[, "btnonunif"] &
  SST.quality.tests[, "sst4vdiff"] &
  SST.quality.tests[, "sst4diff"] &
  SST.quality.tests[, "vhisenz"] & 
  SST.quality.tests[, "hisenz"]

table(tt0, useNA = "always")

SST.quality.tests[tt0, "qsst.new"] <- 0

# --- Quality 1

tt1 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[, "day.or.nite"] =="Night" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[, "btbad"] &
  SST.quality.tests[, "btrange"] &
  SST.quality.tests[, "sstrange"] &
  SST.quality.tests[, "bt4refdiff"] &
  SST.quality.tests[, "sstrefvdiff"] &
  SST.quality.tests[, "sstrefdiff"] & 
  SST.quality.tests[, "btvnonunif"] &
  SST.quality.tests[, "btnonunif"] &
  SST.quality.tests[, "sst4vdiff"] &
  SST.quality.tests[, "vhisenz"]

table(tt1, useNA="always")

SST.quality.tests[tt1, "qsst.new"] <- 1

# --- Quality 2

tt2 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[,"day.or.nite"] =="Night" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[, "btbad"] &
  SST.quality.tests[, "btrange"] &
  SST.quality.tests[, "sstrange"] &
  SST.quality.tests[, "bt4refdiff"] &
  SST.quality.tests[, "sstrefvdiff"] &
  SST.quality.tests[, "btvnonunif"] &
  SST.quality.tests[, "vhisenz"]

table(tt2, useNA="always")

SST.quality.tests[tt2, "qsst.new"] <- 2

# --- Quality 3

# Sue: should cloud (decision tree failed) be here? yes if P-Bad then they are a 3 -K.A.K may 2013

tt3 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[, "day.or.nite"] =="Night" &
  SST.quality.tests[, "btbad"]

SST.quality.tests[tt3, "qsst.new"] <- 3

# --- Quality 4
# --- (for completeness, so that it matches the actual l2gen code; land and BT bad are ql=4)

#tt4 <- SST.quality.tests[, "qsst.new"] == 4 &
#  SST.quality.tests[, "day.or.nite"] == "Night" &
#  (!SST.quality.tests[, "btbad"])

#if (any(tt4, na.rm = TRUE)) {
#  # All tt4 values are FALSE or missing
#  SST.quality.tests[tt4, "qsst.new"] <- 4
#} 

table(SST.quality.tests[, "qsst.new"], useNA="always")


# --- Reduce previously determined SST quality levels if SST4 is not spatially uniform
# --- (only for V5 quality no longer done this way for V6 -K.A.K may 2013)

#tt4 <- SST.quality.tests[, "qsst.new"] <  3 &
#  SST.quality.tests[,"day.or.nite"] =="Night"  &
#  !SST4.quality.tests[,"btnonunif"]
#SST.quality.tests[tt4, "qsst.new"] <- SST.quality.tests[tt4, "qsst.new"] + 1

# --- End of SST NIGHT Quality levels
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- SST DAY Quality levels ----

# --- Quality 0

# Sue: need to add glint < 0.005 to this, so it's quality one if it's in the glint area
# because we can't do all the quality tests in the glint
# region so suspect pixels could sneak in.

tt0 <- SST.quality.tests[, "day.or.nite"] == "Day" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[,"btbad"] &
  SST.quality.tests[,"btrange"] &
  SST.quality.tests[,"sstrange"] &
  SST.quality.tests[,"rednonunif"] &
  SST.quality.tests[,"btvnonunif"] &
  SST.quality.tests[,"btnonunif"] &  
  SST.quality.tests[,"sstrefvdiff"] &
  SST.quality.tests[,"sstrefdiff"] &
  SST.quality.tests[,"vhisenz"] &
  SST.quality.tests[,"hisenz"] &
  orig[, "glint"] == 0  

SST.quality.tests[tt0, "qsst.new"] <- 0

# --- Quality 1

tt1 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[, "day.or.nite"] =="Day" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[, "btbad"] &
  SST.quality.tests[, "btrange"] &
  SST.quality.tests[, "sstrange"] &
  SST.quality.tests[, "rednonunif"] &
  SST.quality.tests[, "btvnonunif"] &
  SST.quality.tests[, "sstrefvdiff"] &
  SST.quality.tests[, "sstrefdiff"] &
  SST.quality.tests[, "vhisenz"]

SST.quality.tests[tt1, "qsst.new"] <- 1

# --- Quality 2

tt2 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[, "day.or.nite"] == "Day" &
  orig[, "img.SST.tree.class"] == "P-Good" &
  SST.quality.tests[, "btbad"] &
  SST.quality.tests[, "btrange"] &
  SST.quality.tests[, "sstrange"] &
  SST.quality.tests[, "btvnonunif"] &
  SST.quality.tests[, "sstrefvdiff"] &
  SST.quality.tests[, "vhisenz"]

SST.quality.tests[tt2, "qsst.new"] <- 2

# --- Quality 3

tt3 <- SST.quality.tests[, "qsst.new"] == 4 &
  SST.quality.tests[, "day.or.nite"] == "Day" &
  SST.quality.tests[, "btbad"]

SST.quality.tests[tt3, "qsst.new"] <- 3

# --- Quality 4  (for completeness so that it matches the actual l2gen code BTBAD or land)

#tt4 <- SST.quality.tests[, "qsst.new"] == 4 &
#  SST.quality.tests[, "day.or.nite"] == "Day" &
#  !SST.quality.tests[, "btbad"]

#SST.quality.tests[tt4, "qsst.new"] <- 4

# --- In the original code there is a decrement of quality that uses redtest
# --- we cannot do it here.
# If red band reflectance is high AND SST is cold only in non-glint areas, then decrement quality
# see new high rht proxy code by KAK may 2013 DEMOTED.REDHIGH.TRUE near line 308 

tt5 <- SST.quality.tests[, "demoted.redhigh.true"] &
  SST.quality.tests[, "qsst.new"] < 3 &
  SST.quality.tests[, "day.or.nite"] == "Day"

SST.quality.tests[tt5, "qsst.new"] <- SST.quality.tests[tt5, "qsst.new"] + 1

# --- End of SST DAY Quality levels
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- calculate tables of SST quality levels ----
# --- once SST qualities have been defined for day and night,

addmargins(xtabs(~ SST.quality.tests[, "qsst.new"]))
round(prop.table(xtabs(~ SST.quality.tests[, "qsst.new"])), 4) * 100

# --- Separate day and night quality levels for SST

addmargins(xtabs(~ SST.quality.tests[, "day.or.nite"] +
  SST.quality.tests[, "qsst.new"] ))

round(prop.table(xtabs(~ SST.quality.tests[, "day.or.nite"] +
  SST.quality.tests[, "qsst.new"])), 4) * 100
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- SST4 NIGHT Quality levels ----

# --- Quality 0

tt0 <- SST4.quality.tests[, "day.or.nite"] =="Night" &
  ( orig[, "img.SST4.tree.class"] == "P-Good" &
    SST4.quality.tests[, "btbad"] &
    SST4.quality.tests[, "btrange"] &
    SST4.quality.tests[, "sstrange"] &
    SST4.quality.tests[, "bt4refdiff"] &
    SST4.quality.tests[, "sstrefvdiff"] &
    SST4.quality.tests[, "sstrefdiff"] &
    SST4.quality.tests[, "btvnonunif"] &
    SST4.quality.tests[, "btnonunif"] &
    SST4.quality.tests[, "sst4vdiff"] &
    SST4.quality.tests[, "sst4diff"] &
    SST4.quality.tests[, "vhisenz"] &
    SST4.quality.tests[, "hisenz"]
  )

SST4.quality.tests[tt0, "qsst4.new"] <- 0

# --- Quality 1

tt1 <- SST4.quality.tests[, "qsst4.new"] == 4 &
  SST4.quality.tests[,"day.or.nite"] == "Night" &
  ( orig[, "img.SST4.tree.class"] == "P-Good" & 
    SST4.quality.tests[, "btbad"] &
    SST4.quality.tests[, "btrange"] &
    SST4.quality.tests[, "sstrange"] &
    SST4.quality.tests[, "bt4refdiff"] &
    SST4.quality.tests[, "sstrefvdiff"] &
    SST4.quality.tests[, "sstrefdiff"] &
    SST4.quality.tests[, "btvnonunif"] &
    SST4.quality.tests[, "sst4vdiff"] &
    SST4.quality.tests[, "vhisenz"]
  )

SST4.quality.tests[tt1, "qsst4.new"] <- 1

# --- Quality 2

tt2 <- SST4.quality.tests[, "qsst4.new"] == 4 &
  SST4.quality.tests[,"day.or.nite"] =="Night" &
  ( orig[, "img.SST4.tree.class"] == "P-Good" & 
    SST4.quality.tests[,"btbad"] &
    SST4.quality.tests[,"btrange"] &
    SST4.quality.tests[,"sstrange"] &
    SST4.quality.tests[,"bt4refdiff"] &
    SST4.quality.tests[,"sstrefvdiff"] &
    SST4.quality.tests[,"btvnonunif"] &
    SST4.quality.tests[,"vhisenz"]
  )

SST4.quality.tests[tt2, "qsst4.new"] <- 2

# --- Quality 3

# Sue: I also have cloud here so if it fails the cloud decision tree it's in tt3
# If SST4 pases the clud tree is defined by orig$SST4.img.tree.class == P-Good

tt3 <-  SST4.quality.tests[, "qsst4.new"] == 4 &
  SST4.quality.tests[,"day.or.nite"] == "Night" &
  SST4.quality.tests[,"btbad"]

SST4.quality.tests[tt3, "qsst4.new"] <- 3

# --- End of SST4 NIGHT Quality levels
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- SST4 DAY Quality levels ----
# --- All SST4 quality levels for daytime matchups have the value of 3
# --- as long as the BT is less than 1000.

tt4 <- SST4.quality.tests[, "day.or.nite"] == "Day" &
  SST.quality.tests[, "btbad"] 

SST4.quality.tests[tt4, "qsst4.new"] <- 3

# --- End of SST4 Day Quality levels
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- calculate tables of SST4 quality levels ----
# --- once SST4 qualities have been defined for day and night.

# *** Warning: Kay suggests doing stas for SST4 only for night records

uu0 <- subset(SST4.quality.tests, day.or.nite == "Night",
    select = c(qsst4.new))

addmargins(xtabs(~ uu0$qsst4.new))
round(prop.table(xtabs(~ uu0$qsst4.new)),4) * 100

rm(uu0); gc()


# --- Separate day and night quality levels for SST4

addmargins(xtabs(~ SST4.quality.tests[, "day.or.nite"] +
  SST4.quality.tests[, "qsst4.new"] ))

round(prop.table(xtabs(~ SST4.quality.tests[, "day.or.nite"] +
  SST4.quality.tests[, "qsst4.new"])), 4) * 100
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Barplot of proportion of SST and SST4 quality levels ----

# --- Barplot of proportion of SST quality levels

tt1 <- prop.table(xtabs(~ SST.quality.tests$qsst.new))

barplot(tt1, names = names(tt1),
  ylim = c(0, 1),
  xlab = "SST Quality Level",
  ylab = "Proportion of Matchups",
  main = paste(config$sensor,"- SST Quality Levels"),
  col = "steelblue")
box()

rm(tt1); gc()

# --- Barplot of proportion of SST4 quality levels

tt1 <- prop.table(xtabs(~ SST4.quality.tests$qsst4.new))

barplot(tt1, names = names(tt1),
  ylim = c(0, 1),      
  xlab = "SST4 Quality Level",
  ylab = "Proportion of Matchups",
  main = paste(config$sensor,"- SST4 Quality Levels"),
  col = "steelblue")
box()

rm(tt1); gc()
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Add SST and SST4 quality levels to data frame "orig" -----

orig <- data.frame(orig,
  qsst.new = SST.quality.tests$qsst.new,
  qsst4.new = SST4.quality.tests$qsst4.new)
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Save "orig" data frame into a data frame for the sensor in question... ----

if (config$sensor == "Terra") {
  TERRA <- orig       # Store object "orig" in object "TERRA"
} else if (config$sensor == "Aqua") {
  AQUA <- orig 				# Store object "orig" in object "AQUA"
}	
# ----------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------#
# --- Export a text file with processed data frame ----
# --- Includes all variables for a sensor.

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





# ----------------------------------------------------------------------------------------
# --- Calculate statistics of SST residuals for each quality level ----

rms <- function(x) {
  x <- x[!is.na(x)]
	x2 <- x * x
	result <- sqrt( sum(x2) / length(x) )
	return(result)
}

# --- SST Statistics for day and night

orig3 <- orig

tt1 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3);
tt3 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=length, simplify=TRUE),3)

tt11 <- data.frame(quality=names(table(orig3$qsst.new)), min=tt1, q1=tt2, median=tt3, mean=tt4,
	q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10);


# --- SST Statistics for night only

orig3 <- subset(orig, solz >= 90, select = c(SST.latband1.res, qsst.new))

tt1 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3);
tt3 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=length, simplify=TRUE),3)

tt11 <- data.frame(quality=names(table(orig3$qsst.new)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10);


# --- SST Statistics for daytime only

orig3 <- subset(orig, solz < 90, select = c(SST.latband1.res, qsst.new))

tt1 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3);
tt3 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=sd, na.rm=TRUE, simplify=TRUE),3)
tt9 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=mad, na.rm=TRUE, simplify=TRUE),3)
tt10 <- round(tapply(orig3$SST.latband1.res, INDEX=orig3$qsst.new, FUN=length, simplify=TRUE), 3)

tt11 <- data.frame(quality=names(table(orig3$qsst.new)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10);

rm(tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,orig3);gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Calculate statistics of SST4 residuals for each quality level, NIGHT only  ----

rms <- function(x) {
  x <- x[!is.na(x)]
  x2 <- x * x
  result <- sqrt( sum(x2) / length(x) )
  return(result)
}

# --- SST4 statistics for night only

orig3 <- subset(orig, solz >= 90, select = c(SST4.latband1.res, qsst4.new))

tt1 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=min, na.rm=TRUE, simplify=TRUE),3)
tt2 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=quantile, probs=0.25, na.rm=TRUE, simplify=TRUE),3);
tt3 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=median, na.rm=TRUE, simplify=TRUE),3)
tt4 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=mean, na.rm=TRUE, simplify=TRUE),3)
tt5 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=quantile, probs=0.75, na.rm=TRUE, simplify=TRUE),3);
tt6 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=max, na.rm=TRUE, simplify=TRUE),3)
tt7 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=rms, simplify=TRUE),3)
tt8 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=sd, na.rm=TRUE,simplify=TRUE),3)
tt9 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=mad, na.rm=TRUE,simplify=TRUE),3)
tt10 <- round(tapply(orig3$SST4.latband1.res, INDEX=orig3$qsst4.new, FUN=length, simplify=TRUE),3)

tt11 <- data.frame(quality=names(table(orig3$qsst4.new)), min=tt1, q1=tt2, median=tt3, mean=tt4,
  q3=tt5, max=tt6, rms=tt7, stdev=tt8, mad=tt9, N=tt10)

rm(tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,orig3);gc()
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# --- Histogram of SST residuals by quality level ----

#orig3 <- orig
orig3 <- subset(orig, solz >= 90, select = c(SST.latband1.res, qsst.new))

tt1 <- ordered(orig3$qsst.new, levels=c(0,1,2,3,4),
  labels=c("Quality 0","Quality 1","Quality 2","Quality 3","Quality 4"))

histogram(~ orig3$SST.latband1.res | tt1,
  layout=c(2,3), breaks=seq(min(pretty(orig3$SST.latband1.res)),
  max(pretty(orig3$SST.latband1.res)), 1),
  main=paste(config$sensor,"SST - Residuals by SST quality level"),
  xlim=c(-10, 5), xlab="SST Residuals",
  scales=list(x=list(at=c(-10,-7.5,-5, -2.5, 0, 2.5, 5)), y=list(at=c(0,20,40,60)  )))


# --- Histogram of SST4 residuals by SST4 quality level

orig3 <- subset(orig, solz >= 90, select = c(SST4.latband1.res, qsst4.new))

tt1 <- ordered(orig3$qsst4.new, levels=c(0,1,2,3,4),
  labels=c("Quality 0","Quality 1","Quality 2","Quality 3","Quality 4"))

histogram(~ orig3$SST4.latband1.res | tt1,
  layout=c(2,3), breaks=seq(min(pretty(orig3$SST4.latband1.res)),
  max(pretty(orig3$SST4.latband1.res)), 1),
  main=paste(config$sensor, "SST4 - Residuals by SST4 quality level"),
  xlim=c(-10, 5), xlab="SST4 Residuals",
  scales=list(x=list(at=c(-10,-7.5,-5, -2.5, 0, 2.5, 5)), y=list(at=c(0,20,40,60)  )))
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
rm(list=tt4)

rm(tt1,tt2a,tt2b,tt2c,tt2d,tt3,tt4); gc()
# ----------------------------------------------------------------------------------------

























