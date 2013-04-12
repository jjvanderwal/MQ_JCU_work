
# Date Created: 28 Mar 2013
# Last Modified By: EGraham 12.04.13

# This script tests the dismo package using one species (AWT), its occurrence and background data,
# and one climate scenario (RCP85_ukmo_hadcm2_2086)

# profile methods: BIOCLIM, DOMAIN, MAHAL 
# machine learning: MAXENT, BRT
# geographic: GEODIST, CONVHULL, CIRCLES, GEOIDW, VORONOIHULL


# source("bccvl/AWT_dismo.R")

# start with a clean environment
rm(list=ls())

# set working and output directories
setwd("/home/jc140298/bccvl")
outdir = paste(getwd(), '/output/', sep="")

# may need to install packages
#install.packages("dismo")
#install.packages("rdgal") 	# need > 0.7-22
#install.packages("rJava")	# for MaxEnt; 
# may need to do a 'R CMD javareconf -e' if you don't have root privileges
# also make sure maxent.jar is available in ../R/library/dismo
#install.packages("gbm")	# for BRT
#install.packages("gstat")	# geoIDW
#install.packages("deldir")	# voronoiHull

# load libraries required
library(dismo)
library(SDMTools)	# for read.as.gz()
library(gbm)


# inputs

# get species occurence data
sp.occur = read.csv("/home/jc165798/working/AWT.NERP/future.SDM/models/ABT/occur.csv")
# EMG Note occur.csv is an SWD file
# species, lon, lat, bioclim_01, bioclim_04, bioclim_05, bioclim_06, bioclim_12, bioclim_15, 
#	bioclim_16, bioclim_17

# background files to evaluate() models
background = read.csv("/home/jc165798/working/AWT.NERP/future.SDM/models/ABT/bkgd.csv")


# get the future climate scenario
future.projs = list.files("/home/jc165798/Climate/CIAS/AWT/250m/bioclim_asc/RCP85_ukmo-hadcm3_2085",
	full.names=TRUE)
# 19 files: bioclim_01 to bioclim_19.asc.gz

# package files as raster stack to send to predict()
# files are saved as *.asc.gz files, need to unzip, rasterize, stack and rename the layers
converted = list()
for (i in 1:length(future.projs)) {
	temp = read.asc.gz(future.projs[i])
	converted[[i]] = raster(temp)
}	
future.predictors = stack(converted)
names(future.predictors) = c("bioclim_01", "bioclim_02", "bioclim_03", "bioclim_04", "bioclim_05",
	"bioclim_06", "bioclim_07", "bioclim_08", "bioclim_09", "bioclim_10", "bioclim_11", 
	"bioclim_12", "bioclim_13", "bioclim_14", "bioclim_15", "bioclim_16", "bioclim_17",
	"bioclim_18", "bioclim_19")
# EMG There's got to be a better way!!


#################################################################################
#
# PROFILE METHODS - only consider presence points: Bioclim, Domain, and Mahal
#
#################################################################################

###############
#
# BIOCLIM
#
###############

#bc = bioclim(x=sp.occur)
# Warning message: 'species' was removed because it is a factor (categorical data)
# EMG: this would consider 'lon' and 'lat' columns as variables, too

bc = bioclim(x=sp.occur[,4:11])

# evaluate bioclim
e.bc = evaluate(p=sp.occur, a=background, model=bc)
# EMG need to check implementation of function for p, a data as SWDs

	
# predict bioclim
p.bc = predict(bc, future.predictors)

# save output
writeRaster(p.bc, paste(outdir, "ABT_bioclim", sep=""), format="ascii")


###############
#
# DOMAIN
#
###############

dm = domain(x=sp.occur[,4:11])

# evaluate domain
e.dm = evaluate(p=sp.occur, a=background, model=dm)

# predict domain
#p.dm = predict(dm, future.predictors.dm)
# Error message: undefined columns selected
# EMG: I think this is because there are more layers in the in the future projections than in the 
#	original model; not sure why it works for other models
# remove layers not present in fitted model
future.predictors.dm = dropLayer(future.predictors, "bioclim_02")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_03")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_07")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_08")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_09")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_10")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_11")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_13")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_14")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_18")
future.predictors.dm = dropLayer(future.predictors.dm, "bioclim_19")

p.dm = predict(dm, future.predictors.dm)

# save output
writeRaster(p.dm, paste(outdir, "ABT_domain", sep=""), format="ascii")


###############
#
# MAHALANOBIS
#
###############

mm = mahal(x=sp.occur[,4:11])

# evaluate mahalanobis
e.mm = evaluate(p=sp.occur, a=background, model=mm)

# predict mahalanobis
p.mm = predict(mm, future.predictors)

# save output
writeRaster(p.mm, paste(outdir, "ABT_mahalanobis", sep=""), format="ascii")


#############################################################################################
#
# MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
#
#############################################################################################


###############
#
# MAXENT
#
###############

# kludge to get data into the right format for maxent

fix.data.me = rbind(sp.occur, background)
ps = rep(1, length=nrow(sp.occur))	# presences
as = rep(0, length=nrow(background))	# absences
v.occur = c(ps,as)

# EMG Note this next command works on my local machine (in <2min) but FAILS on HPC after 1h20min

#me = maxent(x=fix.data.me[,4:11], p=v.occur, path=paste(outdir, "maxent", sep=""))

#Error in .jcall(mxe, "S", "fit", c("autorun", "-e", afn, "-o", dirout,  :
#java.awt.HeadlessException


###############
#
# BRT
#
###############

# use the same 'fix' data.frame as above but add vector column of presence/
#	absence to create single input data.frame
fix.data.brt = cbind(fix.data.me, v.occur)

brt = gbm.step(data=fix.data.brt, gbm.x=4:11, gbm.y=12, tree.complexity=1, 
	learning.rate=0.01, bag.fraction=0.75, family="bernoulli", plot.main=FALSE)
# EMG 'plot.main' is defaulted to TRUE but it's not necessary on the HPC

# evaluate brt
e.brt = evaluate(p=sp.occur, a=background, model=brt, n.trees=brt$gbm.call$best.trees)
# EMG calls predict() which requires n.trees arg

# predict brt
p.brt = predict(future.predictors, brt, n.trees=brt$gbm.call$best.trees)
# EMG Note the order of args for the predict function (cf dismo algorithms)

# save output
writeRaster(p.brt, paste(outdir, "ABT_brt", sep=""), format="ascii")


#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################

############### Spatial-only models for presence data ###############

###############
#
# GEOGRAPHIC DISTANCE
#
###############

gd = geoDist(p=sp.occur[,2:3], lonlat=TRUE)

e.gd = evaluate(model=gd, p=sp.occur, a=background)
#EMG NOTE: no error for p,a if columns not specified

p.gd = predict(gd, future.predictors)

writeRaster(p.gd, paste(outdir, "ABT_geoDistance", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)
# format="GTiff" not show the area, it's very faint in asc


###############
#
# CONVEX HULLS
#
###############

ch = convHull(p=sp.occur[,2:3], lonlat=TRUE)

e.ch = evaluate(model=ch, p=sp.occur[,2:3], a=background[,2:3])
#EMG NOTE: error for p,a if columns not specified

p.ch = predict(ch, future.predictors)

writeRaster(p.ch, paste(outdir, "ABT_convexHulls", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)


###############
#
# CIRCLES
#
###############

#trace(circles)
#c = circles(p=sp.occur[,2:3], lonlat=TRUE, r=6378137)
#Error in pointDistance(xy[i, ], xy, longlat = TRUE, r = r) : 
#	object 'r' not found

#e.c = evaluate(model=c, p=sp.occur[,2:3], a=background[,2:3])
#p.c = predict(c, future.predictors)
#writeRaster(p.c, paste(outdir, "ABT_circles", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)


############### Spatial-only models for presence/background (or absence) data ###############

###############
#
# GEOIDW - inverse distance weighted interpolation
#
###############

library(gstat)

gidw = geoIDW(p=sp.occur[,2:3], a=background[,2:3])

e.gidw = evaluate(model=gidw, p=sp.occur[,2:3], a=background[,2:3])

p.gidw = predict(gidw, future.predictors)

writeRaster(p.gidw, paste(outdir, "ABT_geoIDW", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)


###############
#
# VORONOIHULL
#
###############

library(deldir)

vh = voronoiHull(p=sp.occur[,2:3], a=background[,2:3])

#e.vh = evaluate(model=vh, p=sp.occur[,2:3], a=background[,2:3])
#Error: cannot allocate vector of size 10.0 Gb

p.vh = predict(vh, future.predictors)

writeRaster(p.gidw, paste(outdir, "ABT_voronoiHull", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)