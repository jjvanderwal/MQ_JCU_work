
# Date Created: 28 Mar 2013
# Last Modified By: EGraham 15.04.13

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

# bioclim(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

bc = bioclim(x=sp.occur[,4:11])

# evaluate bioclim
e.bc = evaluate(p=sp.occur, a=background, model=bc)
	
# predict bioclim
p.bc = predict(bc, future.predictors)

# save output
writeRaster(p.bc, paste(outdir, "ABT_bioclim", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=100", "COMPRESS=NONE"),
#	overwrite=TRUE)


###############
#
# DOMAIN
#
###############

# domain(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

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
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)

###############
#
# MAHALANOBIS
#
###############

# mahal(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

mm = mahal(x=sp.occur[,4:11])

# evaluate mahalanobis
e.mm = evaluate(p=sp.occur, a=background, model=mm)

# predict mahalanobis
p.mm = predict(mm, future.predictors)

# save output
writeRaster(p.mm, paste(outdir, "ABT_mahalanobis", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)


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

# maxent(x, p, ...)
# x is a Raster* object or SpatialGridDataFrame, containing grids with predictor variables. These
#	will be used to extract values from for the point locations
# x can also be a data.frame, in which case each column should be a predictor variable and each row a 
#	presence or absence record
# p can be a data.frame, matrix, SpatialPoints* object or a vector
#	- if data.frame or matrix it represents a set of point locations and it must have two columns
#		with the first being the lon and the second the lat
#	- if SpatialPointS* object, p is also coordinates
#	- if x is a data.frame, p should be a vector with a length equal to nrow(x) and contain 
#		0 (background) and 1 (presence) values, to indicate which records are which
# Additional arguments:
# a background points; only used if 'p' is not missing and not a vector
# factors which (if any) variables should be considered categorical, either by name or index
# args additional args, see maxent help
# removeDuplicates boolean; if TRUE, duplicate presence points (that fall in the same grid) are removed
# path optional; where you want the output files to be store

# EMG: kludge to get data into the right format [maxent(data.frame, vector)] for maxent
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

# gbm.step(data, gbm.x, gbm.y, offset = NULL, fold.vector = NULL, tree.complexity = 1, 
#	learning.rate = 0.01, bag.fraction = 0.75, site.weights = rep(1, nrow(data)), 
#	var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE, 
#	family = "bernoulli", n.trees = 50, step.size = n.trees, max.trees = 10000,
#	tolerance.method = "auto", tolerance = 0.001, keep.data = FALSE, plot.main = TRUE, 
#	plot.folds = FALSE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, 
#	keep.fold.vector = FALSE, keep.fold.fit = FALSE, ...)
# data input data.frame
# gbm.x predictor variables
# gbm.y	response variable
# offset = NULL
# fold.vector = NULL	a fold vector to be read in for cross validation with offsets
# tree.complexity = 1	sets the complexity of individual trees
# learning.rate = 0.01	sets the weight applied to individual trees
# bag.fraction = 0.75	sets the proportion of observations used in selecting variables
# site.weights = rep(1, nrow(data))	allows varying weighting for sites
# var.monotone = rep(0, length(gbm.x))	restricts responses to individual predictors to monotone
# n.folds = 10	number of folds
# prev.stratify = TRUE	prevalence stratify the folds - only for presence/absence data
# family = "bernoulli"	family - bernoulli (=binomial), poisson, laplace or gaussian
# n.trees = 50	number of initial trees to fit
# step.size = n.trees	numbers of trees to add at each cycle
# max.trees = 10000	max number of trees to fit before stopping
# tolerance.method = "auto"	method to use in deciding to stop - "fixed" or "auto"
# tolerance = 0.001	tolerance value to use - if method == fixed is absolute, 
#	if auto is multiplier * total mean deviance
# keep.data = FALSE	Logical. keep raw data in final model
# plot.main = TRUE	Logical. plot hold-out deviance curve
# plot.folds = FALSE	Logical. plot the individual folds as well
# verbose = TRUE	Logical. control amount of screen reporting
# silent = FALSE	Logical. to allow running with no output for simplifying model)
# keep.fold.models = FALSE 	Logical. keep the fold models from cross valiation
# keep.fold.vector = FALSE	Logical. allows the vector defining fold membership to be kept
# keep.fold.fit = FALSE	Logical. allows the predicted values for observations from cross-validation 
#	to be kept


# EMG use the same 'fix' data.frame as above but add vector column of presence/
#	absence to create single input data.frame
fix.data.brt = cbind(fix.data.me, v.occur)

brt = gbm.step(data=fix.data.brt, gbm.x=4:11, gbm.y=12, tree.complexity=1, 
	learning.rate=0.01, bag.fraction=0.75, family="bernoulli", plot.main=FALSE,
	verbose = FALSE, silent = TRUE)

# evaluate brt
e.brt = evaluate(p=sp.occur, a=background, model=brt, n.trees=brt$gbm.call$best.trees)
# EMG calls predict() which requires n.trees arg

# predict brt
p.brt = predict(future.predictors, brt, n.trees=brt$gbm.call$best.trees)
# EMG Note the order of args for the predict function (cf dismo algorithms)

# save output
writeRaster(p.brt, paste(outdir, "ABT_brt", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)


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

# geoDist(p, ...)
# p point locations (presence); two column matrix, data.frame or SpatialPoints* object
# ... you must supply a lonlat= argument(logical), unless p is a SpatialPoints* object and has a
#	valid CRS
# ... you can also supply an additional argument 'a' for absence points (currently ignored.); 
#	argument 'a' should be of the same class as argument 'p'

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

# convHull(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# ... you can supply an argument n (>=1) to get n convex hulls around subset of the points
# ... you can also set n=1:x, to get a set of overlapping polygons consisting of 1 to x parts; i.e.
#	the first polygon has 1 part, the second has 2 parts and x has x parts

ch = convHull(p=sp.occur[,2:3])

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

# circles(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# d the radius of each circle in meters; a single number or a vector with elements corresponding to
#	rows in 'p'; if missing the diameter is computed from the inter-point distance
# n how many vertices in the circle? default is 360
# lonlat are these longitude/latitude data? default value is false
# r radius of the earth; only relevant for longitude/latitude data; default is 6378137 m

c = circles(p=sp.occur[,2:3], lonlat=TRUE)

e.c = evaluate(model=c, p=sp.occur[,2:3], a=background[,2:3])
p.c = predict(c, future.predictors)

writeRaster(p.c, paste(outdir, "ABT_circles", sep=""), format="ascii")
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

# geoIDS(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'
# ... none implemented

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

# voronoiHull(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'

vh = voronoiHull(p=sp.occur[,2:3], a=background[,2:3])

#e.vh = evaluate(model=vh, p=sp.occur[,2:3], a=background[,2:3])
#Error: cannot allocate vector of size 10.0 Gb

p.vh = predict(vh, future.predictors)

writeRaster(p.gidw, paste(outdir, "ABT_voronoiHull", sep=""), format="ascii")
#	format="GTiff", 
#	options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=NONE"),
#	overwrite=TRUE)