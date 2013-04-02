
# Date Created: 28 Mar 2013
# Last Modified By: EGraham 28.03.13

# This script test dismo using 1 species, 4 models, and one climate scenario
# AWT occurrence and background data
# BIOCLIM, DOMAIN, MAHAL, and MAXENT
# RCP85_ukmo_hadcm2_2086

# source("bccvl_dismo.R")

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

# load libraries required
library(dismo)
library(SDMTools)	# for read.as.gz()


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
# files are saved as *.asc.gz files, need to unzip, rasterize, stack and rename them
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

#bc = bioclim(sp.occur)
# Warning message: 'species' was removed because it is a factor (categorical data)
# EMG: this would consider 'lon' and 'lat' columns as variables, too

bc = bioclim(x=sp.occur[,4:11])

# evaluate bioclim
e.bc = evaluate(p=sp.occur, a=background, model=bc)
# EMG need to check implementation of function for p, a data as SWDs

	
# predict bioclim
p.bc = predict(bc, future.predictors)

# save output
writeRaster(p.bc, paste(outdir, "AWT_bioclim", sep=""), format="ascii")


###############
#
# DOMAIN
#
###############

dm = domain(sp.occur[,4:11])

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
writeRaster(p.dm, paste(outdir, "AWT_domain", sep=""), format="ascii")


###############
#
# MAHALANOBIS
#
###############

mm = mahal(sp.occur[,4:11])

# evaluate mahalanobis
e.mm = evaluate(p=sp.occur, a=background, model=mm)

# predict mahalanobis
p.mm = predict(mm, future.predictors)

# save output
writeRaster(p.mm, paste(outdir, "AWT_mahalanobis", sep=""), format="ascii")


#############################################################################################
#
# MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
#
#############################################################################################

	
	**************** UNDER CONSTRUCTION ******************


###############
#
# MAXENT
#
###############

#kludge to get data into the right format for maxent

fix = rbind(sp.occur, background)
ps = rep(1, length=nrow(sp.occur))
as = rep(0, length=nrow(background))
v.occur = c(ps,as)

# EMG Note this next command works on my local machine (in <2min) but FAILS on HPC after 1h20min

#me = maxent(x=fix[,4:11], p=v.occur)
# Error in .jcall(mxe, "S", "fit", c("autorun", "-e", afn, "-o", dirout,  :
#  java.awt.HeadlessException

# EMG when it does run, it puts .html and other output files into a temp folder
# but I want to specify the output dir

#me = maxent(x=fix[,4:11], p=v.occur, path=paste(outdir, "maxent/", sep=""))
#Error in .local(x, p, ...) : 
#  cannot create output directory: c:/userdata/SDM/bccvl/output/maxent/
# Funny thing is, it *does* create the dir, it just doesn't write the output to it!

# alternative way to save output
save(me, file=paste(outdir, "maxent/maxent_me", sep=""))
# this is the model object, not sure if it's possible to get previous output (html etc)



###############
#
# BRT
#
###############

library(gbm)

# use the same 'fix' data.frame as above but add vector column of presence/
#	absence to create single input data.frame
fix2 = cbind(fix, v.occur)

brt = gbm.step(data=fix2, gbm.x=4:11, gbm.y=12)
# EMG not sure how to save the initial figure produced by function

# evaluate brt
#e.brt = evaluate(p=sp.occur, a=background, model=brt)
#Error in paste("Using", n.trees, "trees...\n") : 
#  argument "n.trees" is missing, with no default
# EMG may  not be implemented for brt?

# predict brt
#p.brt = predict(brt, future.predictors)
# as above, may not be implemented

# for gbm.predict.grids
source(paste(getwd(), "/brt.functions.R", sep=""))

future.predictors.matrix = extract(future.predictors, sp.occur[,2:3])

p.brt = gbm.predict.grids(brt, as.data.frame(future.predictors.matrix))
p.brt = gbm.predict.grids(brt, as.data.frame(future.predictors.matrix), 
	want.grids=T, sp.name="ABT", pred.vec = rep(-9999, 612226), 
	filepath="c:/userdata/SDM/brt/", num.col=886, num.row=691,
	x11=111.975, y11=-44.525,
	cell.size=100, no.data=-9999, plot=T)


# save output
writeRaster(p.brt, paste(outdir, "ABT_brt", sep=""), format="ascii")


#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################

###############
#
# GEOGRAPHIC DISTANCE
#
###############

###############
#
# CONVEX HULLS
#
###############

###############
#
# CIRCLES
#
###############

###############
#
# GEOIDW
#
###############

###############
#
# VORONOIHULL
#
###############