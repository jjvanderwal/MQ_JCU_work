
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

###############
#
# MAXENT
#
###############

#kludge to get data into the right format for maxent

fix = rbind(sp.occur, background)
ps = rep(1, length=nrow(sp.occur)
as = rep(0, length=nrow(background)
v.occur = c(ps,as)

me = maxent(x=fix[,4:11], p=v.occur)



###############
#
# BRT
#
###############

#library(gbm)

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