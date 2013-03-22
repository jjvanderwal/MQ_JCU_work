
# Date Created: 01 Mar 2013
# Last Modified By: EGraham 22.03.13

# This script is based on the vignette Species Distribution Modeling with R 
# by Hijmas and Elith 2012. It runs, evaluates, and compares several 
# algorithms in the dismo package including BIOCLIM, DOMAIN, MAHAL, and MAXENT
# using the AWT.NERP related data files and climate projections

# source("bccvl_dismo.R")

# start with a clean environment
rm(list=ls())

# set working directory
setwd("/home/jc140298/bccvl")

# may need to install packages
#install.packages("dismo")
#install.packages("rdgal") 	# need > 0.7-22

# load libraries required
library(dismo)

#define the set of projections
curr.projs = list.files(paste(getwd(), '/predictors/bioclim_asc/current.76to05', sep=""),
	full.names=TRUE, recursive = TRUE) 
future.projs = list.files(paste(getwd(), '/predictors/bioclim_asc/RCP3PD_cccma-cgcm31_2015', sep=""),
	full.names=TRUE, recursive = TRUE) 

# create raster stacks of inputs
curr.predictors = stack(curr.projs)
future.predictors = stack(future.projs)

#read in occurrence and background files 
occur = read.csv(paste(getwd(), '/inputs/base.occur.csv', sep=""),as.is=TRUE); 
#clean up the data and get rid of mixsppa 
occur$species = toupper(gsub(' ','',occur$species))
TG = read.csv(paste(getwd(),'/inputs/base.background.targetgroup.csv', sep=""),as.is=TRUE) 

# not used
#Grid = read.csv(paste(getwd(),'/inputs/base.background.grid.csv', sep=""),as.is=TRUE)  

# limit species to just AWT for now
occur = occur[occur$species == "ABT",]


runDismoModels = function SPECIES {


#####
##
## BIOCLIM
##
#####

# fit a bioclim model simply using the predictors and the occurrence points
# predictors as raster* (e.g., *layer, *stack)
# occurrence data x,y only
bc <- bioclim(curr.predictors, occur[,2:3])


#############################
#
# bioclim object has the following attributes:
# $min - value for each layer
# $max - value for each layer
# $presence - one row per record with values for each record
# $absence - <empty>, presumably as above
# $hasabsence - FALSE
# $class
#
#############################

# evaluate the model
e.bc <- evaluate(occur[,2:3], TG[,2:3], bc, curr.predictors)

#############################
#
# e.bc object:
#
# class : ModelEvaluation
# npresences : 337
# nabsences : 11846
# AUC : 0.6056127
# cor : 0.06125562
# max TPR=TNR at : 0.1304368
#
#############################

# will need to create the right directory for outputs via the shell script
jpeg(paste(getwd(), '/outputs/bioclim_evals.jpg', sep=""))

par(mfrow=c(2,2), oma=c(1, 1, 2, 1))
plot(e.bc, 'ROC')	# check if this figure is equivalent to MaxEnt's Fig2
plot(e.bc, 'TPR')	# MaxEnt's Fig1?

# other plotting options:
# the model predictions for known presence and absence points
boxplot(e.bc, col=c('blue', 'red'))

# density plots of presence and absence data
density(e.bc)
# EMG not sure how to interpret these last two yet

mtext("BIOCLIM", 3, outer=TRUE, cex=1.2)
dev.off()

# EMG want to export as rows in a table?
# species, model, npresences, nabsences, AUC, cor, max TPR

# find a threshold
tr.bc <- threshold(e.bc, 'spec_sens')

# spec_sens: the threshold at which the sum of the sensitivity 
# (true positive rate) and specificity (true negative rate) is highest 
# alternate parameters:
# kappa: the threshold at which kappa is highest ("max kappa") 
# no_omission: the highest threshold at which there is no omission 

# NOTE: with no type of threshold species, get a dataframe with all three

# make a prediction
p.bc <- predict(future.predictors, bc) 	# less than one minute


# alternate parameters:
# ext An extent object to limit the prediction to a sub-region of 'x' 
# filename Output filename for a new raster; if NA the result is not written 
#	to a file but returned with the RasterLayer object, in the data slot
# progress Character. Valid values are "" (no progress bar), 
#	"text" and "windows" (on that platform only)

# For maxent models, there is an additional argument 'args' used to pass 
#	arguments (options) to the maxent software
# For bioclim models, there is an additional argument 'tails' which you can 
#	use to ignore the left or right tail of the percentile distribution 
#	for a variable
# For geoDist models, there is an additional argument fun that allows you 
#	to use your own (inverse) distance function, and argument scale=1 that 
#	allows you to scale the values

# plot raw predicted values
jpeg(paste(getwd(), '/outputs/bioclim_raw.jpg', sep=""))
plot(p.bc, main='Bioclim, raw values')
dev.off()


#####
##
## DOMAIN
##
#####

dm <- domain(curr.predictors, occur[,2:3])

e.dm <- evaluate(occur[,2:3], TG[,2:3], dm, curr.predictors)

#############################
#
# e.dm object:
#
# class : ModelEvaluation
# npresences : 337
# nabsences : 11846
# AUC : 0.586676
# cor : 0.0500878
# max TPR=TNR at : 0.675174
#
#############################


jpeg(paste(getwd(), '/outputs/domain_evals.jpg', sep=""))
par(mfrow=c(2,2), oma=c(1, 1, 2, 1))
plot(e.dm, 'ROC')	
plot(e.dm, 'TPR')
boxplot(e.dm, col=c('blue', 'red'))
density(e.dm)
mtext("DOMAIN", 3, outer=TRUE, cex=1.2)
dev.off()

p.dm = predict(future.predictors, dm)

jpeg(paste(getwd(), '/outputs/domain_raw.jpg', sep=""))
plot(p.dm, main='Domain, raw values')
dev.off()


#####
##
## MAHALANOBIS
##
#####

mm <- mahal(curr.predictors, occur[,2:3])

e.mm <- evaluate(occur[,2:3], TG[,2:3], mm, curr.predictors)

#############################
#
# e.mm object:
#
# class : ModelEvaluation
# npresences : 337
# nabsences : 11846
# AUC : 0.7373375
# cor : 0.0538222
# max TPR=TNR at : 0.9999
#
#############################

jpeg(paste(getwd(), '/outputs/mahal_evals.jpg', sep=""))
par(mfrow=c(2,2), oma=c(1, 1, 2, 1))
plot(e.mm, 'ROC')	
plot(e.mm, 'TPR')
boxplot(e.mm, col=c('blue', 'red'))
density(e.mm)
mtext("MAHALANOBIS", 3, outer=TRUE, cex=1.2)
dev.off()

p.mm = predict(future.predictors, mm)

jpeg(paste(getwd(), '/outputs/mahal_raw.jpg', sep=""))
plot(p.mm, main='Mahalanobis distance, raw values')
dev.off()
