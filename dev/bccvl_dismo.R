
# Date Created: 01 Mar 2013
# Last Modified By: EGraham 22.03.13

# This script is based on the vignette Species Distribution Modeling with R 
# by Hijmas and Elith 2012. It runs, evaluates, and compares several 
# algorithms in the dismo package including BIOCLIM, DOMAIN, MAHAL, and MAXENT
# using the AWT.NERP related data files and climate projections
# EMG 22.03.13 MAXENT to be tested separately

# EMG NOTE: need to create output dirs until implemented in shell script

# source("bccvl_dismo.R")

# start with a clean environment
rm(list=ls())

# set working directory
setwd("/home/jc140298/bccvl")

outdir = paste(getwd(), '/outputs', sep="")

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

# get list of species names
species.names = unique(occur$species)


runDismoModels = function (species) {

	# get species occurence data
	sp.occur = occur[occur$species == species,]

	# only need longitude and latitude columns
	sp.occur = cbind(sp.occur$lon, sp.occur$lat)

	# fit bioclim
	bc <- bioclim(curr.predictors, sp.occur)

	# evaluate bioclim
	evaluateDismoModel(species, sp.occur, TG[,2:3], bc, curr.predictors)
	
	# predict bioclim
	predictDismoModel(species, future.predictors, bc)

	# fit domain
	dm <- domain(curr.predictors, sp.occur)

	# evaluate domain
	evaluateDismoModel(species, sp.occur, TG[,2:3], dm, curr.predictors)
	
	# predict domain
	predictDismoModel(species, future.predictors, dm)

	# fit mahalanobis
	mm <- mahal(curr.predictors, sp.occur)

	# evaluate mahalanobis
	evaluateDismoModel(species, sp.occur, TG[,2:3], mm, curr.predictors)
	
	# predict mahalanobis
	predictDismoModel(species, future.predictors, mm)

#	return(list(species, bc, dm, mm))
} # end fit models function


evaluateDismoModel = function (species, o.points, background, model.obj, env.layers) {

	e <- evaluate(o.points, background, model.obj, curr.predictors)

	# use species arg to place files in right folder
	outdirpath = paste(outdir, species, class(model.obj)[1], sep="/")

	# will need to create the right directory for outputs via the shell script
	jpeg(paste(outdirpath, "/", class(model.obj)[1], "_evals.jpg", sep=""))
	
	par(mfrow=c(2,2), oma=c(1, 1, 2, 1))
	plot(e, 'ROC')	# check if this figure is equivalent to MaxEnt's Fig2
	plot(e, 'TPR')	# MaxEnt's Fig1?

	# other plotting options:
	# the model predictions for known presence and absence points
	boxplot(e, col=c('blue', 'red'))

	# density plots of presence and absence data
	density(model.obj)
	# EMG not sure how to interpret these last two yet

	mtext("BIOCLIM", 3, outer=TRUE, cex=1.2)
	dev.off()
	
	# EMG want to export as rows in a table?
	# species, model, npresences, nabsences, AUC, cor, max TPR
} # end evaulate function


predictDismoModel = function (species, future.layers, model.obj) {

	p <- predict(future.predictors, model.obj)

	# use species arg to place files in right folder
	outdirpath = paste(outdir, species, class(model.obj)[1], sep="/")

	# somehow need to keep track of which scenario is being run
	
	# plot raw predicted values
	jpeg(paste(outdirpath, "/", class(model.obj)[1], "_raw.jpg", sep=""))
	plot(p, main='Bioclim, raw values')
	dev.off()

	# EMG export as *.asc? see SDMTools | asc.from.raster
} # end predict function

# run script
test.species = species.names[1:3]

for (species in test.species) {

	runDismoModels(species)
}