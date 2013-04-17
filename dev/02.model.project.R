
# Date Created: 16 April 2013
# Last Modified By: EGraham 16.04.13

# This script will project species' distributions using the dismo package
# Requires SDM fits
# BIOCLIM, DOMAIN, MAHAL, MAXENT, BRT, GEODIST, CONVEXHULL, CIRCLES, GEOIDW, VORONOIHULL

# source("02.model.project.R")

# start with a clean environment
rm(list=ls())

# load libraries required
library(dismo)
library(SDMTools)	# for read.as.gz()
library(gbm)

# get initial arguments for projections - will need to modify this
#source("02.init.args.model.project.R")

# set arguments for now
species="ABT"
models = c("BIOCLIM", "DOMAIN", "MAHAL", "BRT", "GEODIST", "CONVEXHULL", "CIRCLES", 
	"GEOIDW", "VORONOIHULL", "MAXENT")
enviro.dir = "/home/jc165798/working/BCCVL/envirodata"

# create a list to hold the climate scenario names and RasterStack of data layers
predictors = list()
# get list of directories, each directory should have env files related to a climate scenario
scenarios = list.files(enviro.dir)
# create RasterStacks of enviro data
for (f in 1:length(scenarios)) {
	scenario.files = list.files(paste(enviro.dir, scenarios[f], sep="/"), full.names=TRUE)
	scenario.stack = stack(scenario.files)
	predictors[[f]] = list(scenarios[f], scenario.stack)
} # end for scenarios

# for each algorithm
for (m in models) {

	# get the model object
	# where model outputs are stored until jeremy's script is run
	datadir = paste("/home/jc140298/", species, "/output_", m, "/", sep="")
	obj.name = paste(m, "obj.sdm", sep="_")
	hold = load(file=paste(datadir, sep="", obj.name))
	proj.model = get(hold)
	
	# for each set of predictors 
	for (s in 1:length(predictors)) {

		if (!(m %in% c("MAXENT", "BRT"))) {
		
			# have to drop layers for DOMAIN if they weren't in original model
			if (m == "DOMAIN") {
				predictors.dm = dropLayer(predictors[[s]][[2]], c("bioclim_02","bioclim_03","bioclim_07",
					"bioclim_08","bioclim_09","bioclim_10","bioclim_11","bioclim_13","bioclim_14",
					"bioclim_18","bioclim_19"))
				project = predict(proj.model, predictors.dm)
			} else {
				# make projection
				projection = predict(proj.model, predictors[[s]][[2]])
			} # end if domain
			
		}  else if (m == "BRT") {
		
			# make projection
			projection = predict(predictors[[s]][[2]], proj.model, n.trees=proj.model$gbm.call$best.trees)
				
		} else if (m == "MAXENT") {
			cat("Not implemented")
		} # end if
		
		# save output
		outfilename = paste(datadir, "project_", predictors[[s]][[1]], sep="")
		writeRaster(projection, outfilename, format="ascii") 
#			format="GTiff", 
#			options=c("PHOTOMETRIC=CMYK", "JPEG_QUALITY=75", "COMPRESS=LZW"),
#			overwrite=TRUE)
	} # end for predictors
} # end for models