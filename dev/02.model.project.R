#script to project distribution models

#source("/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/dev/02.init.args.model.project.R") #read in the initial arguments
source("/home/jc140298/bccvl/02.init.args.model.project.R")

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###############
#
# predict(object, x, ext=NULL, filename="", progress='text', ...)
#
# object A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)
# x  A Raster* object or a data.frame 
# ext  An extent object to limit the prediction to a sub-region of 'x'. Or an object that can be coerced to an Extent object by extent; such as a Raster* or Spatial* object  
# filename  Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot  
# progress  Character. Valid values are "" (no progress bar), "text" and "windows" (on that platform only)  
# ...  Additional model specific arguments. And additional arguments for file writing as for writeRaster  
#
# For maxent models, there is an additional argument 'args' used to pass arguments (options) to the maxent software.
# For bioclim models, there is an additional argument 'tails' which you can use to ignore the left or right tail of the percentile distribution for a variable.
# For geoDist models, there is an additional argument fun that allows you to use your own (inverse) distance function, and argument scale=1 that allows you to scale 
#	the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance).
# For spatial predictions with BRT, randomForest, etc., see 'predict' in the Raster package
#
###############
 
### read in the environmental data
# get list of directories, each directory should have env files related to a climate scenario
scenarios = list.files(enviro.data.dir)
# create a list to hold the climate scenario names and RasterStack of data layers
predictors = list()
# combine predictors into a RasterStack of enviro data
for (f in 1:length(scenarios)) {
	scenario.files = list.files(paste(enviro.data.dir, scenarios[f], sep="/"), full.names=TRUE)
	scenario.stack = stack(scenario.files)
	predictors[[f]] = list(scenarios[f], scenario.stack)
} # end for scenarios

# function to get model object
getModelObject = function(model.name) {

	model.dir = paste(wd, sp, "/output_", model.name, "/", sep="")
	model.obj = get(load(file=paste(model.dir, "model.object.RData", sep="")))
	return (model.obj)
}

# function to save project output
saveModelProjection = function(out.model, model.name, proj.name) {
	
	model.dir = paste(wd, sp, "/output_", model.name, "/", sep="")
	outdir = paste(model.dir, "project_", proj.name, sep=""); dir.create(outdir,recursive=TRUE);
	writeRaster(out.model, paste(outdir, proj.name, sep="/"), format="GTiff")
}

# get a list of species names from the data directory
species =  list.files(datadir, full.names=FALSE)

# for each species
for (sp in species) {

# for each set of predictors (one set per climate scenarios)
for (s in 1:length(predictors)) {

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
	
if (project.bioclim) {

	bioclim.obj = getModelObject("bioclim")	# get the model object
	bioclim.proj = predict(bioclim.obj, predictors[[s]][[2]])	# predict for given climate scenario
	saveModelProjection(bioclim.proj, "bioclim", predictors[[s]][[1]])	# save output
	rm(list=c("bioclim.obj", "bioclim.proj")); #clean up the memory
}
	
if (project.domain) {

	domain.obj = getModelObject("domain") # get the model object
	# have to drop env layers for DOMAIN if they weren't in original model
	# get the names of the env layers used to create DOMAIN model
	domain.layers = colnames(domain.obj@presence)	# should be the same as 'enviro.data.names'
	# get the names of the climater scenario's env layers
	pred.layers = names(predictors[[s]][[2]])

	# create a new list of env predictors by dropping layers not in the original model
	predictors.dm = predictors[[s]][[2]]
	for (pl in pred.layers) {
		if (!(pl %in% domain.layers)) {
			predictors.dm = dropLayer(predictors.dm, pl)
		}	
	}

	domain.proj = predict(domain.obj, predictors.dm) # predict for given climate scenario
	saveModelProjection(domain.proj, "domain", predictors[[s]][[1]]) 	# save output
	rm(list=c("domain.obj", "domain.proj")); #clean up the memory
}

if (project.mahal) {
	
	mahal.obj = getModelObject("mahal") # get the model object
	mahal.proj = predict(mahal.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(mahal.proj, "mahal", predictors[[s]][[1]]) 	# save output
	rm(list=c("mahal.obj", "mahal.proj")); #clean up the memory
}

if (project.geodist) {
	
	geodist.obj = getModelObject("geodist") # get the model object
	geodist.proj = predict(geodist.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(geodist.proj, "geodist", predictors[[s]][[1]]) 	# save output
	rm(list=c("geodist.obj", "geodist.proj")); #clean up the memory
}

if (project.convHull) {
	
	convHull.obj = getModelObject("convHull") # get the model object
	convHull.proj = predict(convHull.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(convHull.proj, "convHull", predictors[[s]][[1]]) 	# save output
	rm(list=c("convHull.obj", "convHull.proj")); #clean up the memory
}

if (project.circles) {
	
	circles.obj = getModelObject("circles") # get the model object
	circles.proj = predict(circles.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(circles.proj, "circles", predictors[[s]][[1]]) 	# save output
	rm(list=c("circles.obj", "circles.proj")); #clean up the memory
}

if (project.geoIDW) {
	
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	geoIDW.proj = predict(geoIDW.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(geoIDW.proj, "geoIDW", predictors[[s]][[1]]) 	# save output
	rm(list=c("geoIDW.obj", "geoIDW.proj")); #clean up the memory
}

if (project.voronoiHull) {
	
	voronoiHull.obj = getModelObject("voronoiHull") # get the model object
	voronoiHull.proj = predict(voronoiHull.obj, predictors[[s]][[2]]) # predict for given climate scenario
	saveModelProjection(voronoiHull.proj, "voronoiHull", predictors[[s]][[1]]) 	# save output
	rm(list=c("voronoiHull.obj", "voronoiHull.proj")); #clean up the memory
}

if (project.brt) {
	
	brt.obj = getModelObject("brt") # get the model object
	# NOTE the order of arguments in the  predict function for brt; this is because
	#	the function is defined outside of the dismo package
	brt.proj = predict(predictors[[s]][[2]], brt.obj, n.trees=brt.obj$gbm.call$best.trees) # predict for given climate scenario
	saveModelProjection(brt.proj, "brt", predictors[[s]][[1]]) 	# save output
	rm(list=c("brt.obj", "brt.proj")); #clean up the memory
}

if (project.maxent) {

	#*************** UNDER CONSTRUCTION ***************
	# maxent model creation was run as a system call outside of R, need to do the same for projection
	# EMG check to see if argument defaults / modifiables are the same as during creation

	# load java
	#system("module load java")
	# EMG Error "sh: module: command not found" when executing this in R
	
	# create output directory
	model.dir = paste(wd, sp, "/output_maxent/", sep="")
	outdir = paste(model.dir, "project_", predictors[[s]][[1]], sep=""); dir.create(outdir,recursive=TRUE);
	
	### not user modified section
	tstr = paste("java -cp ", maxent.jar, " density.Project ", model.dir, sp, ".lambdas ", sep="")
	# where to find the climate scenarios
	tstr = paste(tstr, enviro.data.dir, "/", scenarios[s], " ", sep="")
	# where to put the output
	tstr = paste(tstr, outdir, "/", scenarios[s], ".asc", sep="")
	# optional arguments
	tstr = paste(tstr, " nowriteclampgrid nowritemess fadebyclamping cache=FALSE", sep="")
	system(tstr)
	
	# EMG 'outputfiletype' = asc, mxe, grd, bil only NOT geotiff; can create *.png ('pictures=TRUE')
}

} # end for predictors

} # end for species