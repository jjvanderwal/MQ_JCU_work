#script to project distribution models

#source("/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/dev/02.init.args.model.project.R") #read in the initial arguments
source("/home/jc140298/bccvl/02.init.args.model.project.R")

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries


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

	model.dir = paste(wd, "output_", model.name, "/", sep="")
	model.obj = get(load(file=paste(model.dir, "model.object.RData", sep="")))
	return (model.obj)
}

# function to save project output
saveModelProjection = function(out.model, model.name, proj.name) {
	
	model.dir = paste(wd, "output_", model.name, "/", sep="")
	outdir = paste(model.dir, "project_", proj.name, sep=""); dir.create(outdir,recursive=TRUE);
	writeRaster(out.model, paste(outdir, proj.name, sep="/"), format="GTiff")
}

# for each set of predictors 
for (s in 1:length(predictors)) {
	
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

#if (project.maxent) {

	#*************** UNDER CONSTRUCTION ***************
	# maxent model creation was run as a system call outside of R, need to do the same for projection
	# EMG check to see if argument defaults / modifiables are the same as during creation

	### not user modified section
#	tstr = ("module load java\n")
#	tstr = paste(tstr, "java -cp ", maxent.jar, " density.Project ", wd, "output_maxent/ABT.lambdas ", sep="")
	# where to find the climate scenarios
#	tstr = paste(tstr, enviro.data.dir, "/", scenarios[s], " ", sep="")
	# where to put the output
#	tstr = paste(tstr, wd, "output_maxent/", scenarios[s], "/projection.asc", sep="")
	# optional arguments
#	tstr = paste(tstr, " nowriteclampgrid nowritemess fadebyclamping", sep="")
#	system(tstr)
#}

} # end for predictors