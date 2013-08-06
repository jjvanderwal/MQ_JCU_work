#script to project distribution models

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
# check to see if arguments are passed
if(length(args)==0){
    print("No arguments supplied.")
    # leave all args as default values
} else {
	for(i in 1:length(args)) { 
		eval(parse(text=args[[i]])) 
	}
	# expecting wd, species, es to be able to locate arguments file
	es.name = basename(es)
}

# load arguments file
load(paste(wd, "02.init.args.model.project.", species, ".", es.name, ".RData", sep=""))

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
 
# combine predictors into a RasterStack of enviro data
cache.present = grep("maxent.cache", enviro.data)
if (length(cache.present) > 0) { # maxent.cache is present
	enviro.data = enviro.data[-cache.present]
}
climate.scenario = stack(enviro.data)

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# function to get model object
getModelObject = function(model.name) {
	model.dir = paste(wd, "output_", model.name, "/", sep="")
	model.obj = tryCatch(get(load(file=paste(model.dir, "model.object.RData", sep=""))), error = err.null)	
}

# function to save projection output raster
saveModelProjection = function(out.model, model.name) {
	model.dir = paste(wd, "output_", model.name, "/", sep="")
	writeRaster(out.model, paste(model.dir, es.name, sep="/"), format="GTiff")
}

###project the models and save raster files
if (project.bioclim) {
	bioclim.obj = getModelObject("bioclim")	# get the model object
	if (!is.null(bioclim.obj)) {
		bioclim.proj = predict(bioclim.obj, climate.scenario, tails=opt.tails, ext=opt.ext)	# predict for given climate scenario
		saveModelProjection(bioclim.proj, "bioclim") # save output
		rm(list=c("bioclim.obj", "bioclim.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load bioclim.obj from", wd, "output_bioclim", sep=": "), stdout())
	}
} # end if bioclim
	
if (project.domain) {
	domain.obj = getModelObject("domain") # get the model object
	if (!is.null(domain.obj)) {
		# have to drop env layers for DOMAIN if they weren't in original model
		# get the names of the env layers used to create DOMAIN model
		domain.layers = colnames(domain.obj@presence)	# should be the same as 'enviro.data.names'
		# get the names of the climater scenario's env layers
		pred.layers = names(climate.scenario)

		# create a new list of env predictors by dropping layers not in the original model
		predictors.dm = climate.scenario
		for (pl in pred.layers) {
			if (!(pl %in% domain.layers)) {
				predictors.dm = dropLayer(predictors.dm, pl)
			}	
		}
		domain.proj = predict(domain.obj, predictors.dm, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(domain.proj, "domain") # save output
		rm(list=c("domain.obj", "domain.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load domain.obj from", wd, "output_domain", sep=": "), stdout())
	}
}

if (project.mahal) {
	mahal.obj = getModelObject("mahal") # get the model object
	if (!is.null(mahal.obj)) {
		mahal.proj = predict(mahal.obj, climate.scenario, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(mahal.proj, "mahal") 	# save output
		rm(list=c("mahal.obj", "mahal.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load mahal.obj from", wd, "output_mahal", sep=": "), stdout())
	}
}

if (project.geodist) {
	geodist.obj = getModelObject("geodist") # get the model object
	if (!is.null(geodist.obj)) {
		geodist.proj = predict(geodist.obj, climate.scenario, ext=opt.ext, scale=opt.scale) # predict for given climate scenario
		saveModelProjection(geodist.proj, "geodist") 	# save output
		rm(list=c("geodist.obj", "geodist.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geodist.obj from", wd, "output_geodist", sep=": "), stdout())
	}
}

if (project.convHull) {
	convHull.obj = getModelObject("convHull") # get the model object
	if (!is.null(convHull.obj)) {
		convHull.proj = predict(convHull.obj, climate.scenario, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(convHull.proj, "convHull") 	# save output
		rm(list=c("convHull.obj", "convHull.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load convHull.obj from", wd, "output_circles", sep=": "), stdout())
	}
}

if (project.circles) {
	circles.obj = getModelObject("circles") # get the model object
	if (!is.null(circles.obj)) {
		circles.proj = predict(circles.obj, climate.scenario, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(circles.proj, "circles") 	# save output
		rm(list=c("circles.obj", "circles.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load circles.obj from", wd, "output_circles", sep=": "), stdout())
	}
}

if (project.geoIDW) {
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	if (!is.null(geoIDW.obj)) {
		geoIDW.proj = predict(geoIDW.obj, climate.scenario, ext=opt.ext, fun=opt.fun) # predict for given climate scenario
		saveModelProjection(geoIDW.proj, "geoIDW") 	# save output
		rm(list=c("geoIDW.obj", "geoIDW.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geoIDW.obj from", wd, "output_geoIDW", sep=": "), stdout())
	}
}

if (project.voronoiHull) {
	voronoiHull.obj = getModelObject("voronoiHull")   # get the model object
	if (!is.null(voronoiHull.obj)) {
		voronoiHull.proj = 	predict(voronoiHull.obj, climate.scenario, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(voronoiHull.proj, "voronoiHull") 	# save output
		rm(list=c("voronoiHull.obj", "voronoiHull.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load voronoiHull.obj from", wd, "output_voronoiHull", sep=": "), stdout())
	}
}

if (project.brt) {
	brt.obj = getModelObject("brt") # get the model object
	if (!is.null(brt.obj)) {
		# NOTE the order of arguments in the  predict function for brt; this is because
		#	the function is defined outside of the dismo package
		brt.proj = predict(climate.scenario, brt.obj, n.trees=brt.obj$gbm.call$best.trees, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(brt.proj, "brt") 	# save output
		rm(list=c("brt.obj", "brt.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load brt.obj from", wd, "output_brt", sep=": "), stdout())
	}
}

if (project.maxent) {

	#*************** UNDER CONSTRUCTION ***************
	# maxent model creation was run as a system call outside of R, need to do the same for projection
	# EMG check to see if argument defaults / modifiables are the same as during creation
	
	# create output directory
	model.dir = paste(wd, "output_maxent/", sep="")
	
	### not user modified section
	tstr = paste("java -cp ", maxent.jar, " density.Project ", model.dir, species, ".lambdas ", sep="")
	# where to find the climate scenarios
	tstr = paste(tstr, dirname(enviro.data[1]), " ", sep="")
	# where to put, what to name the output
	tstr = paste(tstr, model.dir, es.name, ".asc", sep="")
	# optional arguments
	tstr = paste(tstr, " nowriteclampgrid nowritemess fadebyclamping dontcache", sep="")
	system(tstr)
	
	# EMG cache=FALSE nocache and dontcache all manage to be ignored and a maxent.cache is created
	# EMG 'outputfiletype' = asc, mxe, grd, bil only NOT geotiff; can create *.png ('pictures=TRUE')
}