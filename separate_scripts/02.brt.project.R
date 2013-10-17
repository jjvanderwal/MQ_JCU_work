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
load(paste(wd, "/02.init.args.project.", species, ".", es.name, ".RData", sep=""))

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

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
	model.dir = paste(wd, "/output_", model.name, sep="")
	model.obj = tryCatch(get(load(file=paste(model.dir, "/model.object.RData", sep=""))), error = err.null)	
}

# function to check that the environmental layers used to project the  model are the same as the ones used
# 	to create the model object 
checkModelLayers = function(model.obj) {

	message("Checking environmental layers used for projection")
	# get the names of the environmental layers from the original model
	if (inherits(model.obj, "DistModel")) { # dismo package
		model.layers = colnames(model.obj@presence)
	} else if (inherits(model.obj, "gbm")) { # brt package
		model.layers = summary(brt.obj)$var
	} else if (inherits(model.obj, "BIOMOD.models.out")) { # biomod package
		model.layers = model.obj@expl.var.names
	}
	
	# get the names of the climate scenario's env layers
	pred.layers = names(climate.scenario)
	
	# check if the env layers were in the original model
    if(sum(!(pred.layers %in% model.layers)) > 0 ){
		message("Dropping environmental layers not used in the original model creation...")
		# create a new list of env predictors by dropping layers not in the original model
		new.predictors = climate.scenario
		for (pl in pred.layers) {
			if (!(pl %in% model.layers)) {
				new.predictors = dropLayer(new.predictors, pl)
			}	
		}
		return(new.predictors)
	} else {
		return(climate.scenario)
	}
}

# function to save projection output raster
saveModelProjection = function(out.model, model.name) {
	model.dir = paste(wd, "/output_", model.name, sep="")
	writeRaster(out.model, paste(model.dir, es.name, sep="/"), format="GTiff", overwrite=TRUE, options=c("COMPRESS=LZW"))
}

###project the models and save raster files
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

if (project.brt) {
	brt.obj = getModelObject("brt") # get the model object
	if (!is.null(brt.obj)) {
		predictors = checkModelLayers(brt.obj)
		# NOTE the order of arguments in the  predict function for brt; this is because
		#	the function is defined outside of the dismo package
		brt.proj = predict(predictors, brt.obj, n.trees=brt.obj$gbm.call$best.trees, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(brt.proj, "brt") 	# save output
		rm(list=c("brt.obj", "brt.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load brt.obj from", wd, "/output_brt", sep=": "), stdout())
	}
}