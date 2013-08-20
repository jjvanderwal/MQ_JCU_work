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
necessary=c("dismo","SDMTools","gbm","gstat","deldir", "biomod2") #list the libraries needed
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
	
	# get the names of the climater scenario's env layers
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
	model.dir = paste(wd, "output_", model.name, "/", sep="")
	writeRaster(out.model, paste(model.dir, es.name, sep="/"), format="GTiff", overwrite=TRUE)
}

###project the models and save raster files
if (project.bioclim) {
	bioclim.obj = getModelObject("bioclim")	# get the model object
	if (!is.null(bioclim.obj)) {
		predictors = checkModelLayers(bioclim.obj)
		bioclim.proj = predict(bioclim.obj, predictors, tails=opt.tails, ext=opt.ext)	# predict for given climate scenario
		saveModelProjection(bioclim.proj, "bioclim") # save output
		rm(list=c("bioclim.obj", "bioclim.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load bioclim.obj from", wd, "output_bioclim", sep=": "), stdout())
	}
} # end if bioclim
	
if (project.domain) {
	domain.obj = getModelObject("domain") # get the model object
	if (!is.null(domain.obj)) {
		predictors = checkModelLayers(domain.obj)
		domain.proj = predict(domain.obj, predictors, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(domain.proj, "domain") # save output
		rm(list=c("domain.obj", "domain.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load domain.obj from", wd, "output_domain", sep=": "), stdout())
	}
}

if (project.mahal) {
	mahal.obj = getModelObject("mahal") # get the model object
	if (!is.null(mahal.obj)) {
		predictors = checkModelLayers(mahal.obj)
		mahal.proj = predict(mahal.obj, predictors, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(mahal.proj, "mahal") 	# save output
		rm(list=c("mahal.obj", "mahal.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load mahal.obj from", wd, "output_mahal", sep=": "), stdout())
	}
}

if (project.geodist) {
	geodist.obj = getModelObject("geodist") # get the model object
	if (!is.null(geodist.obj)) {
		predictors = checkModelLayers(geodist.obj)
		geodist.proj = predict(geodist.obj, predictors, ext=opt.ext, scale=opt.scale) # predict for given climate scenario
		saveModelProjection(geodist.proj, "geodist") 	# save output
		rm(list=c("geodist.obj", "geodist.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geodist.obj from", wd, "output_geodist", sep=": "), stdout())
	}
}

if (project.convHull) {
	convHull.obj = getModelObject("convHull") # get the model object
	if (!is.null(convHull.obj)) {
		predictors = checkModelLayers(convHull.obj)
		convHull.proj = predict(convHull.obj, predictors, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(convHull.proj, "convHull") 	# save output
		rm(list=c("convHull.obj", "convHull.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load convHull.obj from", wd, "output_circles", sep=": "), stdout())
	}
}

if (project.circles) {
	circles.obj = getModelObject("circles") # get the model object
	if (!is.null(circles.obj)) {
		predictors = checkModelLayers(circles.obj)
		circles.proj = predict(circles.obj, predictors, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(circles.proj, "circles") 	# save output
		rm(list=c("circles.obj", "circles.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load circles.obj from", wd, "output_circles", sep=": "), stdout())
	}
}

if (project.geoIDW) {
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	if (!is.null(geoIDW.obj)) {
		predictors = checkModelLayers(geoIDW.obj)
		geoIDW.proj = predict(geoIDW.obj, predictors, ext=opt.ext, fun=opt.fun) # predict for given climate scenario
		saveModelProjection(geoIDW.proj, "geoIDW") 	# save output
		rm(list=c("geoIDW.obj", "geoIDW.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geoIDW.obj from", wd, "output_geoIDW", sep=": "), stdout())
	}
}

if (project.voronoiHull) {
	voronoiHull.obj = getModelObject("voronoiHull")   # get the model object
	if (!is.null(voronoiHull.obj)) {
		predictors = checkModelLayers(voronoiHull.obj)
		voronoiHull.proj = 	predict(voronoiHull.obj, predictors, ext=opt.ext) # predict for given climate scenario
		saveModelProjection(voronoiHull.proj, "voronoiHull") 	# save output
		rm(list=c("voronoiHull.obj", "voronoiHull.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load voronoiHull.obj from", wd, "output_voronoiHull", sep=": "), stdout())
	}
}

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

############### BIOMOD2 Models ###############
###############
#
# BIOMOD_Projection(modeling.output, new.env, proj.name, xy.new.env = NULL, selected.models = 'all', 
#	binary.meth = NULL, filtered.meth = NULL, compress = 'xz', build.clamping.mask = TRUE, ...)
#
# modeling.output "BIOMOD.models.out" object produced by a BIOMOD_Modeling run
# new.env A set of explanatory variables onto which models will be projected. It could be a data.frame, a matrix, 
#	or a rasterStack object. Make sure the column names (data.frame or matrix) or layerNames (rasterStack) perfectly 
#	match with the names of variables used to build the models in the previous steps.
# proj.name	a character defining the projection name (a new folder will be created with this name)
# xy.new.env optional coordinates of new.env data. Ignored if new.env is a rasterStack
# selected.models 'all' when all models have to be used to render projections or a subset vector of modeling.output 
#	models computed (accessing with the slot @models.computed of your "BIOMOD.models.out" object)
#	EMG: eg, myBiomodModelOut@models.computed[grep("_RF", getModelsBuiltModels(myBiomodModelOut))]
# binary.meth a vector of a subset of models evaluation method computed before (see BIOMOD_Modeling). If NULL then 
#	no binary transformation computed, else the given binary techniques will be used to transform the projection 
#	into 0/1 data.
#	EMG: dimnames(getModelsEvaluations(myBiomodModelOut)[[1]] to see which methods were used
# filtered.meth	a vector of a subset of models evaluation method computed before (see BIOMOD_Modeling). If NULL then 
#	no filtering transformation computed, else the given binary techniques will be used to transform the projection by 
#	settting to 0 the probability values below a specific threshold.
# compress compression format of objects stored on your hard drive. May be one of ‘xz’, ‘gzip’ or NULL
# build.clamping.mask if TRUE, a clamping mask that identifies locations where predictions are uncertain because the
#	values of the variables are outside the range used for calibrating the models will be saved
#	EMG: It appears that setting this to FALSE will still generate a mask file
# ... Additional arguments:
# silent logical, if TRUE, console outputs are turned off
# do.stack logical, if TRUE, attempt to save all projections in a unique object i.e RasterStack. If FALSE or if objects are
#	too heavy to be load all together in memory, projections will be stored into separated files
# keep.in.memory logical, if FALSE only the link pointing to a hard drive copy of projections are stored in output object. 
#	That can be usefull to prevent memory issues.
# output.format whether ‘.Rdata’, ‘.grd’ or ‘.img’ defining projections saving format (on hard drive). If new.env argument 
#	is under table format (data.frame or matrix), the only choice you have is ‘.Rdata’
#	EMG: If .grd is selected (meta-data), also get a .gri file (values)
#
###############

if (project.glm) {
	glm.obj = getModelObject("glm") # get the model object
	outdir = paste(wd,'output_glm/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(glm.obj)) {
		predictors = checkModelLayers(glm.obj)
		glm.proj = BIOMOD_Projection(modeling.output=glm.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("glm.obj", "glm.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load glm.obj from", wd, "output_glm", sep=": "), stdout())
	}
}
	
if (project.gam) {
	gam.obj = getModelObject("gam") # get the model object
	outdir = paste(wd,'output_gam/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(gam.obj)) {
		predictors = checkModelLayers(gam.obj)
		gam.proj = BIOMOD_Projection(modeling.output=gam.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("gam.obj", "gam.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load gam.obj from", wd, "output_gam", sep=": "), stdout())
	}
}

if (project.gbm) {
	gbm.obj = getModelObject("gbm") # get the model object
	outdir = paste(wd,'output_gbm/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(gbm.obj)) {
		predictors = checkModelLayers(gbm.obj)
		gbm.proj = BIOMOD_Projection(modeling.output=gbm.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("gbm.obj", "gbm.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load gbm.obj from", wd, "output_gbm", sep=": "), stdout())
	}
}

if (project.cta) {
	cta.obj = getModelObject("cta") # get the model object
	outdir = paste(wd,'output_cta/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(cta.obj)) {
		predictors = checkModelLayers(cta.obj)
		cta.proj = BIOMOD_Projection(modeling.output=cta.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("cta.obj", "cta.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load cta.obj from", wd, "output_cta", sep=": "), stdout())
	}
}

if (project.ann) {	
	ann.obj = getModelObject("ann") # get the model object
	outdir = paste(wd,'output_ann/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(ann.obj)) {
		predictors = checkModelLayers(ann.obj)
		ann.proj = BIOMOD_Projection(modeling.output=ann.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("ann.obj", "ann.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load ann.obj from", wd, "output_ann", sep=": "), stdout())
	}
}

if (project.sre) {
	sre.obj = getModelObject("sre") # get the model object
	outdir = paste(wd,'output_sre/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(sre.obj)) {
		predictors = checkModelLayers(sre.obj)
		sre.proj = BIOMOD_Projection(modeling.output=sre.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("sre.obj", "sre.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load sre.obj from", wd, "output_sre", sep=": "), stdout())
	}
}

if (project.fda) {
	fda.obj = getModelObject("fda") # get the model object
	outdir = paste(wd,'output_fda/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(fda.obj)) {
		predictors = checkModelLayers(fda.obj)
		fda.proj = BIOMOD_Projection(modeling.output=fda.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("fda.obj", "fda.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load fda.obj from", wd, "output_fda", sep=": "), stdout())
	}
}

if (project.mars) {
	mars.obj = getModelObject("mars") # get the model object
	outdir = paste(wd,'output_mars/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(mars.obj)) {
		predictors = checkModelLayers(mars.obj)
		mars.proj = BIOMOD_Projection(modeling.output=mars.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("mars.obj", "mars.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load mars.obj from", wd, "output_mars", sep=": "), stdout())
	}
}

if (project.rf) {
	rf.obj = getModelObject("rf") # get the model object
	outdir = paste(wd,'output_rf/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(rf.obj)) {
		predictors = checkModelLayers(rf.obj)
		rf.proj = BIOMOD_Projection(modeling.output=rf.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("rf.obj", "rf.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load rf.obj from", wd, "output_rf", sep=": "), stdout())
	}
}

#*************** UNDER CONSTRUCTION ***************
if (project.biomod.maxent) {	
	biomod.maxent.obj = getModelObject("biomod.maxent") # get the model object
	outdir = paste(wd,'output_biomod.maxent/',sep=''); setwd(outdir) #set the working directory
	if (!is.null(biomod.maxent.obj)) {
		predictors = checkModelLayers(biomod.maxent.obj)
		biomod.maxent.proj = BIOMOD_Projection(modeling.output=biomod.maxent.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specied in arg 'opt.biomod.output.format'
		rm(list=c("biomod.maxent.obj", "biomod.maxent.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load biomod.maxent.obj from", wd, "output_biomod.maxent", sep=": "), stdout())
	}
}
