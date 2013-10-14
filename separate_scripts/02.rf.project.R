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
necessary=c("biomod2","SDMTools") #list the libraries needed
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
	writeRaster(out.model, paste(model.dir, es.name, sep="/"), format="GTiff", overwrite=TRUE)
}

###project the models and save raster files
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

if (project.rf) {
	rf.obj = getModelObject("rf") # get the model object
	outdir = paste(wd,'/output_rf',sep=''); setwd(outdir) #set the working directory
	if (!is.null(rf.obj)) {
		predictors = checkModelLayers(rf.obj)
		rf.proj = BIOMOD_Projection(modeling.output=rf.obj, new.env=predictors, proj.name=es.name, 
			xy.new.env = biomod.xy.new.env,	selected.models = biomod.selected.models, binary.meth = biomod.binary.meth, 
			filtered.meth = biomod.filtered.meth, compress = biomod.compress, 
			build.clamping.mask = biomod.build.clamping.mask, silent = opt.biomod.silent, do.stack = opt.biomod.do.stack, 
			keep.in.memory = opt.biomod.keep.in.memory,	output.format = opt.biomod.output.format)
		# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
		rm(list=c("rf.obj", "rf.proj")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load rf.obj from", wd, "/output_rf", sep=": "), stdout())
	}
}