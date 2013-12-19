#script to evaluate distribution models

# read in the arguments listed at the command line in shell script
args=(commandArgs(TRUE))  
# check to see if arguments are passed
if(length(args)==0){
    print("No arguments supplied.")
    # leave all args as default values
} else {
	for(i in 1:length(args)) { 
		eval(parse(text=args[[i]])) 
	}
	# expecting wd and species to be able to locate arguments file
}

# load arguments file
load(paste(wd, "/03.init.args.evaluate.", species, ".RData", sep=""))

### check if libraries are installed, install if necessary and then load them
necessary=c("biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###read in the necessary observation and background data
occur = read.csv(occur.data) #read in the observation data lon/lat
bkgd = read.csv(bkgd.data) #read in the background data lon/lat

# source helper functions (saveBIOMODModelEvaluation)
source(paste(function.path, "/my.Helper.Functions.R", sep=""))

# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)

###evaluate the models and save the outputs
############### BIOMOD2 Models ###############
###############
#
# getModelsEvaluations(obj) #evaluations of each model computed according to selected evaluation methods
# 	obj	"BIOMOD.models.out" object
#		best.iter: the best score obtained for chosen statistic
#		cutoff: the associated cut-off used for transform fitted vector into binary
#		sensibility: the sensibility with this threshold
#		specificity: the specificity with this threshold
#
# getModelsVarImport(obj) #models variable importances
#	obj	"BIOMOD.models.out" object
#
# getModelsPrediction(obj, ...) #predictions of each models on their own calibration + validation dataset
# getModelsPredictionEval(obj, ...) #predictions of each models on evaluation dataset (if defined)
# 	obj	"BIOMOD.models.out" object
#	as.data.frame: logical If TRUE, models predictions will be returned as data.frame rather than array
# 
# response.plot2() # plot of predicted responses in 2 or 3 dimensions (Elith et al 2005)
#
###############
		
if (evaluate.gam) {
	gam.obj = getModelObject("gam") # get the model object
	if (!is.null(gam.obj)) {
		outdir = paste(wd,'/output_gam',sep=''); setwd(outdir)
		gam.loaded.model = BIOMOD_LoadModels(gam.obj, models="GAM") # load model
		saveBIOMODModelEvaluation(gam.loaded.model, gam.obj) 	# save output
	} else {
		write(paste("FAIL!", species, "Cannot load gam.obj from", getwd(), sep=": "), stdout())
	}
}