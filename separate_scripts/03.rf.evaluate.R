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

# load in the data
if (file.exists(paste(wd, "/occur.RData", sep="")) && file.exists(paste(wd, "/bkgd.RData", sep=""))) {
	load(paste(wd, "/occur.RData", sep="")); load(paste(wd, "/bkgd.RData", sep=""));
} else {
	warning("No occurrence or background data available for model evaulation!")
}

# source my modified version of biomod2's Evaluate.models.R for consistent model accuracy statistics
source("/home/jc140298/bccvl/my.Evaluate.models.R")
# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# function to get model object
getModelObject = function(model.name) {
	model.dir = paste(wd, "/output_", model.name, "/", sep=""); setwd(model.dir);
	model.obj = tryCatch(get(load(file=paste(model.dir, "/model.object.RData", sep=""))), error = err.null)
}

# function to save model object, response curves, and variable importance
saveBIOMODModelEvaluation = function(loaded.name, biomod.model) {
	# get and save the model evaluation statistics
	# EMG these must specified during model creation with the arg "models.eval.meth"
	evaluation = getModelsEvaluations(biomod.model)
	write.csv(evaluation, file=paste(getwd(), "/biomod2.modelEvaluation.txt", sep=""))

	# get the model predictions and observed values
	predictions = getModelsPrediction(biomod.model); obs = getModelsInputData(biomod.model, "resp.var");

	# get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
	combined.eval = sapply(model.accuracy, function(x){
		return(my.Find.Optim.Stat(Stat = x, Fit = predictions, Obs = obs))
	})
	# save all the model accuracy statistics provided in both dismo and biomod2
	rownames(combined.eval) <- c("Testing.data","Cutoff","Sensitivity", "Specificity")
	write.csv(t(round(combined.eval, digits=3)), file=paste(getwd(), "/combined.modelEvaluation.csv", sep=""))
		
	# save AUC curve
	require(pROC, quietly=T)
    roc1 <- roc(as.numeric(obs), as.numeric(predictions), percent=T)
	png(file=paste(getwd(), "/pROC.png", sep=''))
	plot(roc1, main=paste("AUC=",round(auc(roc1)/100,3),sep=""), legacy.axes=TRUE)
	dev.off()
	
	# get and save the variable importance estimates
	variableImpt = getModelsVarImport(biomod.model)
	if (!is.na(variableImpt)) {
	#EMG Note this will throw a warning message if variables (array) are returned	
		write.csv(variableImpt, file=paste(getwd(), "/variableImportance.txt", sep=""))
	} else {
		message("VarImport argument not specified during model creation!")
		#EMG must create the model with the arg "VarImport" != 0
	}

	# save response curves (Elith et al 2005)
	png(file=paste(getwd(), "/mean_response_curves.png", sep=''))
		test=response.plot2(models = loaded.name, Data = getModelsInputData(biomod.model,"expl.var"),
			show.variables = getModelsInputData(biomod.model,"expl.var.names"), fixed.var.metric = "mean") 
			#, data_species = getModelsInputData(biomod.model,"resp.var"))
			# EMG need to investigate why you would want to use this option - uses presence data only
	dev.off()
}
	
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

if (evaluate.rf) {	
	rf.obj = getModelObject("rf") # get the model object
	if (!is.null(rf.obj)) {
		rf.loaded.model = BIOMOD_LoadModels(rf.obj, models="RF") # load model
		saveBIOMODModelEvaluation(rf.loaded.model, rf.obj) 	# save output
		rm("rf.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load rf.obj from", getwd(), sep=": "), stdout())
	}
}



