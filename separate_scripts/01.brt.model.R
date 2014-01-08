#script to run to develop distribution models

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
load(paste(wd, "/01.init.args.model.", species, ".RData", sep=""))

###check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###load in the data
if (file.exists(occur.data) && file.exists(bkgd.data)) {
	load(occur.data); load(bkgd.data);
} else {
	stop("No occurrence or background data available for model creation!")
}

###run the models and store models
#############################################################################################
#
# MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
#
#############################################################################################
###############
#
# BRT
#
###############
# gbm.step(data, gbm.x, gbm.y, offset = NULL, fold.vector = NULL, tree.complexity = 1, 
#	learning.rate = 0.01, bag.fraction = 0.75, site.weights = rep(1, nrow(data)), 
#	var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE, 
#	family = "bernoulli", n.trees = 50, step.size = n.trees, max.trees = 10000,
#	tolerance.method = "auto", tolerance = 0.001, keep.data = FALSE, plot.main = TRUE, 
#	plot.folds = FALSE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, 
#	keep.fold.vector = FALSE, keep.fold.fit = FALSE, ...)
# data input data.frame
# gbm.x predictor variables
# gbm.y	response variable
# offset = NULL
# fold.vector = NULL	a fold vector to be read in for cross validation with offsets
# tree.complexity = 1	sets the complexity of individual trees
# learning.rate = 0.01	sets the weight applied to individual trees
# bag.fraction = 0.75	sets the proportion of observations used in selecting variables
# site.weights = rep(1, nrow(data))	allows varying weighting for sites
# var.monotone = rep(0, length(gbm.x))	restricts responses to individual predictors to monotone
# n.folds = 10	number of folds
# prev.stratify = TRUE	prevalence stratify the folds - only for presence/absence data
# family = "bernoulli"	family - bernoulli (=binomial), poisson, laplace or gaussian
# n.trees = 50	number of initial trees to fit
# step.size = n.trees	numbers of trees to add at each cycle
# max.trees = 10000	max number of trees to fit before stopping
# tolerance.method = "auto"	method to use in deciding to stop - "fixed" or "auto"
# tolerance = 0.001	tolerance value to use - if method == fixed is absolute, 
#	if auto is multiplier * total mean deviance
# keep.data = FALSE	Logical. keep raw data in final model
# plot.main = TRUE	Logical. plot hold-out deviance curve
# plot.folds = FALSE	Logical. plot the individual folds as well
# verbose = TRUE	Logical. control amount of screen reporting
# silent = FALSE	Logical. to allow running with no output for simplifying model)
# keep.fold.models = FALSE 	Logical. keep the fold models from cross valiation
# keep.fold.vector = FALSE	Logical. allows the vector defining fold membership to be kept
# keep.fold.fit = FALSE	Logical. allows the predicted values for observations from cross-validation 
#	to be kept

if (model.brt) {
	outdir = paste(wd,'/output_brt',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	brt.data = rbind(occur,bkgd); brt.data$pa = c(rep(1,nrow(occur)),rep(0,nrow(bkgd))) #setup the data as needed
	brt = gbm.step(data=brt.data, gbm.x=which(names(brt.data) %in% enviro.data.names), 
		gbm.y=which(names(brt.data)=='pa'), 
		fold.vector = brt.fold.vector, 
		tree.complexity = brt.tree.complexity, 
		learning.rate = brt.learning.rate, 
		bag.fraction = brt.bag.fraction, 
		site.weights = brt.site.weights, 
		var.monotone = brt.var.monotone, 
		n.folds = brt.n.folds, 
		prev.stratify = brt.prev.stratify, 
		family = brt.family, 
		n.trees = brt.n.trees, 
		step.size = brt.step.size, 
		max.trees = brt.max.trees, 
		tolerance.method = brt.tolerance.method, 
		tolerance = brt.tolerance, 
		keep.data = brt.keep.data, 
		plot.main = brt.plot.main, 
		plot.folds = brt.plot.folds, 
		verbose = brt.verbose, 
		silent = brt.silent, 
		keep.fold.models = brt.keep.fold.models, 
		keep.fold.vector = brt.keep.fold.vector, 
		keep.fold.fit = brt.keep.fold.fit) #run the algorithm
	if (!is.null(brt)) {	
		save(brt,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
	} else {
		write(paste("FAIL!", species, "Cannot create brt model object", sep=": "), stdout())
	}
}