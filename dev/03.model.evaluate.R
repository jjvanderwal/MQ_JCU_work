#script to evaluate distribution models

#source("/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/dev/03.init.args.model.evaluate.R") #read in the initial arguments
source("/home/jc140298/bccvl/03.init.args.model.evaluate.R")

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

setwd(wd)
load("occur.RData"); load("bkgd.RData"); #load in the data

###############
#
# evaluate(p, a, model, x, tr, ...)
#
# p presence points (x and y coordinate or SpatialPoints* object)
#	Or, if x is missing, values at presence points (EMG: values returned by a predict())
#	Or, a matrix with values to compute predictions for
# a absence points (x and y coordinate or SpatialPoints* object)
#	Or, if x is missing, values at absence points (EMG: values returned by a predict())
#	Or, a matrix with values to compute predictions for
# model any fitted model, including objects inheriting from 'DistModel'; not used when x is missing
# x 	Optional. Predictor values (object of class Raster*). If present, p and a are interpreted
#	as (spatial) points (EMG: lon/lat)
# tr 	Optional. a vector of threshold values to use for computing the confusion matrices
# ...	Additional arguments for the predict function (EMG: evaluate() calls predict())
#
# 'ModelEvaluation' output based on Fielding and Bell (1997) with attributes:
# presence - presence data used 
# absence - absence data used
# np - number of presence points
# na - number of absence points
# auc - Area under the receiver operator (ROC) curve
# pauc - p-value for the AUC (for the Wilcoxon test W statistic
# cor - Correlation coefficient
# pcor - p-value for correlation coefficient 
# t - vector of thresholds used to compute confusion matrices 
# confusion - confusion matrices 
# prevalence - Prevalence 
# ODP - Overall diagnostic power 
# CCR - Correct classification rate 
# TPR - True positive rate 
# TNR - True negative rate 
# FPR - False positive rate 
# FNR - False negative rate 
# PPP - Positive predictive power 
# NPP - Negative predictive power 
# MCR - Misclassification rate 
# OR - Odds-ratio 
# kappa - Cohen's kappa 
#
###############

# function to get model object
getModelObject = function(model.name) {

	model.dir = paste(wd, "output_", model.name, "/", sep="")
	model.obj = get(load(file=paste(model.dir, "model.object.RData", sep="")))
	return (model.obj)
}

# function to save evaluate output
saveModelEvaluation = function(out.model, model.name) {
	
	model.dir = paste(wd, "output_", model.name, "/", sep="")	# set the output directory for eval object
	save(out.model, file=paste(model.dir, "eval.object.RData", sep=''))	# save the 'ModelEvalution' object

	# Elith et al 2006 compares the AUC, COR, and Kappa values to assess predictive performance
	# the Area under the Receiver Operating Characteristic curve (AUC) - ability to discriminate between sites where sp if present versus absent
	# the correlation (COR) - between the observation in the PA dataset and the prediction
	# Kappa - chance-corrected measure of agreement, requires a threshold
	elith = c(out.model@auc, as.numeric(out.model@cor), max(out.model@kappa), out.model@t[which.max(out.model@kappa)])
	write("AUC,COR,maxKAPPA,threshold@MaxKAPPA", file=paste(model.dir, "elith.txt", sep=""), append=TRUE)
	write(elith, file=paste(model.dir, "elith.txt", sep=""), append=TRUE, sep=",")

	# save AUC curve
	png(file=paste(model.dir, "roc.png", sep='')); plot(out.model, 'ROC'); dev.off()
	
}

if (evaluate.bioclim) {

	bioclim.obj = getModelObject("bioclim")	# get the model object
	bioclim.eval = evaluate(p=occur, a=bkgd, model=bioclim.obj)	# evaluate model
	saveModelEvaluation(bioclim.eval, "bioclim")	# save output
	rm(list=c("bioclim.obj", "bioclim.eval")); #clean up the memory
}
	
if (evaluate.domain) {

	domain.obj = getModelObject("domain") # get the model object
	domain.eval = evaluate(p=occur, a=bkgd, model=domain.obj) # evaluate model
	saveModelEvaluation(domain.eval, "domain") 	# save output
	rm(list=c("domain.obj", "domain.eval")); #clean up the memory
}

if (evaluate.mahal) {
	
	mahal.obj = getModelObject("mahal") # get the model object
	mahal.eval = evaluate(p=occur, a=bkgd, model=mahal.obj) # evaluate model
	saveModelEvaluation(mahal.eval, "mahal") 	# save output
	rm(list=c("mahal.obj", "mahal.eval")); #clean up the memory
}

if (evaluate.geodist) {
	
	geodist.obj = getModelObject("geodist") # get the model object
	geodist.eval = evaluate(model=geodist.obj, p=occur, a=bkgd) # evaluate model
#EMG NOTE: no error for p,a if columns not specified c.f. convHull
	saveModelEvaluation(geodist.eval, "geodist") 	# save output
	rm(list=c("geodist.obj", "geodist.eval")); #clean up the memory
}

if (evaluate.convHull) {
	
	convHull.obj = getModelObject("convHull") # get the model object
	convHull.eval = evaluate(model=convHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
	saveModelEvaluation(convHull.eval, "convHull") 	# save output
	rm(list=c("convHull.obj", "convHull.eval")); #clean up the memory
}

if (evaluate.circles) {
	
	circles.obj = getModelObject("circles") # get the model object
	circles.eval = evaluate(model=circles.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
	saveModelEvaluation(circles.eval, "circles") 	# save output
	rm(list=c("circles.obj", "circles.eval")); #clean up the memory
}

if (evaluate.geoIDW) {
	
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	geoIDW.eval = evaluate(model=geoIDW.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
	saveModelEvaluation(geoIDW.eval, "geoIDW") 	# save output
	rm(list=c("geoIDW.obj", "geoIDW.eval")); #clean up the memory
}

if (evaluate.voronoiHull) {
	
	voronoiHull.obj = getModelObject("voronoiHull") # get the model object
	voronoiHull.eval = evaluate(model=voronoiHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
	saveModelEvaluation(voronoiHull.eval, "voronoiHull") 	# save output
	rm(list=c("voronoiHull.obj", "voronoiHull.eval")); #clean up the memory
}

if (evaluate.brt) {
	
	brt.obj = getModelObject("brt") # get the model object
	# NOTE the order of arguments in the  predict function for brt; this is because
	#	the function is defined outside of the dismo package
	brt.eval = evaluate(p=occur, a=bkgd, model=brt.obj, n.trees=brt.obj$gbm.call$best.trees) # evaluate model
	saveModelEvaluation(brt.eval, "brt") 	# save output
	rm(list=c("brt.obj", "brt.eval")); #clean up the memory
}

if (evaluate.maxent) {

	# EMG I believe maxent does model evaluation during fitting
	cat(paste("See MAXENT output directory: ", wd, "output_maxent/\n", sep=""))
}