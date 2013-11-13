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
necessary=c("dismo") #list the libraries needed
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
source(paste(function.path, "/my.Evaluate.models.R", sep=""))
# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# function to get model object
getModelObject = function(model.name) {
	model.dir = paste(wd, "/output_", model.name, sep=""); setwd(model.dir);
	model.obj = tryCatch(get(load(file=paste(model.dir, "/model.object.RData", sep=""))), error = err.null)
}

# function to save evaluate output
saveModelEvaluation = function(out.model, out.combined.model) {
	save(out.model, file=paste(getwd(), "/dismo.eval.object.RData", sep=''))	# save the 'dismo::ModelEvalution' object

	# save all the model accuracy statistics provided in both dismo and biomod2
	rownames(out.combined.model) <- c("Testing.data","Cutoff","Sensitivity", "Specificity")
	write.csv(t(round(out.combined.model, digits=3)), file=paste(getwd(), "/combined.modelEvaluation.csv", sep=""))

	# save AUC curve
	png(file=paste(getwd(), "/roc.png", sep='')); plot(out.model, 'ROC'); dev.off()
	
	# determine and save thresholds
	# threshold() returns kappa, spec_sens, and no_omission thresholds
	dismo.thresh = threshold(out.model)
	minROCdistance = out.model@t[which.min(sqrt((1-out.model@TPR)^2 + (1-out.model@TNR)^2))]
	write("kappa,spec_sens,no_omission,minROCdistance", file=paste(getwd(), "/thresholds.txt", sep=""))
	write(c(as.numeric(dismo.thresh),minROCdistance), file=paste(getwd(), "/thresholds.txt", sep=""), sep=",", append=TRUE)
	# EMG kappa is different from above
}

# function to generate marginal (mean) response curves for dismo models
# i.e., hold all but one predictor variable to its mean value and recalculate model predictions
createMarginalResponseCurves = function(out.model, model.name) {

	# get the enviromental variables and values used to create the model
	if (model.name == "brt") {
		model.values = matrix(out.model$data$x, ncol=length(out.model$var.names)); env.vars = out.model$var.names;
	} else if (model.name %in% c("geoIDW", "voronoiHull")) {
		model.values = rbind(out.model@presence, out.model@absence); env.vars = colnames(model.values);
	} else {
		model.values = out.model@presence; env.vars = colnames(model.values);
	}

	if (!(length(model.values)==0)) {

		# create a matrix to hold average values for each environmental variable
		mean.values = matrix(data = NA, nrow = 100, ncol = length(env.vars)); colnames(mean.values) = env.vars;
		# for each variable, populate the column with the mean value
		for (i in 1:ncol(mean.values)) {
			mean.values[,i] = rep(mean(model.values[,i], na.rm=TRUE), 100)
		}
	
		# allow each environmental variable to vary, keeping other variable values at average, and predict suitability
		for (j in 1:ncol(mean.values)) {
			range.values = seq(min(model.values[,j]), max(model.values[,j]), length.out=100)
			temp.data = mean.values
			temp.data[,j] = range.values
			if (model.name == "brt") {
				colnames(temp.data) = env.vars
				new.predictions = predict(out.model, as.data.frame(temp.data), n.trees = out.model$gbm.call$best.trees, type = "response")
			} else {
				new.predictions = predict(out.model, temp.data)
			}
			
			# create separate file for each response curve
			save.name = env.vars[j]
			png(file=paste(getwd(), "/", save.name, "_response.png", sep=""))
				plot(range.values, new.predictions, ylim=c(0,1), xlab="", ylab="", main=save.name, type="l")
				rug(model.values[,j])
			dev.off()
		}
	} else {
		write(paste(species, ": Cannot create response curves from", model.name, "object", sep=" "), stdout())
	}
}

# function to calculate variable importance values for dismo models based on biomod2's correlation between predictions
# i.e., hold all but one predictor variable to its actual values, resample that one predictor and recalculate model predictions
calculateVariableImpt = function(out.model, model.name, num_samples) {
# EMG num_samples should be same as biomod.VarImport arg set in 01.init.args.model.current.R 
	
	# get the enviromental variables and values used to create the model
	# EMG this is duplicated from above, should be able to combine
	if (model.name == "brt") {
		model.values = matrix(out.model$data$x, ncol=length(out.model$var.names)); env.vars = out.model$var.names;
		colnames(model.values) = env.vars
	} else if (model.name %in% c("geoIDW", "voronoiHull")) {
		model.values = rbind(out.model@presence, out.model@absence); env.vars = colnames(model.values);
	} else {
		model.values = out.model@presence; env.vars = colnames(model.values);
	}

	if (!(length(model.values)==0)) {
	
		# predict using actual values
		if (model.name == "brt") {
			actual.predictions = predict(out.model, as.data.frame(model.values), n.trees = out.model$gbm.call$best.trees, type = "response")
		} else {
			actual.predictions = predict(out.model, model.values)
		} 

		# create a table to hold the output
		varimpt.out = matrix(NA, nrow=length(env.vars), ncol=num_samples+2)
		dimnames(varimpt.out) = list(env.vars, c(paste("sample_", c(1:num_samples, "mean")), "percent"))
		
		# create a copy of the env data matrix
		sample.data = model.values
		
		# for each predictor variable 
		for (p in 1:ncol(sample.data)) {

			# for each num_sample
			for (s in 1:num_samples) {
					
				# resample from that variables' values, keeping other variable values the same, and predict suitability
				sample.data[,p] = sample(x=sample.data[,p], replace=FALSE)

				# predict using sampled values
				if (model.name == "brt") {
					new.predictions = predict(out.model, as.data.frame(sample.data), n.trees = out.model$gbm.call$best.trees, type = "response")
				} else {
					new.predictions = predict(out.model, sample.data)
				}
			
				# calculate correlation between original predictions and new predictions
				varimpt.out[p,s] = 1-max(round(cor(x=actual.predictions, y=new.predictions, use="pairwise.complete.obs",
					method="pearson"), digits=3),0)
			}		
		}
		
		# calculate mean variable importance, normalize to percentages, and write results
		varimpt.out[,num_samples+1] = round(rowMeans(varimpt.out, na.rm=TRUE), digits=3)
		varimpt.out[,num_samples+2] = round((varimpt.out[,num_samples+1]/sum(varimpt.out[,num_samples+1]))*100, digits=0)
		write.csv(varimpt.out, file=paste(getwd(), "/biomod2_like_VariableImportance.csv", sep=""))
	} else {
		write(paste(species, ": Cannot calculate variable importance for ", model.name, "object", sep=" "), stdout())
	}
}

# function to calculate variable importance values for dismo models based on Maxent's decrease in AUC 
# i.e., hold all but one predictor variable to its original values, resample that one predictor and recalculate model AUC
calculatePermutationVarImpt = function(out.model, model.eval, model.name) {
	
	# get the enviromental variables and values used to create the model
	# EMG this is duplicated from above, should be able to combine or find an easier way to determine
	if (model.name == "brt") {
		model.values = matrix(out.model$data$x, ncol=length(out.model$var.names)); env.vars = out.model$var.names;
		colnames(model.values) = env.vars
	} else if (model.name %in% c("geoIDW", "voronoiHull")) {
		model.values = rbind(out.model@presence, out.model@absence); env.vars = colnames(model.values);
	} else {
		model.values = out.model@presence; env.vars = colnames(model.values);
	}

	if (!(length(model.values)==0)) {
	
		# get the occurrence and background environmental data used to evaluate the model
		p.swd=occur
		a.swd=bkgd
			
		# get the AUC from the original model evaluation
		init.auc = round(model.eval@auc, digits=3)
		
		# create a table to hold the output
		permvarimpt.out = matrix(NA, nrow=length(env.vars), ncol=4)
		dimnames(permvarimpt.out) = list(env.vars, c("init.auc", "sample.auc", "change.auc", "percent"))
		permvarimpt.out[,"init.auc"] = rep(init.auc, length(env.vars))
		
		# create a copy of the occurrence and background environmental data
		sample.p = p.swd[,env.vars]
		sample.a = a.swd[,env.vars]
			
		# for each predictor variable 
		for (v in 1:length(env.vars)) {
					
			# resample from that variables' values, keeping other variable values the same 
			sample.p[,v] = sample(x=sample.p[,v], replace=FALSE)
			sample.a[,v] = sample(x=sample.a[,v], replace=FALSE)

			# re-evaluate model with sampled env values
			if (model.name == "brt") {
				sample.eval = evaluate(p=sample.p, a=sample.a, model=brt.obj, n.trees=brt.obj$gbm.call$best.trees)
			} else {
				sample.eval = evaluate(p=sample.p, a=sample.a, model=out.model)
			}
			# get the new auc
			permvarimpt.out[v,"sample.auc"] = round(sample.eval@auc, digits=3)
		}
		
		# calculate the difference in auc, normalize to percentages, and write results
		permvarimpt.out[,"change.auc"] = permvarimpt.out[,"init.auc"] - permvarimpt.out[,"sample.auc"]
		for (r in 1:nrow(permvarimpt.out)) {
			if (permvarimpt.out[r,"change.auc"] < 0) {  # EMG what if AUC increases?
				permvarimpt.out[r,"change.auc"] = 0
			}
		}
		permvarimpt.out[,"percent"] = round((permvarimpt.out[,"change.auc"]/sum(permvarimpt.out[,"change.auc"]))*100, digits=0)
		write.csv(permvarimpt.out, file=paste(getwd(), "/maxent_like_VariableImportance.csv", sep=""))
	} else {
		write(paste(species, ": Cannot calculate maxent-like variable importance for ", model.name, "object", sep=" "), stdout())
	}
}
	
###evaluate the models and save the outputs
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
	
if (evaluate.domain) {
	domain.obj = getModelObject("domain") # get the model object
	if (!is.null(domain.obj)) {
		domain.eval = evaluate(p=occur, a=bkgd, model=domain.obj) # evaluate model using dismo's evaluate
		
		# need predictions and observed values to create confusion matrices for accuracy statistics
		domain.fit = c(domain.eval@presence, domain.eval@absence)
		domain.obs = c(rep(1, length(domain.eval@presence)), rep(0, length(domain.eval@absence)))

		# get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
		domain.combined.eval = sapply(model.accuracy, function(x){
			return(my.Find.Optim.Stat(Stat = x, Fit = domain.fit, Obs = domain.obs))
		})
		saveModelEvaluation(domain.eval, domain.combined.eval)	# save output
				
		# create response curves
		createMarginalResponseCurves(domain.obj, "domain")

		# calculate variable importance (like biomod2, using correlations between predictions)
		calculateVariableImpt(domain.obj, "domain", 3)
		
		# calculate variable importance (like maxent, using decrease in AUC)
		calculatePermutationVarImpt(domain.obj, domain.eval, "domain")
		
		rm(list=c("domain.obj", "domain.eval", "domain.combined.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load domain.obj from", wd, "/output_domain", sep=": "), stdout())
	}
}