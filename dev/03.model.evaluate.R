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
load(paste(wd, "03.init.args.model.evaluate.", species, ".RData", sep=""))

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir", "biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

# load in the data
if (file.exists(paste(wd, "occur.RData", sep="")) && file.exists(paste(wd, "bkgd.RData", sep=""))) {
	load(paste(wd, "occur.RData", sep="")); load(paste(wd, "bkgd.RData", sep=""));
} else {
	warning("No occurrence or background data available for model evaulation!")
}

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

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# function to get model object
getModelObject = function(model.name) {
	model.dir = paste(wd, "output_", model.name, "/", sep=""); setwd(model.dir);
	model.obj = tryCatch(get(load(file=paste(model.dir, "model.object.RData", sep=""))), error = err.null)
}

# function to save evaluate output
saveModelEvaluation = function(out.model) {
	model.dir = paste(getwd(), "/", sep="")
	save(out.model, file=paste(getwd(), "/eval.object.RData", sep=''))	# save the 'ModelEvalution' object
	
	# true skill statistic (TSS)
	#tss = sensitivity + specificity - 1 == TPR + TNR - 1
	# can be derived from evaluate() model output
	#tss = out.model@TPR + out.model$TNR - 1
	#EMG one for each confusion matrix, do we want to save them in a separate file?
	
	#omission = out.model@FNR
	#EMG one for each confusion matrix, do we want to save them in a separate file?
	
	# Elith et al 2006 compares the AUC, COR, and Kappa values to assess predictive performance
	# the Area under the Receiver Operating Characteristic curve (AUC) - ability to discriminate between sites where sp if present versus absent
	# the correlation (COR) - between the observation in the PA dataset and the prediction
	# Kappa - chance-corrected measure of agreement, requires a threshold
	elith = c(out.model@auc, as.numeric(out.model@cor), max(out.model@kappa), out.model@t[which.max(out.model@kappa)])
	write("AUC,COR,maxKAPPA,threshold@MaxKAPPA", file=paste(model.dir, "elith.txt", sep=""))
	write(elith, file=paste(model.dir, "elith.txt", sep=""), sep=",", append=TRUE)

	# save AUC curve
	png(file=paste(model.dir, "roc.png", sep='')); plot(out.model, 'ROC'); dev.off()
	
	# determine and save thresholds
	# threshold() returns kappa, spec_sens, and no_omission thresholds
	dismo.thresh = threshold(out.model)
	minROCdistance = out.model@t[which.min(sqrt((1-out.model@TPR)^2 + (1-out.model@TNR)^2))]
	write("kappa,spec_sens,no_omission,minROCdistance", file=paste(model.dir, "thresholds.txt", sep=""))
	write(c(as.numeric(dismo.thresh),minROCdistance), file=paste(model.dir, "thresholds.txt", sep=""), sep=",", append=TRUE)
}

###evaluate the models and save the outputs
if (evaluate.bioclim) {
	bioclim.obj = getModelObject("bioclim")	# get the model object
	if (!is.null(bioclim.obj)) {
		bioclim.eval = evaluate(p=occur, a=bkgd, model=bioclim.obj)	# evaluate model
		saveModelEvaluation(bioclim.eval)	# save output
		rm(list=c("bioclim.obj", "bioclim.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load bioclim.obj from", wd, "output_bioclim", sep=": "), stdout())
	}
} # end if bioclim
	
if (evaluate.domain) {
	domain.obj = getModelObject("domain") # get the model object
	if (!is.null(domain.obj)) {
		domain.eval = evaluate(p=occur, a=bkgd, model=domain.obj) # evaluate model
		saveModelEvaluation(domain.eval) 	# save output
		rm(list=c("domain.obj", "domain.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load domain.obj from", wd, "output_domain", sep=": "), stdout())
	}
}

if (evaluate.mahal) {
	mahal.obj = getModelObject("mahal") # get the model object
	if (!is.null(mahal.obj)) {
		mahal.eval = evaluate(p=occur, a=bkgd, model=mahal.obj) # evaluate model
		saveModelEvaluation(mahal.eval) 	# save output
		rm(list=c("mahal.obj", "mahal.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load mahal.obj from", wd, "output_mahal", sep=": "), stdout())
	}
}

if (evaluate.geodist) {
	geodist.obj = getModelObject("geodist") # get the model object
	if (!is.null(geodist.obj)) {
		geodist.eval = evaluate(model=geodist.obj, p=occur, a=bkgd) # evaluate model
		#EMG NOTE: no error for p,a if columns not specified c.f. convHull
		saveModelEvaluation(geodist.eval) 	# save output
		rm(list=c("geodist.obj", "geodist.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geodist.obj from", wd, "output_geodist", sep=": "), stdout())
	}
}

if (evaluate.convHull) {
	convHull.obj = getModelObject("convHull") # get the model object
	if (!is.null(convHull.obj)) {
		convHull.eval = evaluate(model=convHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(convHull.eval) 	# save output
		rm(list=c("convHull.obj", "convHull.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load convHull.obj from", wd, "output_convHull", sep=": "), stdout())
	}
}

if (evaluate.circles) {
	circles.obj = getModelObject("circles") # get the model object
	if (!is.null(circles.obj)) {
		circles.eval = evaluate(model=circles.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(circles.eval) 	# save output
		rm(list=c("circles.obj", "circles.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load circles.obj from", wd, "output_circles", sep=": "), stdout())
	}
}

if (evaluate.geoIDW) {
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	if (!is.null(geoIDW.obj)) {
		geoIDW.eval = evaluate(model=geoIDW.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(geoIDW.eval) 	# save output
		rm(list=c("geoIDW.obj", "geoIDW.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load geoIDW.obj from", wd, "output_geoIDW", sep=": "), stdout())
	}
}

if (evaluate.voronoiHull) {
	voronoiHull.obj = getModelObject("voronoiHull") # get the model object
	if (!is.null(voronoiHull.obj)) {
		voronoiHull.eval = evaluate(model=voronoiHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(voronoiHull.eval) 	# save output
		rm(list=c("voronoiHull.obj", "voronoiHull.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load voronoiHull.obj from", wd, "output_voronoiHull", sep=": "), stdout())
	}
}

if (evaluate.brt) {
	brt.obj = getModelObject("brt") # get the model object
	if (!is.null(brt.obj)) {
		# NOTE the order of arguments in the  predict function for brt; this is because
		#	the function is defined outside of the dismo package
		brt.eval = evaluate(p=occur, a=bkgd, model=brt.obj, n.trees=brt.obj$gbm.call$best.trees) # evaluate model
		saveModelEvaluation(brt.eval) 	# save output
		rm(list=c("brt.obj", "brt.eval")) #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load brt.obj from", wd, "output_brt", sep=": "), stdout())
	}
}

if (evaluate.maxent) {
	# read in the Maxent predictions at the presence and background points, and 
	#	extract the columns we need
	model.dir <- paste(wd, "/output_maxent", sep="")
	presence <- read.csv(paste(model.dir, "/", sp, "_samplePredictions.csv", sep=""))
	background <- read.csv(paste(model.dir, "/", sp, "_backgroundPredictions.csv", sep=""))
	p <- presence$Logistic.prediction
	a <- background$logistic
	
	# use predictions to generate ModelEvaluation() per dismo package (copy code from dismo::evaluate.R)
	# evaluate() default thresholds are one per prediction, unless > 1000
	
	np <- length(p)
	na <- length(a)

		if (length(p) > 1000) {
			tr <- as.vector(quantile(p, 0:1000/1000))
		} else {
			tr <- p
		}
		if (length(a) > 1000) {
			tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
		} else {
			tr <- c(tr, a)
		}
		tr <- sort(unique( round(tr, 8)))
		tr <- c( tr - 0.0001, tr[length(tr)] + c(0, 0.0001))
		
		N <- na + np

	xc <- new('ModelEvaluation')
	xc@presence = p
	xc@absence = a
		
	R <- sum(rank(c(p, a))[1:np]) - (np*(np+1)/2)
	xc@auc <- R / (na * np)
	
	cr <- try( cor.test(c(p,a), c(rep(1, length(p)), rep(0, length(a))) ), silent=TRUE )
	if (class(cr) != 'try-error') {
		xc@cor <- cr$estimate
		xc@pcor <- cr$p.value
	}
	
	res <- matrix(ncol=4, nrow=length(tr))
	colnames(res) <- c('tp', 'fp', 'fn', 'tn')
	xc@t <- tr
	for (i in 1:length(tr)) {
		res[i,1] <- length(p[p>=tr[i]])  # a  true positives
		res[i,2] <- length(a[a>=tr[i]])  # b  false positives
		res[i,3] <- length(p[p<tr[i]])    # c  false negatives
		res[i,4] <- length(a[a<tr[i]])    # d  true negatives
	}
	xc@confusion = res
	a = res[,1]
	b = res[,2]
	c = res[,3]
	d = res[,4]
# after Fielding and Bell	
	xc@np <- as.integer(np)
	xc@na <- as.integer(na)
	xc@prevalence = (a + c) / N
	xc@ODP = (b + d) / N
	xc@CCR = (a + d) / N
	xc@TPR = a / (a + c)
	xc@TNR = d / (b + d)
	xc@FPR = b / (b + d)
	xc@FNR = c/(a + c)
	xc@PPP = a/(a + b)
	xc@NPP = d/(c + d)
	xc@MCR = (b + c)/N
	xc@OR = (a*d)/(c*b)

	prA = (a+d)/N
	prY = (a+b)/N * (a+c)/N
	prN = (c+d)/N * (b+d)/N
	prE = prY + prN
	xc@kappa = (prA - prE) / (1-prE)
	
	saveModelEvaluation(xc)	# save output
	rm(xc); #clean up the memory
}

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

saveBIOMODModelEvaluation = function(loaded.name, biomod.model) {
	# get and save the model evaluation statistics
	# EMG these must specified during model creation with the arg "models.eval.meth"
	evaluation = getModelsEvaluations(biomod.model)
	write.csv(evaluation, file=paste(getwd(), "/modelEvaluation.txt", sep=""))

	# save AUC curve
	# need predictions at the presence and background points and observed values
	predictions = getModelsPrediction(biomod.model); obs = attributes(getModelsInputData(biomod.model))[[3]];
	require(pROC, quietly=T)
    roc1 <- roc(as.numeric(obs), as.numeric(predictions), percent=T)
	png(file=paste(getwd(), "/roc.png", sep=''))
	plot(roc1, main=paste("AUC=",round(auc(roc1)/100,3),sep=""), legacy.axes=TRUE)
	dev.off()

	# COR not one of the methods provided automatically
	# COR <- try( cor.test(c(p,a), c(rep(1, length(p)), rep(0, length(a))) ), silent=TRUE )
	p = predictions[1:nrow(occur)]; a = predictions[nrow(occur)+1:nrow(predictions)]
	COR <- try( cor.test(c(p,a), c(rep(1, length(p)), rep(0, length(a))) ), silent=TRUE )

	elith = c(auc(roc1)/100, as.numeric(COR$estimate), evaluation[[1]], evaluation[[4]])
	write("AUC,COR,maxKAPPA,threshold@MaxKAPPA", file=paste(getwd(), "/elith.txt", sep=""))
	write(elith, file=paste(getwd(), "/elith.txt", sep=""), sep=",", append=TRUE)
	
	# get and save the variable importance estimates
	variableImpt = getModelsVarImport(biomod.model)
	if (!is.na(variableImpt)) {
		write.csv(variableImpt, file=paste(getwd(), "/variableImportance.txt", sep=""))
	} else {
		message("VarImport argument not specified during model creation!")
		#EMG must create the model with the arg "VarImport" != 0
	}

	# save response curves (Elith et al 2005)
	png(file=paste(getwd(), "/response_curves.png", sep=''))
		response.plot2(models = loaded.name, Data = getModelsInputData(biomod.model,"expl.var"),
			show.variables = getModelsInputData(biomod.model,"expl.var.names"),
			do.bivariate = FALSE, fixed.var.metric = "median", col = c("blue", "red"),
			legend = TRUE, data_species = getModelsInputData(biomod.model,"resp.var"))
	dev.off()
}

if (evaluate.glm) {
	glm.obj = getModelObject("glm") # get the model object
	if (!is.null(glm.obj)) {
		glm.loaded.model = BIOMOD_LoadModels(glm.obj, models="GLM")
		saveBIOMODModelEvaluation(glm.loaded.model, glm.obj) 	# save output
		rm("glm.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load glm.obj from", getwd(), sep=": "), stdout())
	}
}
		
if (evaluate.gam) {
	gam.obj = getModelObject("gam") # get the model object
	if (!is.null(gam.obj)) {
		gam.loaded.model = BIOMOD_LoadModels(gam.obj, models="GAM") # load model
		saveBIOMODModelEvaluation(gam.loaded.model, gam.obj) 	# save output
		rm("gam.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load gam.obj from", getwd(), sep=": "), stdout())
	}
}
	
if (evaluate.gbm) {
	gbm.obj = getModelObject("gbm") # get the model object
	if (!is.null(gbm.obj)) {
		gbm.loaded.model = BIOMOD_LoadModels(gbm.obj, models="GBM") # load model
		saveBIOMODModelEvaluation(gbm.loaded.model, gbm.obj) 	# save output
		rm("gbm.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load gbm.obj from", getwd(), sep=": "), stdout())
	}
}
	
if (evaluate.cta) {
	cta.obj = getModelObject("cta") # get the model object
	if (!is.null(cta.obj)) {
		cta.loaded.model = BIOMOD_LoadModels(cta.obj, models="CTA") # load model
		saveBIOMODModelEvaluation(cta.loaded.model, cta.obj) 	# save output
		rm("cta.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load cta.obj from", getwd(), sep=": "), stdout())
	}
}

if (evaluate.ann) {	
	ann.obj = getModelObject("ann") # get the model object
	if (!is.null(ann.obj)) {
		ann.loaded.model = BIOMOD_LoadModels(ann.obj, models="ANN") # load model
		saveBIOMODModelEvaluation(ann.loaded.model, ann.obj) 	# save output
		rm("ann.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load ann.obj from", getwd(), sep=": "), stdout())
	}
}

if (evaluate.sre) {	
	sre.obj = getModelObject("sre") # get the model object
	if (!is.null(sre.obj)) {
		sre.loaded.model = BIOMOD_LoadModels(sre.obj, models="SRE") # load model
		saveBIOMODModelEvaluation(sre.loaded.model, sre.obj) 	# save output
		rm("sre.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load sre.obj from", getwd(), sep=": "), stdout())
	}
}

if (evaluate.fda) {	
	fda.obj = getModelObject("fda") # get the model object
	if (!is.null(fda.obj)) {
		fda.loaded.model = BIOMOD_LoadModels(fda.obj, models="FDA") # load model
		saveBIOMODModelEvaluation(fda.loaded.model, fda.obj) 	# save output
		rm("fda.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load fda.obj from", getwd(), sep=": "), stdout())
	}
}

if (evaluate.mars) {	
	mars.obj = getModelObject("mars") # get the model object
	if (!is.null(mars.obj)) {
		mars.loaded.model = BIOMOD_LoadModels(mars.obj, models="MARS") # load model
		saveBIOMODModelEvaluation(mars.loaded.model, mars.obj) 	# save output
		rm("mars.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load mars.obj from", getwd(), sep=": "), stdout())
	}
}

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

if (evaluate.biomod.maxent) {	
	biomod.maxent.obj = getModelObject("biomod.maxent") # get the model object
	if (!is.null(biomod.maxent.obj)) {
		biomod.maxent.loaded.model = BIOMOD_LoadModels(biomod.maxent.obj, models="MAXENT") # load model
		saveBIOMODModelEvaluation(biomod.maxent.loaded.model, biomod.maxent.obj) 	# save output
		rm("biomod.maxent.obj") #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot load biomod.maxent.obj from", getwd(), sep=": "), stdout())
	}
}




