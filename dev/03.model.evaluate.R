#script to evaluate distribution models

#source("/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/dev/03.init.args.model.evaluate.R") #read in the initial arguments
source("/home/jc140298/bccvl/03.init.args.model.evaluate.R")

### check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

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

	model.dir = paste(spwddir, "output_", model.name, "/", sep="")
	model.obj = NULL
	tryCatch({
		model.obj = get(load(file=paste(model.dir, "model.object.RData", sep="")))
	}, error = function(e) {
		print(paste("FAIL!", sp, "Cannot load model.obj from", model.dir, sep=": "))
	}, finally = {
		return(model.obj)
	})
	# EMG This is ugly, try to make it pretty
}

# function to save evaluate output
saveModelEvaluation = function(out.model, model.name) {
	
	model.dir = paste(spwddir, "/output_", model.name, "/", sep="")	# set the output directory for eval object
	save(out.model, file=paste(model.dir, "eval.object.RData", sep=''))	# save the 'ModelEvalution' object
	
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

# for each species
for (sp in species) {

# try to load in the data
spwddir = paste(wd, sp, "/", sep="")
tryCatch({
	load(paste(spwddir, "occur.RData", sep="")) 
	load(paste(spwddir, "bkgd.RData", sep=""))
}, error = function(e) {
	print(paste("FAIL!", sp, "missing data needed to evaluate models", sep=": "))
}) # end tryCatch	

if (evaluate.bioclim) {

	bioclim.obj = getModelObject("bioclim")	# get the model object
	tryCatch ({
		bioclim.eval = evaluate(p=occur, a=bkgd, model=bioclim.obj)	# evaluate model
		saveModelEvaluation(bioclim.eval, "bioclim")	# save output
		rm("bioclim.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.bioclim", sep=": "))
	}, finally = {
		rm("bioclim.obj") #clean up the memory
	}) # end tryCatch
}
	
if (evaluate.domain) {

	domain.obj = getModelObject("domain") # get the model object
	tryCatch ({
		domain.eval = evaluate(p=occur, a=bkgd, model=domain.obj) # evaluate model
		saveModelEvaluation(domain.eval, "domain") 	# save output
		rm("domain.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.domain", sep=": "))
	}, finally = {
		rm("domain.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.mahal) {
	
	mahal.obj = getModelObject("mahal") # get the model object
	tryCatch ({
		mahal.eval = evaluate(p=occur, a=bkgd, model=mahal.obj) # evaluate model
		saveModelEvaluation(mahal.eval, "mahal") 	# save output
		rm("mahal.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.mahal", sep=": "))
	}, finally = {
		rm("mahal.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.geodist) {
	
	geodist.obj = getModelObject("geodist") # get the model object
	tryCatch ({
		geodist.eval = evaluate(model=geodist.obj, p=occur, a=bkgd) # evaluate model
		#EMG NOTE: no error for p,a if columns not specified c.f. convHull
		saveModelEvaluation(geodist.eval, "geodist") 	# save output
		rm("geodist.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.geodist", sep=": "))
	}, finally = {
		rm("geodist.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.convHull) {
	
	convHull.obj = getModelObject("convHull") # get the model object
	tryCatch ({
		convHull.eval = evaluate(model=convHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(convHull.eval, "convHull") 	# save output
		rm("convHull.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.convHull", sep=": "))
	}, finally = {
		rm("convHull.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.convHull) {
	
	circles.obj = getModelObject("circles") # get the model object
	tryCatch ({
		circles.eval = evaluate(model=circles.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(circles.eval, "circles") 	# save output
		rm("circles.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.convHull", sep=": "))
	}, finally = {
		rm("convHull.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.geoIDW) {
	
	geoIDW.obj = getModelObject("geoIDW") # get the model object
	tryCatch ({
		geoIDW.eval = evaluate(model=geoIDW.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(geoIDW.eval, "geoIDW") 	# save output
		rm("geoIDW.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.geoIDW", sep=": "))
	}, finally = {
		rm("geoIDW.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.voronoiHull) {
	
	voronoiHull.obj = getModelObject("voronoiHull") # get the model object
	tryCatch ({
		voronoiHull.eval = evaluate(model=voronoiHull.obj, p=occur[c("lon","lat")], a=bkgd[c("lon","lat")]) # evaluate model
		saveModelEvaluation(voronoiHull.eval, "voronoiHull") 	# save output
		rm("voronoiHull.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.voronoiHull", sep=": "))
	}, finally = {
		rm("voronoiHull.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.brt) {
	
	brt.obj = getModelObject("brt") # get the model object
	tryCatch ({
		# NOTE the order of arguments in the  predict function for brt; this is because
		#	the function is defined outside of the dismo package
		brt.eval = evaluate(p=occur, a=bkgd, model=brt.obj, n.trees=brt.obj$gbm.call$best.trees) # evaluate model
		saveModelEvaluation(brt.eval, "brt") 	# save output
		rm(	"brt.eval"); #clean up the memory
	}, error = function(e) {
		print(paste("FAIL!", sp, "evaluate.brt", sep=": "))
	}, finally = {
		rm("brt.obj") #clean up the memory
	}) # end tryCatch
}

if (evaluate.maxent) {
	# read in the Maxent predictions at the presence and background points, and 
	#	extract the columns we need
	model.dir <- paste(spwddir, "/output_maxent", sep="")
	presence <- read.csv(paste(model.dir, "/", sp, "_samplePredictions.csv", sep=""))
	background <- read.csv(paste(model.dir, "/", sp, "_backgroundPredictions.csv", sep=""))
	p <- presence$Logistic.prediction
	a <- background$logistic
	
	# use predictions to generate ModelEvaluation() per dismo package (copy code from evaluate.R)
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
	
	saveModelEvaluation(xc, "maxent")	# save output
	rm(xc); #clean up the memory
}

} # end for species