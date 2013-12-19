#helper functions for BCCVL model, project, and evaulate scripts

###########################
## 
## Predict
##
###########################

# function to get model object
getModelObject = function(model.name) {
	model.dir = paste(wd, "/output_", model.name, sep="")
	model.obj = get(load(file=paste(model.dir, "/model.object.RData", sep="")))	
}

# function to check that the environmental layers used to project the  model are the same as the ones used
# 	to create the model object 
checkModelLayers = function(model.obj) {

	message("Checking environmental layers used for projection...")
	# get the names of the environmental layers from the original model
	if (inherits(model.obj, "DistModel")) { # dismo package
		model.layers = colnames(model.obj@presence)
	} else if (inherits(model.obj, "gbm")) { # brt package
		model.layers = summary(model.obj)$var
		# EMG biomod2 packages has been updated
		#model.layers = model.obj$var.names
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

# function to save projection output as ascii gz
saveModelProjection = function(out.model, model.name) {
	model.dir = paste(wd, "/output_", model.name, sep="")
	outfilename = paste(model.dir, "/", es.name, "_", model.scale, "_", project.scale, sep="")
	writeRaster(out.model, outfilename, format="ascii", overwrite=TRUE)
	message(paste("Zipping ascii: ", outfilename, sep=""))
	system(paste("gzip ", outfilename, ".asc", sep=""))
}

###########################
## 
## Evaluate
##
###########################

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
					
			# resample from that variables' values, keeping other variable values the same, and 
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

# function to save model object, response curves, and variable importance
saveBIOMODModelEvaluation = function(loaded.name, biomod.model) {
	# get and save the model evaluation statistics
	# EMG these must specified during model creation with the arg "models.eval.meth"
	evaluation = getModelsEvaluations(biomod.model)
	write.csv(evaluation, file=paste(getwd(), "/biomod2.modelEvaluation.txt", sep=""))

	# get the model predictions and observed values
	predictions = getModelsPrediction(biomod.model); obs = getModelsInputData(biomod.model, "resp.var");

	# get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
	# source my modified version of biomod2's Evaluate.models.R for consistent model accuracy statistics
	source(paste(function.path, "/my.Evaluate.models.R", sep=""))
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