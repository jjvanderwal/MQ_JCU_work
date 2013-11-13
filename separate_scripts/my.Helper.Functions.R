#helper functions for BCCVL model, project, and evaulate scripts

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
		model.layers = summary(model.obj)$var
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
	writeRaster(out.model, paste(model.dir, "/", es.name, "_", model.scale, "_", project.scale, sep=""), format="ascii", 
		overwrite=TRUE)
	system(paste("gzip ", model.dir, "/", es.name, "_", model.scale, "_", project.scale, ".asc", sep=""))
}
