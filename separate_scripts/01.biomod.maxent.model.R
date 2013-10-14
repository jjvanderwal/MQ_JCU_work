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
load(paste(wd, "01.init.args.model.", species, ".RData", sep=""))

###check if libraries are installed, install if necessary and then load them
necessary=c("SDMTools","biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###read in the necessary observation, background and environmental data
#setwd(wd) #set the working directory
populate.data = FALSE #variable to define if there is a need to generate occur & background environmental info
if (file.exists(paste(wd, "occur.RData", sep="")) && file.exists(paste(wd, "bkgd.RData", sep=""))) {
	load(paste(wd, "occur.RData", sep="")); load(paste(wd, "bkgd.RData", sep="")); #if files already exist, load in the data
	if (!all(colnames(occur)==c('lon','lat',enviro.data.names))) { populate.data=TRUE } #not the right data, we need to repopulate it
} else { populate.data=TRUE } # data does not exist, we need to generate it
if (populate.data) {
	occur = read.csv(occur.data) #read in the observation data lon/lat
	bkgd = read.csv(bkgd.data) #read in teh background position data lon.lat
	for (ii in 1:length(enviro.data)) { cat(ii,'of',length(enviro.data),'\n') #cycle through each of the environmental datasets and append the data
		tasc = read.asc(enviro.data[ii]) #read in the envirodata
		occur[,enviro.data.names[ii]] = extract.data(cbind(occur$lon,occur$lat),tasc) #extract envirodata for observations
		bkgd[,enviro.data.names[ii]] = extract.data(cbind(bkgd$lon,bkgd$lat),tasc) #extract envirodata for background data
	}
	save(occur,file=paste(wd, "occur.RData", sep="")); save(bkgd,file=paste(wd, "bkgd.RData", sep="")) #write out the raw data for analysis
}

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

###run the models and store models
############### BIOMOD2 Models ###############
# 1. Format the data
# 2. Define the model options
# 3. Compute the model
# NOTE: Model evaluation is included as part of model creation

# BIOMOD_FormatingData(resp.var, expl.var, resp.xy = NULL, resp.name = NULL, eval.resp.var = NULL, 
#	eval.expl.var = NULL, eval.resp.xy = NULL, PA.nb.rep = 0, PA.nb.absences = 1000, PA.strategy = 'random',
#	PA.dist.min = 0, PA.dist.max = NULL, PA.sre.quant = 0.025, PA.table = NULL, na.rm = TRUE)
#
# resp.var a vector, SpatialPointsDataFrame (or SpatialPoints if you work with ‘only presences’ data) containing species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to build the species distribution models.
# expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to build your models.
# resp.xy optional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to build your models.
# eval.resp.var	a vector, SpatialPointsDataFrame your species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to evaluate the models with independant data (or past data for instance).
# eval.expl.var	a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to evaluate the models with independant data (or past data for instance).
# eval.resp.xy opional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to evaluate the modelswith independant data (or past data for instance).
# resp.name	response variable name (character). The species name.
# PA.nb.rep	number of required Pseudo Absences selection (if needed). 0 by Default.
# PA.nb.absences number of pseudo-absence selected for each repetition (when PA.nb.rep > 0) of the selection (true absences included)
# PA.strategy strategy for selecting the Pseudo Absences (must be ‘random’, ‘sre’, ‘disk’ or ‘user.defined’)
# PA.dist.min minimal distance to presences for ‘disk’ Pseudo Absences selection (in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.dist.max maximal distance to presences for ‘disk’ Pseudo Absences selection(in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.sre.quant quantile used for ‘sre’ Pseudo Absences selection
# PA.table a matrix (or a data.frame) having as many rows than resp.var values. Each column correspund to a Pseudo-absences selection. It contains TRUE or FALSE indicating which values of resp.var will be considered to build models. It must be used with ‘user.defined’ PA.strategy.
# na.rm	logical, if TRUE, all points having one or several missing value for environmental data will be removed from analysis

# format the data as required by the biomod package
formatBiomodData = function() {
	biomod.data = rbind(occur[,c("lon","lat")],bkgd[,c("lon","lat")])
	biomod.data.pa = c(rep(1,nrow(occur)),rep(0,nrow(bkgd)))
	myBiomodData <- BIOMOD_FormatingData(resp.var = biomod.data.pa, expl.var = stack(enviro.data),	
		resp.xy = biomod.data, resp.name = species)
	return(myBiomodData)
}

	
# BIOMOD_Modeling(data, models = c('GLM','GBM','GAM','CTA','ANN','SRE','FDA','MARS','RF','MAXENT'), models.options = NULL, 
#	NbRunEval=1, DataSplit=100, Yweights=NULL, Prevalence=NULL, VarImport=0, models.eval.meth = c('KAPPA','TSS','ROC'), 
#	SaveObj = TRUE, rescal.all.models = TRUE, do.full.models = TRUE, modeling.id = as.character(format(Sys.time(), '%s')),
#	...)
#
# data	BIOMOD.formated.data object returned by BIOMOD_FormatingData
# models vector of models names choosen among 'GLM', 'GBM', 'GAM', 'CTA', 'ANN', 'SRE', 'FDA', 'MARS', 'RF' and 'MAXENT'
# models.options BIOMOD.models.options object returned by BIOMOD_ModelingOptions
# NbRunEval	Number of Evaluation run
# DataSplit	% of data used to calibrate the models, the remaining part will be used for testing
# Yweights response points weights
# Prevalence either NULL (default) or a 0-1 numeric used to build 'weighted response weights'
# VarImport	Number of permutation to estimate variable importance
# models.eval.meth vector of names of evaluation metric among 'KAPPA', 'TSS', 'ROC', 'FAR', 'SR', 'ACCURACY', 'BIAS', 'POD', 'CSI' and 'ETS'
# SaveObj keep all results and outputs on hard drive or not (NOTE: strongly recommended)
# rescal.all.models	if true, all model prediction will be scaled with a binomial GLM
# do.full.models if true, models calibrated and evaluated with the whole dataset are done
# modeling.id character, the ID (=name) of modeling procedure. A random number by default.
# ... further arguments :
# DataSplitTable : a matrix, data.frame or a 3D array filled with TRUE/FALSE to specify which part of data must be used for models calibration (TRUE) and for models validation (FALSE). Each column correspund to a 'RUN'. If filled, args NbRunEval, DataSplit and do.full.models will be ignored.

###############
#
# MAXENT - maxent
#
###############

# myBiomodOptions <- BIOMOD_ModelingOptions(MAXENT = list(path_to_maxent.jar = 'C:/userdata', maximumiterations = 200, 
#	visible = FALSE, linear = TRUE, quadratic = TRUE, product = TRUE, threshold = TRUE, hinge = TRUE, 
#	lq2lqptthreshold = 80, l2lqthreshold = 10, hingethreshold = 15, beta_threshold = -1, beta_categorical = -1, 
#	beta_lqp = -1, beta_hinge = -1, defaultprevalence = 0.5))
# path_to_maxent.jar : character, the link to maxent.jar file (the working directory by default)
# maximumiterations : integer (default 200), maximum iteration done
# visible : logical (default FALSE), make the Maxent user interface visible
# linear : logical (default TRUE), allow linear features to be used
# quadratic : logical (default TRUE), allow quadratic features to be used
# product : logical (default TRUE), allow product features to be used
# threshold : logical (default TRUE), allow threshold features to be used
# hinge : logical (default TRUE), allow hinge features to be used
# lq2lqptthreshold : integer (default 80), number of samples at which product and threshold features start being used
# l2lqthreshold : integer (default 10), number of samples at which quadratic features start being used
# hingethreshold : integer (default 15), number of samples at which hinge features start being used
# beta_threshold : numeric (default -1.0), regularization parameter to be applied to all threshold features; negative value enables automatic setting
# beta_categorical : numeric (default -1.0), regularization parameter to be applied to all categorical features; negative value enables automatic setting
# beta_lqp : numeric (default -1.0), regularization parameter to be applied to all linear, quadratic and product features; negative value enables automatic setting
# beta_hinge : numeric (default -1.0), regularization parameter to be applied to all hinge features; negative value enables automatic setting
# defaultprevalence : numeric (default 0.5), default prevalence of the species: probability of presence at ordinary occurrence points

if (model.biomod.maxent) {
	outdir = paste(wd,'output_biomod.maxent/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	setwd(outdir) # set the working directory (where model results will be stored)
	myBiomodData = formatBiomodData() # 1. Format the data
	myBiomodOptions <- BIOMOD_ModelingOptions(MAXENT = biomod.maxent.BiomodOptions) # 2. Define the model options
	# 3. Compute the model
	myBiomodModelOut.biomod.maxent <- BIOMOD_Modeling(data = myBiomodData, models = c('MAXENT'),	models.options = myBiomodOptions,
		NbRunEval=biomod.NbRunEval,	DataSplit=biomod.DataSplit,	Yweights=biomod.Yweights, Prevalence=biomod.Prevalence,
		VarImport=biomod.VarImport,	models.eval.meth = biomod.models.eval.meth, SaveObj = TRUE,
		rescal.all.models = biomod.rescal.all.models, do.full.models = biomod.do.full.models, 
		modeling.id = biomod.modeling.id)
	if (!is.null(myBiomodModelOut.biomod.maxent)) {		
			save(myBiomodModelOut.biomod.maxent, file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	}
}
