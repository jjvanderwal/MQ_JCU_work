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
necessary=c("dismo","SDMTools", "rJava") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###read in the necessary observation, background and environmental data
#setwd(wd) #set the working directory
populate.data = FALSE #variable to define if there is a need to generate occur & background environmental info
if (file.exists(paste(wd, "/occur.RData", sep="")) && file.exists(paste(wd, "/bkgd.RData", sep=""))) {
	load(paste(wd, "/occur.RData", sep="")); load(paste(wd, "/bkgd.RData", sep="")); #if files already exist, load in the data
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
	save(occur,file=paste(wd, "/occur.RData", sep="")); save(bkgd,file=paste(wd, "/bkgd.RData", sep="")) #write out the raw data for analysis
}

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

###run the models and store models
#############################################################################################
#
# MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
#
#############################################################################################
###############
#
# MAXENT
#
###############
# maxent is being run as a system call with the same data. Arguments include:
# table below is "Flag -- Type -- Default -- Description"
# responsecurves -- boolean -- FALSE -- Create graphs showing how predicted relative probability of occurrence depends on the value of each environmental variable
# pictures -- boolean -- TRUE -- Create a .png image for each output grid
# jackknife -- boolean -- FALSE -- Measure importance of each environmental variable by training with each environmental variable first omitted, then used in isolation
# outputformat -- string -- logistic -- Representation of probabilities used in writing output grids. See Help for details
# outputfiletype -- string -- asc -- File format used for writing output grids
# outputdirectory -- directory --  -- Directory where outputs will be written. This should be different from the environmental layers directory.
# projectionlayers -- file/directory --  -- Location of an alternate set of environmental variables. Maxent models will be projected onto these variables.
# --  --  -- Can be a .csv file (in SWD format) or a directory containing one file per variable.
# --  --  -- Multiple projection files/directories can be separated by commas.
# samplesfile -- file --  -- Please enter the name of a file containing presence locations for one or more species.
# environmentallayers -- file/directory --  -- Environmental variables can be in a directory containing one file per variable,
# --  --  -- or all together in a .csv file in SWD format. Please enter a directory name or file name.
# randomseed -- boolean -- FALSE -- If selected, a different random seed will be used for each run, so a different random test/train partition
# --  --  -- will be made and a different random subset of the background will be used, if applicable.
# logscale -- boolean -- TRUE -- If selected, all pictures of models will use a logarithmic scale for color-coding.
# warnings -- boolean -- TRUE -- Pop up windows to warn about potential problems with input data.
# --  --  -- Regardless of this setting, warnings are always printed to the log file.
# tooltips -- boolean -- TRUE -- Show messages that explain various parts of the interface, like this message
# askoverwrite -- boolean -- TRUE -- If output files already exist for a species being modeled,
# --  --  -- pop up a window asking whether to overwrite or skip. Default is to overwrite.
# skipifexists -- boolean -- FALSE -- If output files already exist for a species being modeled,
# --  --  -- skip the species without remaking the model.
# removeduplicates -- boolean -- TRUE -- Remove duplicate presence records.
# --  --  -- If environmental data are in grids, duplicates are records in the same grid cell.
# --  --  -- Otherwise, duplicates are records with identical coordinates.
# writeclampgrid -- boolean -- TRUE -- Write a grid that shows the spatial distribution of clamping.
# --  --  -- At each point, the value is the absolute difference between prediction values with and without clamping.
# writemess -- boolean -- TRUE -- A multidimensional environmental similarity surface (MESS) shows where novel climate conditions exist in the projection layers.
# --  --  -- The analysis shows both the degree of novelness and the variable that is most out of range at each point.
# randomtestpoints -- integer -- 0 -- Percentage of presence localities to be randomly set aside as test points, used to compute AUC, omission etc.
# betamultiplier -- double -- 1 -- Multiply all automatic regularization parameters by this number. A higher number gives a more spread-out distribution.
# maximumbackground -- integer -- 10000 -- If the number of background points / grid cells is larger than this number, then this number of cells is chosen randomly for background points
# biasfile -- file --  -- Sampling is assumed to be biased according to the sampling distribution given in this grid file.
# --  --  -- Values in this file must not be zero or negative. MaxEnt will factor out the bias.
# --  --  -- Requires environmental data to be in grids, rather than a SWD format file
# testsamplesfile -- file --  -- Use the presence localities in this file to compute statistics (AUC, omission etc.)
# --  --  -- The file can contain different localities for different species.
# --  --  -- It takes precedence over the random test percentage.
# replicates -- integer -- 1 -- Number of replicate runs to do when cross-validating, bootstrapping or doing sampling with replacement runs
# replicatetype -- string -- crossvalidate -- If replicates > 1, do multiple runs of this type:
# --  --  -- Crossvalidate: samples divided into replicates folds; each fold in turn used for test data.
# --  --  -- Bootstrap: replicate sample sets chosen by sampling with replacement.
# --  --  -- Subsample: replicate sample sets chosen by removing random test percentage without replacement to be used for evaluation.
# perspeciesresults -- boolean -- FALSE -- Write separate maxentResults file for each species
# writebackgroundpredictions -- boolean -- FALSE -- Write .csv file with predictions at background points
# responsecurvesexponent -- boolean -- FALSE -- Instead of showing the logistic value for the y axis in response curves, show the exponent (a linear combination of features)
# linear -- boolean -- TRUE -- Allow linear features to be used
# quadratic -- boolean -- TRUE -- Allow quadratic features to be used
# product -- boolean -- TRUE -- Allow product features to be used
# threshold -- boolean -- TRUE -- Allow threshold features to be used
# hinge -- boolean -- TRUE -- Allow hinge features to be used
# addsamplestobackground -- boolean -- TRUE -- Add to the background any sample for which has a combination of environmental values that isn't already present in the background
# addallsamplestobackground -- boolean -- FALSE -- Add all samples to the background, even if they have combinations of environmental values that are already present in the background
# autorun -- boolean -- FALSE -- Start running as soon as the the program starts up
# writeplotdata -- boolean -- FALSE -- Write output files containing the data used to make response curves, for import into external plotting software
# fadebyclamping -- boolean -- FALSE -- Reduce prediction at each point in projections by the difference between
# --  --  -- clamped and non-clamped output at that point
# extrapolate -- boolean -- TRUE -- Predict to regions of environmental space outside the limits encountered during training
# visible -- boolean -- TRUE -- Make the Maxent user interface visible
# autofeature -- boolean -- TRUE -- Automatically select which feature classes to use, based on number of training samples
# doclamp -- boolean -- TRUE -- Apply clamping when projecting
# outputgrids -- boolean -- TRUE -- Write output grids. Turning this off when doing replicate runs causes only the summary grids (average, std deviation etc.) to be written, not those for the individual runs.
# plots -- boolean -- TRUE -- Write various plots for inclusion in .html output
# appendtoresultsfile -- boolean -- FALSE -- If false, maxentResults.csv file is reinitialized before each run
# maximumiterations -- integer -- 500 -- Stop training after this many iterations of the optimization algorithm
# convergencethreshold -- double -- 1.00E-05 -- Stop training when the drop in log loss per iteration drops below this number
# adjustsampleradius -- integer -- 0 -- Add this number of pixels to the radius of white/purple dots for samples on pictures of predictions.
# --  --  -- Negative values reduce size of dots.
# threads -- integer -- 1 -- Number of processor threads to use. Matching this number to the number of cores on your computer speeds up some operations, especially variable jackknifing.
# lq2lqptthreshold -- integer -- 80 -- Number of samples at which product and threshold features start being used
# l2lqthreshold -- integer -- 10 -- Number of samples at which quadratic features start being used
# hingethreshold -- integer -- 15 -- Number of samples at which hinge features start being used
# beta_threshold -- double -- -1 -- Regularization parameter to be applied to all threshold features; negative value enables automatic setting
# beta_categorical -- double -- -1 -- Regularization parameter to be applied to all categorical features; negative value enables automatic setting
# beta_lqp -- double -- -1 -- Regularization parameter to be applied to all linear, quadratic and product features; negative value enables automatic setting
# beta_hinge -- double -- -1 -- Regularization parameter to be applied to all hinge features; negative value enables automatic setting
# logfile -- string -- maxent.log -- File name to be used for writing debugging information about a run in output directory
# cache -- boolean -- TRUE -- Make a .mxe cached version of ascii files, for faster access
# defaultprevalence -- double -- 0.5 -- Default prevalence of the species: probability of presence at ordinary occurrence points.
# --  --  -- See Elith et al., Diversity and Distributions, 2011 for details.
# applythresholdrule -- string --  -- Apply a threshold rule, generating a binary output grid in addition to the regular prediction grid. Use the full name of the threshold rule in Maxent's html output as the argument. For example, 'applyThresholdRule=Fixed cumulative value 1'.
# togglelayertype -- string --  -- Toggle continuous/categorical for environmental layers whose names begin with this prefix (default: all continuous)
# togglespeciesselected -- string --  -- Toggle selection of species whose names begin with this prefix (default: all selected)
# togglelayerselected -- string --  -- Toggle selection of environmental layers whose names begin with this prefix (default: all selected)
# verbose -- boolean -- FALSE -- Gived detailed diagnostics for debugging
# allowpartialdata -- boolean -- FALSE -- During model training, allow use of samples that have nodata values for one or more environmental variables.
# prefixes -- boolean -- TRUE -- When toggling samples or layers selected or layer types, allow toggle string to be a prefix rather than an exact match.
# nodata -- integer -- -9999 -- Value to be interpreted as nodata values in SWD sample data

if (model.maxent) {
	outdir = paste(wd,'/output_maxent',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	# create RasterStack containing grids with predictor variables used to extract values from for the point locations
	predictors = stack(enviro.data)
	me = tryCatch(maxent(x=predictors, p=occur[,c("lon","lat")], a=bkgd[,c("lon","lat")], 
		factors=enviro.data.names[which(enviro.data.type == "categorical")], 
		args=c("autorun=TRUE", "visible=FALSE", "warnings=FALSE", "tooltips=FALSE", "askoverwrite=FALSE", "skipifexists=FALSE",
		"prefixes=TRUE", "verbose=FALSE", "responsecurves=TRUE", "pictures=TRUE", "jackknife=TRUE", "writeclampgrid=TRUE",
		"writemess=TRUE", "writebackgroundpredictions=TRUE", "writeplotdata=TRUE", "outputgrids=TRUE", "plots=TRUE",
		"appendtoresultsfile=FALSE", "threads=1", "adjustsampleradius=0", "logfile=\"maxent.log\"", "cache=TRUE", 
		"allowpartialdata=FALSE", "perspeciesresults=FALSE", "responsecurvesexponent=FALSE",
#, "outputfiletype=\"asc\""		
		### based on user modified
#		paste('outputformat=',outputformat,sep=''),
		paste('randomseed=',randomseed,sep=''),
		paste('logscale=',logscale,sep=''),
		paste('removeduplicates=',removeduplicates,sep=''),
		paste('randomtestpoints=',randomtestpoints,sep=''),
		paste('betamultiplier=',betamultiplier,sep=''),
		paste('maximumbackground=',maximumbackground,sep=''),
		paste('biasfile=',biasfile,sep=''),
		paste('testsamplesfile=',testsamplesfile,sep=''),
		paste('replicates=',replicates,sep=''),
		paste('replicatetype=',replicatetype,sep=''),
		paste('linear=',linear,sep=''),
		paste('quadratic=',quadratic,sep=''),
		paste('product=',product,sep=''),
		paste('threshold=',threshold,sep=''),
		paste('hinge=',hinge,sep=''),
		paste('addsamplestobackground=',addsamplestobackground,sep=''),
		paste('addallsamplestobackground=',addallsamplestobackground,sep=''),
		paste('fadebyclamping=',fadebyclamping,sep=''),
		paste('extrapolate=',extrapolate,sep=''),
		paste('autofeature=',autofeature,sep=''),
		paste('doclamp=',doclamp,sep=''),
		paste('maximumiterations=',maximumiterations,sep=''),
		paste('convergencethreshold=',convergencethreshold,sep=''),
		paste('lq2lqptthreshold=',lq2lqptthreshold,sep=''),
		paste('l2lqthreshold=',l2lqthreshold,sep=''),
		paste('hingethreshold=',hingethreshold,sep=''),
		paste('beta_threshold=',beta_threshold,sep=''),
		paste('beta_categorical=',beta_categorical,sep=''),
		paste('beta_lqp=',beta_lqp,sep=''),
		paste('beta_hinge=',beta_hinge,sep=''),
		paste('defaultprevalence=',defaultprevalence,sep=''),
		paste('nodata=',nodata,sep=''))))
	if (!is.null(me)) {	
		save(me,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
		rm(me); #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot create maxent model object", sep=": "), stdout())
	}
}
