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

###read in the necessary observation and background data
#occur = read.csv(occur.data) #read in the observation data lon/lat
bkgd = read.csv(bkgd.data) #read in the background data lon/lat

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
	###not user modified section
	tstr = paste('java -mx2048m -jar ',maxent.jar,' ',sep='') #start the maxent string
	tstr = paste(tstr,'environmentallayers=', bkgd.data, " ",sep='') 
	tstr = paste(tstr,'environmentallayers=',outdir,'/bkgd.csv ',sep='')
	tstr = paste(tstr,'samplesfile=', occur.data, " ",sep='')
	tstr = paste(tstr,'outputdirectory=',outdir, " ", sep='')
	tstr = paste(tstr,'autorun=TRUE visible=FALSE warnings=FALSE tooltips=FALSE ',sep='')
	tstr = paste(tstr,'askoverwrite=FALSE skipifexists=FALSE prefixes=TRUE verbose=FALSE ',sep='')
	tstr = paste(tstr,'responsecurves=TRUE pictures=TRUE jackknife=TRUE writeclampgrid=TRUE ',sep='')
	tstr = paste(tstr,'writemess=TRUE writebackgroundpredictions=TRUE writeplotdata=TRUE outputgrids=TRUE ',sep='')
	tstr = paste(tstr,'plots=TRUE appendtoresultsfile=FALSE threads=1 adjustsampleradius=0 ',sep='')
	tstr = paste(tstr,'logfile=maxent.log cache=TRUE allowpartialdata=FALSE outputfiletype="asc" ',sep='')
	tstr = paste(tstr,'perspeciesresults=FALSE responsecurvesexponent=FALSE    ',sep='')
	if (any(enviro.data.type!='continuous')){
		catvals = which(enviro.data.type!='continuous')
		for (ii in catvals) {
			tstr = paste(tstr,'togglelayertype=',enviro.data.names[ii],' ',sep='') #toggle the layer type
		}
	}
	### based on user modified
	tstr = paste(tstr,'outputformat=',outputformat,' ',sep='')
	tstr = paste(tstr,'randomseed=',randomseed,' ',sep='')
	tstr = paste(tstr,'logscale=',logscale,' ',sep='')
	tstr = paste(tstr,'removeduplicates=',removeduplicates,' ',sep='')
	tstr = paste(tstr,'randomtestpoints=',randomtestpoints,' ',sep='')
	tstr = paste(tstr,'betamultiplier=',betamultiplier,' ',sep='')
	tstr = paste(tstr,'maximumbackground=',maximumbackground,' ',sep='')
	tstr = paste(tstr,'biasfile=',biasfile,' ',sep='')
	tstr = paste(tstr,'testsamplesfile=',testsamplesfile,' ',sep='')
	tstr = paste(tstr,'replicates=',replicates,' ',sep='')
	tstr = paste(tstr,'replicatetype=',replicatetype,' ',sep='')
	tstr = paste(tstr,'linear=',linear,' ',sep='')
	tstr = paste(tstr,'quadratic=',quadratic,' ',sep='')
	tstr = paste(tstr,'product=',product,' ',sep='')
	tstr = paste(tstr,'threshold=',threshold,' ',sep='')
	tstr = paste(tstr,'hinge=',hinge,' ',sep='')
	tstr = paste(tstr,'addsamplestobackground=',addsamplestobackground,' ',sep='')
	tstr = paste(tstr,'addallsamplestobackground=',addallsamplestobackground,' ',sep='')
	tstr = paste(tstr,'fadebyclamping=',fadebyclamping,' ',sep='')
	tstr = paste(tstr,'extrapolate=',extrapolate,' ',sep='')
	tstr = paste(tstr,'autofeature=',autofeature,' ',sep='')
	tstr = paste(tstr,'doclamp=',doclamp,' ',sep='')
	tstr = paste(tstr,'maximumiterations=',maximumiterations,' ',sep='')
	tstr = paste(tstr,'convergencethreshold=',convergencethreshold,' ',sep='')
	tstr = paste(tstr,'lq2lqptthreshold=',lq2lqptthreshold,' ',sep='')
	tstr = paste(tstr,'l2lqthreshold=',l2lqthreshold,' ',sep='')
	tstr = paste(tstr,'hingethreshold=',hingethreshold,' ',sep='')
	tstr = paste(tstr,'beta_threshold=',beta_threshold,' ',sep='')
	tstr = paste(tstr,'beta_categorical=',beta_categorical,' ',sep='')
	tstr = paste(tstr,'beta_lqp=',beta_lqp,' ',sep='')
	tstr = paste(tstr,'beta_hinge=',beta_hinge,' ',sep='')
	tstr = paste(tstr,'defaultprevalence=',defaultprevalence,' ',sep='')
	tstr = paste(tstr,'nodata=',nodata,' ',sep='')
	system(tstr)	
}