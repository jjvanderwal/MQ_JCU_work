#script to run to develop distribution models

#source("/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/dev/01.init.args.model.current.R") #read in the initial arguments
source("/home/jc140298/bccvl/01.init.args.model.current.R")

###check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools","gbm","gstat","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

# get a list of species names from the data directory
species =  list.files(datadir, full.names=FALSE)

for (sp in species) {

###read in the necessary observation, background and environmental data
spdatadir = paste(datadir, sp, "/", sep="")
spwddir = paste(wd, sp, "/", sep=""); dir.create(spwddir,recursive=TRUE); #create the output directory
#setwd(wd) #set the working directory

populate.data = TRUE #variable to define if there is a need to generate occur & background environmental info
if (file.exists(paste(datadir, sp, "/occur.RData", sep="")) && file.exists(paste(datadir, sp, "/bkgd.RData"))) {
	load(paste(datadir, sp, "/occur.RData", sep="")); load(paste(datadir, sp, "/bkgd.RData")); #if files already exist, load in the data
	if (!all(colnames(occur)==c('lon','lat',enviro.data.names))) { populate.data=TRUE } #not data we need to repopulate it
}
if (populate.data) {
	occur = read.csv(paste(spdatadir, occur.data.name, sep="")) #read in the observation data lon/lat
	bkgd = read.csv(paste(spdatadir, bkgd.data.name, sep="")) #read in the background position data lon/lat
	for (ii in 1:length(enviro.data)) { cat(ii,'of',length(enviro.data),'\n') #cycle through each of the environmental datasets and append the data
		tasc = read.asc(enviro.data[ii]) #read in the envirodata
		occur[,enviro.data.names[ii]] = extract.data(cbind(occur$lon,occur$lat),tasc) #extract envirodata for observations
		bkgd[,enviro.data.names[ii]] = extract.data(cbind(bkgd$lon,bkgd$lat),tasc) #extract envirodata for background data
	}
	save(occur,file=paste(spwddir, "occur.RData", sep="")); save(bkgd,file=paste(spwddir, "bkgd.RData", sep="")) #write out the raw data for analysis
}
###run the models and store models

#################################################################################
#
# PROFILE METHODS - only consider presence points: Bioclim, Domain, and Mahal
#
#################################################################################

###############
#
# BIOCLIM
#
###############

# bioclim(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

if (model.bioclim) {
	if (!all(enviro.data.type=="continuous")) {
		warning("bioclim not run because categorical data cannot be used")
	} else {
		outdir = paste(spwddir,'/output_bioclim/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
		bc = bioclim(x=occur[,enviro.data.names]) #run bioclim with matrix of enviro data
		save(bc,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
		rm(bc); #clean up memory
	}
}

###############
#
# DOMAIN
#
###############

# domain(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

if (model.domain) {
	if (!all(enviro.data.type=="continuous")) {
		warning("domain not run because categorical data cannot be used")
	} else {
		outdir = paste(spwddir,'output_domain/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
		dm = domain(x=occur[,enviro.data.names]) #run domain with matrix of enviro data
		save(dm,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
		rm(dm); #clean up memory
	}
}

###############
#
# MAHALANOBIS
#
###############

# mahal(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

if (model.mahal) {
	if (!all(enviro.data.type=="continuous")) {
		warning("Mahal not run because categorical data cannot be used")
	} else {
		outdir = paste(spwddir,'output_mahal/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
		mm = mahal(x=occur[,enviro.data.names]) #run mahal with matrix of enviro data
		save(mm,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
		rm(mm); #clean up memory
	}
}

#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################

############### Spatial-only models for presence data ###############

###############
#
# GEOGRAPHIC DISTANCE
#
###############

# geoDist(p, ...)
# p point locations (presence); two column matrix, data.frame or SpatialPoints* object
# ... you must supply a lonlat= argument(logical), unless p is a SpatialPoints* object and has a
#	valid CRS
# ... you can also supply an additional argument 'a' for absence points (currently ignored.); 
#	argument 'a' should be of the same class as argument 'p'

if (model.geodist) {
	outdir = paste(spwddir,'output_geodist/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	gd = geoDist(p=occur[,c('lon','lat')], lonlat=TRUE) #run geodist 
	save(gd,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(gd); #clean up memory
}

###############
#
# CONVEX HULLS
#
###############

# convHull(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# ... you can supply an argument n (>=1) to get n convex hulls around subset of the points
# ... you can also set n=1:x, to get a set of overlapping polygons consisting of 1 to x parts; i.e.
#	the first polygon has 1 part, the second has 2 parts and x has x parts

if (model.convHull) {
	outdir = paste(spwddir,'output_convHull/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	ch = convHull(p=occur[,c('lon','lat')]) #run convex hull 
	save(ch,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(ch); #clean up memory
}

###############
#
# CIRCLES
#
###############

# circles(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# d the radius of each circle in meters; a single number or a vector with elements corresponding to
#	rows in 'p'; if missing the diameter is computed from the inter-point distance
# n how many vertices in the circle? default is 360
# lonlat are these longitude/latitude data? default value is false
# r radius of the earth; only relevant for longitude/latitude data; default is 6378137 m

if (model.circles) {
	outdir = paste(spwddir,'output_circles/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	cc = circles(p=occur[,c('lon','lat')], lonlat=TRUE) #run circles 
	save(cc,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(cc); #clean up memory
}

############### Spatial-only models for presence/background (or absence) data ###############

###############
#
# GEOIDW - inverse distance weighted interpolation
#
###############

# geoIDS(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'
# ... none implemented

if (model.geoIDW) {
	outdir = paste(spwddir,'output_geoIDW/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	gidw = geoIDW(p=occur[,c('lon','lat')], a=bkgd[,c('lon','lat')]) #run the algorithm
	save(gidw,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(gidw); #clean up the memory
}

###############
#
# VORONOIHULL
#
###############

# voronoiHull(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'

if (model.voronoiHull) {
	outdir = paste(spwddir,'output_voronoiHull/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	vh = voronoiHull(p=occur[,c('lon','lat')], a=bkgd[,c('lon','lat')]) #run the algorithm
	save(vh,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(vh); #clean up the memory
}

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
	outdir = paste(spwddir,'output_brt/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	brt.data = rbind(occur,bkgd); brt.data$pa = c(rep(1,nrow(occur)),rep(0,nrow(bkgd))) #setup the data as needed
	brt = gbm.step(data=brt.data, gbm.x=which(names(brt.data) %in% enviro.data.names), 
		gbm.y=which(names(brt.data)=='pa'), 
		fold.vector = brt.fold.vector, 
		tree.complexity = brt.tree.complexity, 
		learning.rate = brt.learning.rate, 
		bag.fraction = brt.bag.fraction, 
		#site.weights = brt.site.weights, 
		#var.monotone = brt.var.monotone, 
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
	save(brt,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
	rm(brt); #clean up the memory
}

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
	outdir = paste(spwddir,'output_maxent/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
	write.csv(data.frame(species=sp,occur),paste(outdir,"occur.csv",sep=''),row.names=FALSE)### create occur.csv for maxent
	write.csv(data.frame(species="bkgd",bkgd),paste(outdir,"bkgd.csv",sep=''),row.names=FALSE)### create bkgd.csv for maxent
	###not user modified section
	tstr = paste('java -mx2048m -jar ',maxent.jar,' ',sep='') #start the maxent string
	tstr = paste(tstr,'environmentallayers=',outdir,'bkgd.csv ',sep='')
	tstr = paste(tstr,'samplesfile=',outdir,'occur.csv ',sep='')
	tstr = paste(tstr,'outputdirectory=',outdir,' ',sep='')
	tstr = paste(tstr,'autorun=TRUE visible=FALSE warnings=FALSE tooltips=FALSE ',sep='')
	tstr = paste(tstr,'askoverwrite=FALSE skipifexists=FALSE prefixes=TRUE verbose=FALSE ',sep='')
	tstr = paste(tstr,'responsecurves=TRUE pictures=TRUE jackknife=TRUE writeclampgrid=TRUE ',sep='')
	tstr = paste(tstr,'writemess=TRUE writebackgroundpredictions=TRUE writeplotdata=TRUE outputgrids=TRUE ',sep='')
	tstr = paste(tstr,'plots=TRUE appendtoresultsfile=FALSE threads=1 adjustsampleradius=0 ',sep='')
	tstr = paste(tstr,'logfile=maxent.log cache=FALSE allowpartialdata=FALSE outputfiletype="asc" ',sep='')
	tstr = paste(tstr,'perspeciesresults=FALSE responsecurvesexponent=FALSE	 ',sep='')
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

} # end for species
