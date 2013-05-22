#initial arguments used to define the inputs, models, outputs, etc

#wd <- "/home/jc165798/working/BCCVL/models/ABT/" #define the core working directory
datadir <- "/home/jc165798/working/BCCVL/models/" #define the data directory
wd = "/home/jc140298/bccvl/"
#species <- c("ABT", "ANTADUS", "ANTFLAV") #define the species of interest
#occur.data <- "/home/jc165798/working/BCCVL/models/ABT/occur.csv" #define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
#bkgd.data <- "/home/jc165798/working/BCCVL/models/ABT/bkgd.csv" #define the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
occur.data.name <- "occur.csv" #define the filename of the lon/lat of the observation records -- 2 column matrix of longitude and latitude
bkgd.data.name <- "bkgd.csv" #define the filename of the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude

enviro.data <- c("/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_01.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_04.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_05.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_06.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_12.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_15.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_16.asc",
	"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_17.asc") #define the enviro data to use -- assumed location of data files in ascii grid format
enviro.data.names <- c("bioclim_01","bioclim_04","bioclim_05","bioclim_06",
	"bioclim_12","bioclim_15","bioclim_16","bioclim_17") #define the names of the enviro data
enviro.data.type <- c('continuous','continuous','continuous','continuous',
	'continuous','continuous','continuous','continuous') #type in terms of continuous or categorical

### define the models to be used
model.bioclim = TRUE #boolean to run BIOCLIM algorithm -- all envirodata must be continuous
model.domain = TRUE #boolean to run DOMAIN algorithm -- all envirodata must be continuous
model.mahal = TRUE #boolean to run MAHALANOBIS algorithm -- all envirodata must be continuous
model.geodist = TRUE #boolean to run geographic distances algorithm -- only requires lon/lat of observations
model.convHull = TRUE #boolean to run convex hulls algorithm -- only requires lon/lat of observations
model.circles = TRUE #boolean to run "circles" algorithm -- only requires lon/lat of observations
model.geoIDW = TRUE #boolean to run inverse distance weighted algorithm -- only requires lon/lat of observations and pseudo-absences
model.voronoiHull = TRUE #boolean to run Voronoi Hulls algorithm -- only requires lon/lat of observations and pseudo-absences

model.brt = TRUE #boolean to run Boosted regression tree algorithm
if (model.brt) { #additional parameters to set
	brt.fold.vector = NULL #a fold vector to be read in for cross validation with offsets
	brt.tree.complexity = 1 #sets the complexity of individual trees
	brt.learning.rate = 0.01 #sets the weight applied to individual trees
	brt.bag.fraction = 0.75 #sets the proportion of observations used in selecting variables
	#brt.site.weights = rep(1, nrow(data)) #allows varying weighting for sites
	#brt.var.monotone = rep(0, length(gbm.x)) #restricts responses to individual predictors to monotone
	brt.n.folds = 10 #number of folds
	brt.prev.stratify = TRUE #prevalence stratify the folds - only for presence/absence data
	brt.family = "bernoulli" #family - bernoulli (=binomial), poisson, laplace or gaussian
	brt.n.trees = 50 #number of initial trees to fit
	brt.step.size = brt.n.trees #numbers of trees to add at each cycle
	brt.max.trees = 10000 #max number of trees to fit before stopping
	brt.tolerance.method = "auto" #method to use in deciding to stop - "fixed" or "auto"
	brt.tolerance = 0.001 #tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance
	brt.keep.data = FALSE #Logical. keep raw data in final model
	brt.plot.main = FALSE #Logical. plot hold-out deviance curve
	brt.plot.folds = FALSE #Logical. plot the individual folds as well
	brt.verbose = TRUE #Logical. control amount of screen reporting
	brt.silent = FALSE #Logical. to allow running with no output for simplifying model)
	brt.keep.fold.models = FALSE  #Logical. keep the fold models from cross valiation
	brt.keep.fold.vector = FALSE #Logical. allows the vector defining fold membership to be kept
	brt.keep.fold.fit = FALSE #Logical. allows the predicted values for observations from cross-validation to be kept
}

model.maxent = TRUE #boolean to run maxent algorithm
if (model.maxent) {
	maxent.jar = "/home/jc165798/working/BCCVL/maxent.jar" #define location of maxent.jar file
	outputformat='logistic' #options include logistic, cumulative, raw
	randomseed=FALSE
	logscale=TRUE
	removeduplicates=TRUE
	randomtestpoints=0
	betamultiplier=1
	maximumbackground=10000
	biasfile=NULL
	testsamplesfile=NULL
	replicates=1
	replicatetype="crossvalidate" #options include crossvalidate, bootstrap, subsample
	linear=TRUE
	quadratic=TRUE
	product=TRUE
	threshold=TRUE
	hinge=TRUE
	addsamplestobackground=TRUE
	addallsamplestobackground=FALSE
	fadebyclamping=FALSE
	extrapolate=TRUE
	autofeature=TRUE
	doclamp=TRUE
	maximumiterations=500
	convergencethreshold=1.00E-05
	lq2lqptthreshold=80
	l2lqthreshold=10
	hingethreshold=15
	beta_threshold=-1
	beta_categorical=-1
	beta_lqp=-1
	beta_hinge=-1
	defaultprevalence=0.5
	nodata=-9999
}
