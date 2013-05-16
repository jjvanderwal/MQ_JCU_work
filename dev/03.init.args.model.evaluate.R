#initial arguments used to evaluate models

#wd <- "/home/jc165798/working/BCCVL/models/ABT/" 	#define the core working directory
wd <- "/home/jc140298/ABT/" 
species <- "ABT"	#define the species of interest

### define the models to be used for evaluation
evaluate.bioclim = TRUE #boolean to evaluate BIOCLIM algorithm 
evaluate.domain = TRUE #boolean to evaluate DOMAIN algorithm -- envirodata should have same number of data layers as model creation
evaluate.mahal = TRUE #boolean to evaluate MAHALANOBIS algorithm
evaluate.geodist = TRUE #boolean to evaluate geographic distances algorithm
evaluate.convHull = TRUE #boolean to evaluate convex hulls algorithm 
evaluate.circles = TRUE #boolean to evaluate circles algorithm 
evaluate.geoIDW = TRUE #boolean to evaluate inverse distance weighted algorithm
evaluate.voronoiHull = TRUE #boolean to evaluate Voronoi Hulls algorithm
evaluate.brt = TRUE #boolean to evaluate Boosted regression tree algorithm

evaluate.maxent = FALSE #boolean to evaluate maxent algorithm
if (evaluate.maxent) {
	maxent.jar = "/home/jc165798/working/BCCVL/maxent.jar" #define location of maxent.jar file
}

#*************** UNDER CONSTRUCTION ***************
	
have.test.data = FALSE

if (!have.test.data) {
	
### define data partitioning methods for calibration (aka training) and testing (aka evaluation) data sets
partitionMethod = "resubstitution"	# options include resubstitution, bootstrapping, prospectiveSampling, kfoldPartitioning, leaveOneOut, K2, kfold
### see Fielding and Bell 1997
#resubstitution - no partitioning, same data used for training and testing
#bootstrapping - sampling with replacement
#prospectiveSampling - a new sample of cases is obtained from a different region or time
#kfoldPartitioning - data are split into k (k>2) sets, only one is used for training, the rest are pooled for testing
#leaveOneOut - special case of k-fold, n samples of 1 case are tested sequentially, the remaining n-1 form the training set
#K2 - data are split into one training and one testing
}

#if (partitionMethod == "K2") {
### Huberty (1994) heurisitc for determining the ratio of training to testing cases; restricted to PA models
#p=length(enviro.data)
#huberty = 1-((1+(p-1)^(1/2))^-1)	# p is the number of predictors
#}

### define threshold values
#specifyThreshold = FALSE
#if (specifyThreshold) {
	#threshold = c() # a vector of threshold values to use for computing the confusion matrices
#}