#initial arguments used to evaluate models

datadir <- "/home/jc165798/working/BCCVL/models/" #define the data directory
wd = "/home/jc140298/bccvl/" #define the working directory
#species <- c("ABT")	#define the species of interest
species =  list.files(datadir, full.names=FALSE) # get a list of species names from the data directory

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
evaluate.maxent = TRUE #boolean to evaluate maxent algorithm


#*************** UNDER CONSTRUCTION ***************

###############
#
# TEST DATA
#
# Define data partitioning methods for calibration (aka training) and testing (aka evaluation) data sets
#
###############

have.test.data = FALSE
# if you have test data, specify the location
test.data.dir = ""

want.test.data = FALSE
# if you want to simulate test data, specify the method to use
partitionMethod = "resubstitution"
# see Fielding and Bell 1997
# options include resubstitution, bootstrapping, prospectiveSampling, kfoldPartitioning, leaveOneOut, K2, kfold
#resubstitution - no partitioning, same data used for training and testing
#bootstrapping - sampling with replacement
#prospectiveSampling - a new sample of cases is obtained from a different region or time
#kfoldPartitioning - data are split into k (k>2) sets, only one is used for training, the rest are pooled for testing
#leaveOneOut - special case of k-fold, n samples of 1 case are tested sequentially, the remaining n-1 form the training set
#K2 - data are split into one training and one testing


use.Huberty.heuristic = FALSE
### Huberty (1994) heuristic for determining the ratio of training to testing cases; restricted to PA models
#p=length(enviro.data)
#huberty = 1-((1+(p-1)^(1/2))^-1)	# p is the number of predictors
#}
if (partitionMethod == "K2") {
	}


###############
#
# THRESHOLD 
# 
# Optional. a vector of threshold values to use for computing the confusion matrices
#
# If threshold is not specified, the default is for evaluate() to create a vector of thresholds based
#	on the predicted values for presence/absence data
#	- if the number of p's and a's is < 1000, then the combined predictions themselves are the threshold values
#	- if either is > 1000, then the 0-100% quantiles are the threshold values
#
###############

opt.threshold = c()
# EMG Not sure how to include: there is no default value, and an empty vector will cause an Error

# options include opinion, omissionRate, maxKappa, minROCdistance, maxSpec_Sens
#opinion - user-specified vector of threshold values
## these next ones are all determined after a set of confusion matrices is made
#omissionRate - the threshold at which there is a specified rate of omission 
#maxKappa - the threshold at which kappa is highest
#minROCdistance - the threshold which has the shortest distance ro the top-left corner (0,1) in ROC plot
#maxSpec_Sens - the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest 


