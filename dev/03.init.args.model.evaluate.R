#initial arguments used to evaluate models

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
# check to see if arguments are passed
if(length(args)==0){
    print("No arguments supplied.")
    # leave all args as default values
	
wd = "/home/jc140298/bccvl/" #define the working directory
species <- c("ABT")	#define the species of interest

} else {
	for(i in 1:length(args)) { 
		eval(parse(text=args[[i]])) 
	}
	# expecting wd, species	
}
# EMG need to expand this to include all other args or come up with a way to parse this properly

### define the models to be used for evaluation
evaluate.bioclim = FALSE #boolean to evaluate BIOCLIM algorithm 
evaluate.domain = FALSE #boolean to evaluate DOMAIN algorithm -- envirodata should have same number of data layers as model creation
evaluate.mahal = FALSE #boolean to evaluate MAHALANOBIS algorithm
evaluate.geodist = FALSE #boolean to evaluate geographic distances algorithm
evaluate.convHull = FALSE #boolean to evaluate convex hulls algorithm 
evaluate.circles = FALSE #boolean to evaluate circles algorithm 
evaluate.geoIDW = FALSE #boolean to evaluate inverse distance weighted algorithm
evaluate.voronoiHull = FALSE #boolean to evaluate Voronoi Hulls algorithm
evaluate.brt = FALSE #boolean to evaluate Boosted regression tree algorithm
evaluate.maxent = FALSE #boolean to evaluate maxent algorithm

############### BIOMOD2 Models ###############
evaluate.glm = TRUE #boolean to evaluate generalized linear model algorithm
evaluate.gam = TRUE #boolean to evaluate generalized boosting model algorithm
evaluate.gbm = TRUE #boolean to evaluate generalized additive model algorithm
evaluate.cta = TRUE #boolean to evaluate classification tree analysis algorithm
evaluate.ann = TRUE #boolean to evaluate artificial neural network algorithm
evaluate.sre = TRUE #boolean to evaluate surface range envelop algorithm
evaluate.fda = TRUE #boolean to evaluate flexible discriminant analysis algorithm
evaluate.mars = TRUE #boolean to evaluate multiple adaptive regression splines algorithm
evaluate.rf = TRUE #boolean to evaluate random forest algorithm
evaluate.biomod.maxent = TRUE #boolean to evaluate {biomod} maxent algorithm

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

# save workspace to set arguments used by 03.model.evaluate.R
save.image(paste(wd, "/03.init.args.model.evaluate.", species, ".RData", sep=""))
save.image(paste(wd, "/03.init.args.model.evaluate.", species, ".Rascii", sep=""), ascii=TRUE) # for Daniel
