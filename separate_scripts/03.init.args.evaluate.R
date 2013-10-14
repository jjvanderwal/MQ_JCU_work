#initial arguments used to evaluate models

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
# check to see if arguments are passed
if(length(args)==0){
    print("No arguments supplied.")
    # leave all args as default values
	
wd = "/home/jc140298/bccvl" #define the working directory
species <- c("ABT")	#define the species of interest

} else {
	for(i in 1:length(args)) { 
		eval(parse(text=args[[i]])) 
	}
	# expecting wd, species	
}
# EMG need to expand this to include all other args or come up with a way to parse this properly

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

# model accuracy statistics 
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")

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

# save workspace to set arguments used by 03.model.evaluate.R
save.image(paste(wd, "/03.init.args.evaluate.", species, ".RData", sep=""))
