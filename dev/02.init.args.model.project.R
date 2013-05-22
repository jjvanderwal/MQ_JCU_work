#initial arguments used to define the inputs, models, outputs, etc

#wd <- "/home/jc165798/working/BCCVL/models/ABT/" 	#define the core working directory
wd <- "/home/jc140298/bccvl/"
#species <- c("ABT", "ANTADUS", "ANTFLAV")	#define the species of interest

enviro.data.dir <- "/home/jc165798/working/BCCVL/envirodata"	#define the enviro data directory to use
enviro.data.names <- c("bioclim_01","bioclim_04","bioclim_05","bioclim_06",
	"bioclim_12","bioclim_15","bioclim_16","bioclim_17") #define the names of the enviro data
	# EMG should we restrict these to be the same as model creation?
	# - if additional layers, all but DOMAIN will handle it (unless we can drop them explicitly)
	# - if missing layers, BIOCLIM, DOMAIN, MAHAL, and BRT will produce Errors

### define the models to be used for projection
project.bioclim = TRUE #boolean to project BIOCLIM algorithm 
project.domain = TRUE #boolean to project DOMAIN algorithm -- envirodata should have same number of data layers as model creation
project.mahal = TRUE #boolean to project MAHALANOBIS algorithm
project.geodist = TRUE #boolean to project geographic distances algorithm
project.convHull = TRUE #boolean to project convex hulls algorithm 
project.circles = TRUE #boolean to project circles algorithm 
project.geoIDW = TRUE #boolean to project inverse distance weighted algorithm
project.voronoiHull = TRUE #boolean to project Voronoi Hulls algorithm
project.brt = TRUE #boolean to project Boosted regression tree algorithm

project.maxent = TRUE #boolean to project maxent algorithm
if (project.maxent) {
	maxent.jar = "/home/jc165798/working/BCCVL/maxent.jar" #define location of maxent.jar file
	# there is also an additional argument 'args' used to pass arguments (options) to the maxent software.
}

#*************** UNDER CONSTRUCTION ***************

if (project.bioclim) {
	# there is an additional argument 'tails' which you can use to ignore the left or right tail of the percentile distribution for a variable
	tails = "both" # character vector with a length equal to the number of variables used in the model
	# valid values are "both" (the default), "low" and "high" 
	# For example, if you have a variable x with an observed distribution between 10 and 20 and you are predicting the bioclim value for a value 25, 
		# the default result would be zero (outside of all observed values); but if you use tail='low', the high (right) tail is ignored and the value 
		# returned will be 1.
	# EMG: no idea what this means
}

if (project.geodist) {
	# there is an additional argument fun that allows you to use your own (inverse) distance function
	scale=1 # allows you to scale the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance)
}