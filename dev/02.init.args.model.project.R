#initial arguments used to define the inputs, models, outputs, etc

#wd <- "/home/jc165798/working/BCCVL/models/ABT/" 	#define the core working directory
wd <- "/home/jc140298/ABT/" 
species <- "ABT"	#define the species of interest

enviro.data.dir <- "/home/jc165798/working/BCCVL/envirodata"	#define the enviro data directory to use
enviro.data.names <- c("bioclim_01","bioclim_04","bioclim_05","bioclim_06",
	"bioclim_12","bioclim_15","bioclim_16","bioclim_17") #define the names of the enviro data
	# EMG Not sure if this is needed, might use to check layers prior to predicting DOMAIN

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

project.maxent = FALSE #boolean to project maxent algorithm
if (project.maxent) {
	maxent.jar = "/home/jc165798/working/BCCVL/maxent.jar" #define location of maxent.jar file
}