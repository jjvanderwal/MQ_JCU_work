#initial arguments used to define the inputs, models, outputs, etc

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
# check to see if arguments are passed
if(length(args)==0){
    print("No arguments supplied.")
    # leave all args as default values

wd = "/home/jc140298/bccvl/" #define the working directory
species = "ABT"	#define the species of interest
es.name = "climate_1990"
enviro.data = c("/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_01.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_04.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_05.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_06.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_12.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_15.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_16.asc",
"/home/jc165798/working/BCCVL/envirodata/climate_1990/bioclim_17.asc") #define the enviro data to use -- assumed location of data files in ascii grid format
enviro.data.names = c("bioclim_01","bioclim_04","bioclim_05","bioclim_06",
"bioclim_12","bioclim_15","bioclim_16","bioclim_17") #define the names of the enviro data
enviro.data.type = c('continuous','continuous','continuous','continuous',
'continuous','continuous','continuous','continuous') #type in terms of continuous or categorical
	# EMG should we restrict these to be the same as model creation?
	# - if additional layers, all but DOMAIN will handle it (unless we can drop them explicitly)
	# - if missing layers, BIOCLIM, DOMAIN, MAHAL, and BRT will produce Errors
} else {
	for(i in 1:length(args)) { 
		eval(parse(text=args[[i]])) 
	}
	# expecting wd, species, es
	es.name = basename(es)
	enviro.data = list.files(es, full.names=TRUE)
	enviro.data.names = basename(enviro.data)
	
}
# EMG need to expand this to include all other args or come up with a way to parse this properly


### define the models to be used for projection
project.bioclim = TRUE #boolean to project BIOCLIM algorithm 
if (project.bioclim) {
	# there is an additional argument 'tails' which you can use to ignore the left or right tail of the percentile distribution for a variable
	opt.tails = c("both") # character vector with a length equal to the number of variables used in the model
	# valid values are "both" (the default), "low" and "high" 
	# if only one value is entered in the vector, that value will apply to each variable
	# For example, if you have a variable x with an observed distribution between 10 and 20 and you are predicting the bioclim value for a value 25, 
		# the default result would be zero (outside of all observed values); but if you use tail='low', the high (right) tail is ignored and the value 
		# returned will be 1.
}

project.domain = TRUE #boolean to project DOMAIN algorithm -- envirodata should have same number of data layers as model creation
project.mahal = TRUE #boolean to project MAHALANOBIS algorithm
project.geodist = TRUE #boolean to project geographic distances algorithm
project.convHull = TRUE #boolean to project convex hulls algorithm 
project.circles = TRUE #boolean to project circles algorithm 
project.geoIDW = TRUE #boolean to project inverse distance weighted algorithm
if (project.geodist) {
	opt.fun = NULL # allows you to use your own (inverse) distance function
	opt.scale=1 # allows you to scale the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance)
}

project.voronoiHull = TRUE #boolean to project Voronoi Hulls algorithm
project.brt = TRUE #boolean to project Boosted regression tree algorithm

project.maxent = TRUE #boolean to project maxent algorithm
if (project.maxent) {
	maxent.jar = "/home/jc165798/working/BCCVL/maxent.jar" #define location of maxent.jar file
	# there is also an additional argument 'args' used to pass arguments (options) to the maxent software.
}

### define the extent to be used for projection
opt.ext=NULL
#An extent object to limit the prediction to a sub-region of 'x'. Or an object that can be coerced to an Extent object by extent; such as a Raster* or Spatial* object 

# save workspace to set arguments used by 02.model.project.R
save.image(paste(wd, "/02.init.args.model.project.", species, ".", es.name, ".RData", sep=""))
save.image(paste(wd, "/02.init.args.model.project.", species, ".", es.name, ".Rascii", sep=""), ascii=TRUE) # for Daniel