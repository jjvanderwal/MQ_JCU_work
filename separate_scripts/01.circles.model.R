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
necessary=c("dismo","SDMTools") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###load in the data
if (file.exists(occur.data)) {
	load(occur.data);
} else {
	warning("No occurrence data available for model creation!")
}

###run the models and store models
#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################
############### Spatial-only models for presence data ###############
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
	outdir = paste(wd,'/output_circles',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	cc = circles(p=occur[,c('lon','lat')], lonlat=TRUE) #run circles 
	if (!is.null(cc)) {	
		save(cc,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
	} else {
		write(paste("FAIL!", species, "Cannot create circles model object", sep=": "), stdout())
	}
}