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
necessary=c("dismo","SDMTools","gstat") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###read in the necessary observation and background data
occur = read.csv(occur.data) #read in the observation data lon/lat
bkgd = read.csv(bkgd.data) #read in the background data lon/lat

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

###run the models and store models
#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################
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
	outdir = paste(wd,'/output_geoIDW',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	gidw = tryCatch(geoIDW(p=occur[,c('lon','lat')], a=bkgd[,c('lon','lat')]), error = err.null) #run the algorithm
	if (!is.null(gidw)) {	
		save(gidw,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
		rm(gidw); #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot create geoIDW model object", sep=": "), stdout())
	}
}