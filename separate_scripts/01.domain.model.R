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
#################################################################################
#
# PROFILE METHODS - only consider presence points: Bioclim, Domain, and Mahal
#
#################################################################################
###############
#
# DOMAIN
#
###############
# domain(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical

if (model.domain) {
	if (!all(enviro.data.type=="continuous")) {
		warning("domain not run because categorical data cannot be used")
	} else {
		outdir = paste(wd,'/output_domain',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
		dm = domain(x=occur[,enviro.data.names]) #run domain with matrix of enviro data
		if (!is.null(dm)) {	
			save(dm,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
		} else {
			write(paste("FAIL!", species, "Cannot create domain model object", sep=": "), stdout())
		}
	}
}