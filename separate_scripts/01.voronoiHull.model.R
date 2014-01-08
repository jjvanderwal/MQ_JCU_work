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
necessary=c("dismo","SDMTools","deldir") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) install.packages(necessary[!installed], dep = T) #if library is not installed, install it
for (lib in necessary) library(lib,character.only=T)#load the libraries

###load in the data
if (file.exists(occur.data) && file.exists(bkgd.data)) {
	load(occur.data); load(bkgd.data);
} else {
	stop("No occurrence or background data available for model creation!")
}

# remove the background records matching the occurrence records 
# duplicates are removed by .voronoiHull() and cause problems if the function itself does it
combined_occur_bkgd = rbind(occur, bkgd)
combined_occur_bkgd = combined_occur_bkgd[-which(duplicated(combined_occur_bkgd[,c("lon","lat")])),]
bkgd = combined_occur_bkgd[combined_occur_bkgd$SPPCODE=="bkgd",]

###run the models and store models
#############################################################################################
#
# GEOGRAPHIC MODELS - use the geographic location of known occurrences
#
#############################################################################################
############### Spatial-only models for presence/background (or absence) data ###############
###############
#
# VORONOIHULL
#
###############
# voronoiHull(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'

if (model.voronoiHull) {
	outdir = paste(wd,'/output_voronoiHull',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	vh = voronoiHull(p=occur[,c('lon','lat')], a=bkgd[,c('lon','lat')]) #run the algorithm
	if (!is.null(vh)) {	
		save(vh,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
	} else {
		write(paste("FAIL!", species, "Cannot create voronoiHull model object", sep=": "), stdout())
	}
}