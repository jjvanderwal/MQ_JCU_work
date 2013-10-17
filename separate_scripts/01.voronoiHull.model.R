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

###read in the necessary observation, background and environmental data
#setwd(wd) #set the working directory
populate.data = FALSE #variable to define if there is a need to generate occur & background environmental info
if (file.exists(paste(wd, "/occur.RData", sep="")) && file.exists(paste(wd, "/bkgd.RData", sep=""))) {
	load(paste(wd, "/occur.RData", sep="")); load(paste(wd, "/bkgd.RData", sep="")); #if files already exist, load in the data
	if (!all(colnames(occur)==c('lon','lat',enviro.data.names))) { populate.data=TRUE } #not the right data, we need to repopulate it
} else { populate.data=TRUE } # data does not exist, we need to generate it
if (populate.data) {
	occur = read.csv(occur.data) #read in the observation data lon/lat
	bkgd = read.csv(bkgd.data) #read in teh background position data lon.lat
	for (ii in 1:length(enviro.data)) { cat(ii,'of',length(enviro.data),'\n') #cycle through each of the environmental datasets and append the data
		tasc = read.asc(enviro.data[ii]) #read in the envirodata
		occur[,enviro.data.names[ii]] = extract.data(cbind(occur$lon,occur$lat),tasc) #extract envirodata for observations
		bkgd[,enviro.data.names[ii]] = extract.data(cbind(bkgd$lon,bkgd$lat),tasc) #extract envirodata for background data
	}
	save(occur,file=paste(wd, "/occur.RData", sep="")); save(bkgd,file=paste(wd, "/bkgd.RData", sep="")) #write out the raw data for analysis
}

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
# VORONOIHULL
#
###############
# voronoiHull(p, a, ...)
# p presence points; two column matrix, data.frame or SpatialPoints* object
# a absence points; must be of the same class as 'p'

if (model.voronoiHull) {
	outdir = paste(wd,'/output_voronoiHull',sep=''); #dir.create(outdir,recursive=TRUE); #create the output directory
	vh = tryCatch(voronoiHull(p=occur[,c('lon','lat')], a=bkgd[,c('lon','lat')]), error = err.null) #run the algorithm
	if (!is.null(vh)) {	
		save(vh,file=paste(outdir,"/model.object.RData",sep='')) #save out the model object
		rm(vh); #clean up the memory
	} else {
		write(paste("FAIL!", species, "Cannot create voronoiHull model object", sep=": "), stdout())
	}
}