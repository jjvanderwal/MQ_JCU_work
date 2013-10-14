# create the shell script to set the arguments and run the models for each species

# get a list of species directories
species.names = list.files("/home/jc165798/working/BCCVL/models") #get a list of all the species

# create a list of model algorithms
model.algorithms = c("bioclim", "domain", "mahal", "geodist", "convHull", "circles", "geoIDW", "voronoiHull", "brt", "maxent",
	"glm", "gam", "gbm", "cta", "ann", "sre", "fda", "mars", "rf", "biomod.maxent")

# define working directory
wd = "/home/jc140298/ibccvl"

for (sp in species.names) { #cycle through each of the species

	# set the species arg
	species.arg = sp	
	# set the location of the occurrence data
	occur.data.arg = paste("/home/jc165798/working/BCCVL/models/", sp, "/occur.csv", sep="") 
	# set the location of the background data
	bkgd.data.arg = paste("/home/jc165798/working/BCCVL/models/", sp, "/bkgd.csv", sep="") 

	# create the species specific working directory argument
	sp.wd.arg = paste(wd, "/", sp, sep=""); dir.create(sp.wd.arg);	setwd(sp.wd.arg) 

	for (model in model.algorithms) { # cycle through each model algorithm
		# create the shell file
		shell.file.name = paste(wd, "/", sp, "/01.", model, ".model.", sp, ".sh", sep="")
		
		shell.file = file(shell.file.name, "w")
			cat('#!/bin/bash\n', file=shell.file)
			cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
			cat('cd $PBS_O_WORKDIR\n', file=shell.file)
			cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
			cat('module load java\n', file=shell.file) # need for maxent
			cat('module load R\n', file=shell.file) # need for R
			# this job calls the 01.init.args.model.R file using arguments defined above to set the parameters for the models
			cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" occur.data=\"", occur.data.arg, "\" bkgd.data=\"", bkgd.data.arg, "\"' ", wd, "/01.init.args.model.R ", wd, "/01.init.args.model.", sp, ".Rout \n", sep="", file=shell.file)
			# this job calls the 01.model.R file to run the models
			cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\"' ", wd, "/01.", model, ".model.R ", wd, "/01.", model, ".model.", sp, ".Rout \n", sep="", file=shell.file)
		close(shell.file)

		# submit job
		system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
	} # end for model
} # end for species