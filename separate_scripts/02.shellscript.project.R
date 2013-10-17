# create the shell script to set the arguments and project the models for each species and climate scenario

# get a list of species directories
species.names = list.files("/home/jc165798/working/BCCVL/models") #get a list of all the species

# create a list of model algorithms
model.algorithms = c("bioclim", "domain", "mahal", "geodist", "convHull", "circles", "geoIDW", "voronoiHull", "brt", "maxent",
	"glm", "gam", "gbm", "cta", "ann", "sre", "fda", "mars", "rf", "biomod.maxent")

# get a list of climate scenarios directories, each directory should have env files related to a climate scenario
scenarios = list.files("/home/jc165798/working/BCCVL/envirodata", full.names=TRUE)

# define working directory
wd = "/home/jc140298/ibccvl"

for (sp in species.names[1]) { # cycle through each of the species

	# set the species arg
	species.arg = sp
	
	# set the species specific working directory argument //directory should already be created
	sp.wd.arg = paste(wd, "/", sp, sep=""); setwd(sp.wd.arg)
	
	for (model in model.algorithms[1]) { # cycle through each model algorithm
	
		for (es in scenarios[1]) { # cycle through each of the climate scenarios
			es.name = basename(es)

			# get output directory
			outdir = paste(sp.wd.arg, "/output_", model, sep='')
		
			# create the shell file
			shell.file.name = paste(outdir, "/02.", model, ".project.", sp, ".", es.name, ".sh", sep="")
		
			shell.file = file(shell.file.name, "w")
				cat('#!/bin/bash\n', file=shell.file)
				cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
				cat('cd $PBS_O_WORKDIR\n', file=shell.file)
				cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
				cat('module load java\n', file=shell.file) # need for maxent
				cat('module load R\n', file=shell.file) # need for R
				# this job calls the 02.init.args.project.R file using arguments defined above to set the parameters for the models
				cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\"' ", wd, "/02.init.args.project.R ", outdir, "/02.init.args.project.", sp, ".", es.name, ".Rout \n", sep="", file=shell.file)
				# this job calls the 02.project.R file to run the models
				cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\"' ", wd, "/02.", model, ".project.R ", outdir, "/02.", model, ".project.", sp, ".", es.name, ".Rout \n", sep="", file=shell.file)
			close(shell.file)

			# submit job
			system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
		} # end for climate scenario
	} # end for model
} # end for species