# create the shell script to set the arguments and project the models for each species and climate scenario

# get a list of species directories
species.names = list.files("/home/jc165798/working/BCCVL/models/") #get a list of all the species

# get a list of climate scenarios directories, each directory should have env files related to a climate scenario
scenarios = list.files("/home/jc165798/working/BCCVL/envirodata/", full.names=TRUE)

for (sp in species.names) { # cycle through each of the species

	# set the species specific working directory argument //should already be created
	wd.arg = paste("/home/jc140298/bccvl/", sp, "/", sep="")
	# set the species arg
	species.arg = sp	

	# set the working directory to be the species directory
	setwd(wd.arg) 
	
	for (es in scenarios) { # cycle through each of the climate scenarios
	es.name = basename(es)
	
		# create the shell file
		shell.file = file(paste("/home/jc140298/bccvl/", sp, "/02.model.project.", sp, ".", es.name, ".sh", sep=""), "w")
			cat('#!/bin/bash\n', file=shell.file)
			cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
			cat('cd $PBS_O_WORKDIR\n', file=shell.file)
			cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
			cat('module load java\n', file=shell.file) # need for maxent
			cat('module load R\n', file=shell.file) # need for R
		
			# this job calls the 02.init.args.model.project.R file using arguments defined above to set the parameters for the models
			cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\"' /home/jc140298/bccvl/02.init.args.model.project.R 02.init.args.model.project.", sp, ".", es.name, ".Rout \n", sep="", file=shell.file)
			# this job calls the 02.model.project.R file to run the models
			cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\"' /home/jc140298/bccvl/02.model.project.R 02.model.project.", sp, ".", es.name, ".Rout \n", sep="", file=shell.file)
		close(shell.file)

		shell.file.name = paste("/home/jc140298/bccvl/", sp, "/02.model.project.", sp, ".", es.name, ".sh", sep="")
		system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
	} # end for climate scenario
} # end for species