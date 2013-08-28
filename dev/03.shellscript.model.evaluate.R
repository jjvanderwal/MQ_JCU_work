# create the shell script to set the arguments and evaluate the models for each species

# get a list of species directories
species.names = list.files("/home/jc165798/working/BCCVL/models/") # get a list of all the species

for (sp in species.names) { # cycle through each of the species

	# set the species specific working directory argument //should already be created
	wd.arg = paste("/home/jc140298/bccvl/", sp, "/", sep="")
	# set the species arg
	species.arg = sp	

	# set the working directory to be the species directory
	setwd(wd.arg) 
	
	# create the shell file
	shell.file = file(paste("/home/jc140298/bccvl/", sp, "/03.model.evaluate.", sp, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
		cat('module load java\n', file=shell.file) # need for maxent
		cat('module load R\n', file=shell.file) # need for R
		
		# this job calls the 03.init.args.model.evaluate.R file using arguments defined above to set the parameters for the models
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\"' /home/jc140298/bccvl/03.init.args.model.evaluate.R 03.init.args.model.evaluate.", sp, ".Rout \n", sep="", file=shell.file)
		# this job calls the 03.model.evaluate.R file to run the models
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\"' /home/jc140298/bccvl/03.model.evaluate.R 03.model.evaluate.", sp, ".Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/bccvl/", sp, "/03.model.evaluate.", sp, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
} # end for species