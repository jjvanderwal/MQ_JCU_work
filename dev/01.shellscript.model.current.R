# create the shell script to set the arguments and run the models for each species

# get a list of species directories
species = list.files("/home/jc165798/working/BCCVL/models/") #get a list of all the species

for (sp in species) { #cycle through each of the species

	# set the species specific working directory argument and create it
	wd.arg = paste("/home/jc140298/bccvl/", sp, "/", sep=""); dir.create(wd.arg) 
	# set the species arg
	species.arg = sp	
	# set the location of the occurrence data
	occur.data.arg = paste("/home/jc165798/working/BCCVL/models/", sp, "/occur.csv", sep="") 
	# set the location of the background data
	bkgd.data.arg = paste("/home/jc165798/working/BCCVL/models/", sp, "/bkgd.csv", sep="") 

	# set the working directory to be the species directory
	setwd(wd.arg) 
	# create the shell file
	shell.file = file(paste("/home/jc140298/bccvl/", sp, "/01.model.current.", sp, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
		cat('module load java\n', file=shell.file) # need for maxent
		cat('module load R\n', file=shell.file) # need for R
		# this job calls the 01.init.args.model.current.R file using arguments defined above to set the parameters for the models
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\" occur.data=\"", occur.data.arg, "\" bkgd.data=\"", bkgd.data.arg, "\"' /home/jc140298/bccvl/01.init.args.model.current.R 01.init.args.model.current.", sp, ".Rout \n", sep="", file=shell.file)
		# this job calls the 01.model.current.R file to run the models
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\"' /home/jc140298/bccvl/01.model.current.R 01.model.current.", sp, ".Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/bccvl/", sp, "/01.model.current.", sp, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
} # end for species