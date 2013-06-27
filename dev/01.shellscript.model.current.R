# modify the argument file and create the shell script to run the models for each species

species = list.files("/home/jc165798/working/BCCVL/models/") #get a list of all the species

for (sp in species) { #cycle through each of the species

	wd.arg = paste("/home/jc140298/bccvl/", sp, sep=""); dir.create(wd.arg)
	species.arg = sp	
	occur.data.arg = paste("/home/jc140298/bccvl/", sp, "/occur.csv", sep="")
	bkgd.data.arg = paste("/home/jc140298/bccvl/", sp, "/bkgd.csv", sep="")

	setwd(wd.arg)
	shell.file = file(paste("/home/jc140298/bccvl/", sp, "/01.model.current.", sp, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file)
		cat('module load java\n', file=shell.file)
		cat('module load R\n', file=shell.file)
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\" occur.data=\"", occur.data, "\" bkgd.data=\"", bkgd.data, "\"' /home/jc140298/bccvl/01.init.args.model.current.R 01.init.args.model.current.Rout \n", sep="", file=shell.file)
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\"' /home/jc140298/bccvl/01.model.current.R 01.model.current.R.Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/bccvl/", sp, "/01.model.current.", sp, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
}