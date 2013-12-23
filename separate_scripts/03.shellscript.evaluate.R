# create the shell script to set the arguments and evaluate the models for each species

# define location of R scripts
script.dir = "/home/jc140298/MQ_JCU_work/separate_scripts"

# define working dir
wd = "/rdsi/ccimpacts/SDM_assessment"

# define taxa
taxa = c("mammals", "birds", "reptiles", "amphibians")

# define spatial scales
scales = c("5km", "1km", "250m")

# create a list of model algorithms
model.algorithms = c("bioclim", "domain", "mahal", "geodist", "convHull", "circles", "geoIDW", "voronoiHull", "brt", "maxent",
	"glm", "gam", "gbm", "cta", "ann", "sre", "fda", "mars", "rf") #, "biomod.maxent")

# create the individual shell scripts
for (taxon in taxa[1]) {

	taxon.dir = paste(wd, "/", taxon, sep="")

#	for (i in 1:length(scales[1:2])) {
i=1
		# set the location of the background data
		bkgd.data.arg = paste(taxon.dir, "/", scales[i], "_bkgd.csv", sep="")

		# get a list of species directories
		species.names = list.files(paste(taxon.dir, "/models", sep="")) #get a list of all the species
		
		for (sp in species.names[76:100]) { # cycle through each of the species

			# create the species specific working directory argument
			sp.wd.arg = paste(taxon.dir, "/models/", sp, "/", scales[i], sep=""); setwd(sp.wd.arg) 
			
			# set the species arg
			species.arg = sp	

			# set the location of the occurrence data
			occur.data.arg = paste(sp.wd.arg, "/occur.csv", sep="") 

			for (model in model.algorithms[1:9]) { # cycle through each model algorithm
					
				# create the shell file
				shell.file.name = paste(sp.wd.arg, "/03.", model, ".evaluate.", sp, ".sh", sep="")

				shell.file = file(shell.file.name, "w")
					cat('#!/bin/bash\n', file=shell.file)
					cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
					cat('cd $PBS_O_WORKDIR\n', file=shell.file)
					cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
					cat('module load java\n', file=shell.file) # need for maxent
					cat('module load R\n', file=shell.file) # need for R
					
					# this job calls the 03.init.args.evaluate.R file using arguments defined above to set the parameters for the models
					cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" occur.data=\"", occur.data.arg, "\" bkgd.data=\"", bkgd.data.arg, "\"' ", script.dir, "/03.init.args.evaluate.R ", sp.wd.arg, "/03.init.args.evaluate.", sp, ".Rout \n", sep="", file=shell.file)
					# this job calls the 03.evaluate.R file to run the models
					cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\"' ", script.dir, "/03.", model, ".evaluate.R ", sp.wd.arg, "/03.", model, ".evaluate.", sp, ".Rout \n", sep="", file=shell.file)
				close(shell.file)

				# submit job
				system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
			} # end for model
		} # end for species
#	} # end for scales
} # end for taxa