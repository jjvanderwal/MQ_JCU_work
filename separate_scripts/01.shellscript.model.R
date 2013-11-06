# create the shell script to set the arguments and run the models for each species

# define working dir
wd = "/rdsi/ccimpacts/SDM_assessment"

# define taxa
taxa = c("mammals", "birds", "reptiles", "amphibians")

# define spatial scales
scales = c("5km", "1km", "250m")

# create a list of model algorithms
model.algorithms = c("bioclim", "domain", "mahal", "geodist", "convHull", "circles", "geoIDW", "voronoiHull", "brt", "maxent",
	"glm", "gam", "gbm", "cta", "ann", "sre", "fda", "mars", "rf") #, "biomod.maxent")
	
script.dir = "/home/jc140298/MQ_JCU_work/separate_scripts"

for (taxon in taxa[1]) {

	taxon.dir = paste(wd, "/", taxon, sep="")

	for (i in 1:length(scales[1:2])) {
	#EMG Only 5km, 1km is currently available (in current.76to05)

		# set the location of the background data
		bkgd.data.arg = paste(taxon.dir, "/", scales[i], "_bkgd.csv", sep="")

		# get a list of species directories
		species.names = list.files(paste(taxon.dir, "/models", sep="")) #get a list of all the species

		for (sp in species.names[1]) { #cycle through each of the species
		
			# create the species specific working directory argument
			sp.wd.arg = paste(taxon.dir, "/models/", sp, "/", scales[i], sep=""); setwd(sp.wd.arg) 

			# set the species arg
			species.arg = sp	
			# set the location of the occurrence data
			occur.data.arg = paste(sp.wd.arg, "/occur.csv", sep="") 

			for (model in model.algorithms) { # cycle through each model algorithm
	
				# create output directory
				outdir = paste(sp.wd.arg, "/output_", model, sep=''); dir.create(outdir,recursive=TRUE);
		
				# create the shell file
				shell.file.name = paste(sp.wd.arg, "/01.", model, ".model.", sp, ".sh", sep="")
			
				shell.file = file(shell.file.name, "w")
					cat('#!/bin/bash\n', file=shell.file)
					cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
					cat('cd $PBS_O_WORKDIR\n', file=shell.file)
					cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
					cat('module load java\n', file=shell.file) # need for maxent
					cat('module load R\n', file=shell.file) # need for R
					# this job calls the 01.init.args.model.R file using arguments defined above to set the parameters for the models
					cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" occur.data=\"", occur.data.arg, "\" bkgd.data=\"", bkgd.data.arg, "\"' ", script.dir, "/01.init.args.model.R ", sp.wd.arg, "/01.init.args.model.", sp, ".Rout \n", sep="", file=shell.file)
					# this job calls the 01.model.R file to run the models
					cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\"' ", script.dir, "/01.", model, ".model.R ", sp.wd.arg, "/01.", model, ".model.", sp, ".Rout \n", sep="", file=shell.file)
				close(shell.file)

				# submit job
				system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
			} # end for model
		} # end for species
	} # end for scales
} # end for taxa

