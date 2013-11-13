# create the shell script to set the arguments and project the models for each species and climate scenario

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
	"glm", "gam", "gbm", "cta", "ann", "sre", "fda", "mars", "rf", "biomod.maxent")

# define root climate scenarios directory, each directory should have env files related to a climate scenario
scenarios.dir = "/rdsi/ctbcc_data/Climate/CIAS/Australia"

# create the individual shell scripts
for (taxon in taxa[1]) {

	taxon.dir = paste(wd, "/", taxon, sep="")
	
	# get a list of species directories
	species.names = list.files(paste(taxon.dir, "/models", sep="")) #get a list of all the species

	for (sp in species.names) { # cycle through each of the species

		# set the species arg
		species.arg = sp
			
		for (i in 1:length(scales[1:2])) {
		#EMG Only 5km, 1km is currently available (in current.76to05)
		
			model.scale = scales[i]

			# create the species specific working directory argument
			sp.wd.arg = paste(taxon.dir, "/models/", sp, "/", model.scale, sep=""); setwd(sp.wd.arg)
		
			for (model in model.algorithms[1:10]) { # cycle through each model algorithm
			# EMG only dismo models until I sort out asc.gz projections
		
				# create output directory
				outdir = paste(sp.wd.arg, "/output_", model, sep='')
				
				for (j in 1:length(scales[1:2])) {
				#EMG Only 5km, 1km is currently available (in current.76to05)
				
					project.scale = scales[j]
					
					all.scenarios = list.files(paste(scenarios.dir, "/", project.scale, "/bioclim_asc", sep=""),
						full.names=TRUE)
					#EMG just want RCP85_xxxx_2055 
					rcp85.scenarios = all.scenarios[grep("RCP85", all.scenarios)]
					scenarios = rcp85.scenarios[grep("2055", rcp85.scenarios)]
			
					for (es in scenarios) { # cycle through each of the climate scenarios
						es.name = basename(es)
													
						# create the shell file
						shell.file.name = paste(sp.wd.arg, "/02.", model, ".project.", sp, ".", es.name, ".", model.scale, "_", project.scale, ".sh", sep="")
					
						shell.file = file(shell.file.name, "w")
							cat('#!/bin/bash\n', file=shell.file)
							cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
							cat('cd $PBS_O_WORKDIR\n', file=shell.file)
							cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
							cat('module load java\n', file=shell.file) # need for maxent
							cat('module load R\n', file=shell.file) # need for R
							# this job calls the 02.init.args.project.R file using arguments defined above to set the parameters for the models
							cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\" model.scale=\"", model.scale, "\" project.scale=\"", project.scale, "\"' ", script.dir, "/02.init.args.project.R ", sp.wd.arg, "/02.init.args.project.", sp, ".", es.name, ".", model.scale, "_", project.scale, ".Rout \n", sep="", file=shell.file)
							# this job calls the 02.project.R file to run the models
							cat("R CMD BATCH --no-save --no-restore '--args wd=\"", sp.wd.arg, "\" species=\"", species.arg, "\" es=\"", es, "\" model.scale=\"", model.scale, "\" project.scale=\"", project.scale, "\"' ", script.dir, "/02.", model, ".project.R ", sp.wd.arg, "/02.", model, ".project.", sp, ".", es.name, ".", model.scale, "_", project.scale, ".Rout \n", sep="", file=shell.file)
						close(shell.file)

						# submit job
						system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
					} # end for climate scenario
				} # end for project scale
			} # end for model
		} # end for model scale
	} # end for species
} # end for taxon
	