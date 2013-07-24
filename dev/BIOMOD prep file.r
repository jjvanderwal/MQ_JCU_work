#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
#=-=-=-=-=-=-=-=-=-=-=-=  SUBMITTING JOBS  =-=-=-=-=-=-=-=-=-=-=-=#
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#

# Written by Abigail Cabrelli, Senior Research Officer at Macquarie University
# abigail.cabrelli@mq.edu.au
# 5th July 2013


work.dir = "/scratch/dsmtb023/WetTropics/BIOMOD/"
species.dir = "/scratch/dsmtb023/WetTropics/BIOMOD/Species_data"
climate.dir = "/scratch/dsmtb023/WetTropics/BIOMOD/Climate_data"

setwd(work.dir)

dir.create(paste(work.dir,"/SH_files/",sep=""))

species.names<-read.table("Input/Species_names.txt",header=T)
species.names<-as.vector(species.names$Species)
species.names<-species.names[1:50]

for(spp in species.names){
  zz = file(paste("SH_files/",spp,'.sh',sep=''),'w')
  cat('##################################\n',file=zz)
  cat('#!/bin/bash\n',file=zz)
  cat('cd $PBS_O_WORKDIR\n',file=zz)
  cat('source /etc/profile.d/modules.sh\n', file=zz)
  cat('module load R/3.0.0\n',file=zz)
  cat("R CMD BATCH --no-save --no-restore '--args spp=\"", spp, "\" work.dir=\"", work.dir, "\" species.dir=\"", species.dir, "\" climate.dir=\"", climate.dir, "\"' /scratch/dsmtb023/WetTropics/BIOMOD/Code/BIOMOD_code.r ",spp,'.Rout \n', sep="", file=zz)
  cat('##################################\n',file=zz)
  close(zz)
  
  system(paste('qsub -l nodes=1:ppn=1 ','/scratch/dsmtb023/WetTropics/BIOMOD/SH_files/',spp,'.sh',sep=''))
  
}
