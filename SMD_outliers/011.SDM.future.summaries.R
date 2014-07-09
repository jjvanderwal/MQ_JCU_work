#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
script.file = '/home/jc165798/SCRIPTS/git_code/MQ_JCU_work/SMD_outliers/011.script2run.R'

base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
data.dir = paste(base.dir,'mammals/models/',sep='')

pbs.dir = '/rdsi/ccimpacts/SDM_assessment/tmp.pbs/'; setwd(pbs.dir) #set the working directory
species = list.files(data.dir); species = species[-grep('zzz',species)] #get a list of all the species

for (spp in species) { #cycle through each of the species
	spp.arg = paste('spp="',spp,'" ',sep='') #define the species argument
	zz = file(paste(spp,'2.sh',sep=''),'w')##create the sh file
		cat('##################################\n',file=zz)
		cat('#!/bin/bash\n',file=zz)
		cat('source /etc/profile.d/modules.sh \n',file=zz)
		cat('module load R/3.0.0\n',file=zz)
		cat('cd $PBS_O_WORKDIR\n',file=zz)
		cat("R CMD BATCH --no-save --no-restore '--args ",spp.arg,"' ",script.file,' ',spp,'.Rout \n',sep='',file=zz)
	close(zz)
			
	#submit the job
	system(paste('qsub -m n -l nodes=1:ppn=1 -l pmem=2gb -l walltime=10:00:00 ',spp,'2.sh',sep=''))
}

