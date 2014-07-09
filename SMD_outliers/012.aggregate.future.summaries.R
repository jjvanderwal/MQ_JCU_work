#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
out.dir = paste(base.dir,'Summaries/mammals/',sep='')
pbs.dir = '/rdsi/ccimpacts/SDM_assessment/tmp.pbs/' #set the working directory
setwd(out.dir) #set wd as this output dir

out = NULL
tfiles = list.files(,pattern='summaries',recursive=TRUE)
tfiles =  tfiles[-grep('current',tfiles)] #remove current

for (tfile in tfiles) { cat('.')
	tt = read.csv(tfile,as.is=TRUE)
	try( {out = rbind (out,read.csv(tfile,as.is=TRUE))} )
	if (!is.null(out) & ncol(out) != ncol(tt)) {
		cat('\n',tfile,'\n')
		# setwd(pbs.dir)
		# spp = gsub('.summaries.csv','',tfile)
		# system(paste('qsub -m n -l nodes=1:ppn=1 -l pmem=2gb -l walltime=10:00:00 ',spp,'.sh',sep=''))
		# setwd(paste(out.dir,'current/',sep='')) #set wd as this output dir
	}
}

write.csv(out,'future.summaries.aggregated.csv',row.names=FALSE)
