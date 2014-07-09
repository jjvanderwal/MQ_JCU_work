#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################

args=(commandArgs(TRUE)) #get the command line arguements
for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments
#should have read in spp
print(spp)
# spp = 'Antechinus_leo'


###############################################################################
library(SDMTools) #load the libraries

###define the basic directories
base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
data.dir = paste(base.dir,'mammals/models/',sep='')
out.dir = paste(base.dir,'Summaries/mammals/',sep='')
clim.dir = '/rdsi/ctbcc_data/Climate/CIAS/Australia/5km/'

###start working
pos = read.csv(paste(out.dir,'base.positions.csv',sep=''),as.is=TRUE) #read in teh base positions
baseasc = read.asc(paste(out.dir,'base.asc',sep='')) #read in teh baseasc

GCMs = list.files(paste(clim.dir,'bioclim_asc/',sep=''),pattern='RCP85'); GCMs = GCMs[grep('2055',GCMs)] #get the list of GCMs
GCMs = gsub('RCP85_','',gsub('_2055','',GCMs))

for (gcm in GCMs) { cat(gcm,'\n')
	##if needed, create the current spp distribution RData files
	t.dir = paste(out.dir,gcm,'/',sep=''); dir.create(t.dir); setwd(t.dir) #set wd as this output dir
	recreate=FALSE
	if (!file.exists(paste(spp,'.RData',sep='')) | recreate) {
		out = pos #define the output
		models = list.files(paste(data.dir,spp,'/5km/',sep=''),pattern = 'output_') #get a list of the models
		for (mod in models) { cat('\t',mod,'\n')
			tfile = list.files(paste(data.dir,spp,'/5km/',mod,'/',sep=''), pattern=gcm,full.name=TRUE)
			if (length(tfile)>0) {
				if (length(tfile)>1) tfile = tfile[grep('gz',tfile)]
				if (length(tfile)>1) tfile = tfile[1]
				if (file.exists(tfile)) {
					out[,gsub('output_','',mod)] = extract.data(cbind(pos$lon,pos$lat),read.asc.gz(tfile))
				} else {
					out[,gsub('output_','',mod)] = NA
				}
			} else {
				out[,gsub('output_','',mod)] = NA
			}
		}
		save(out,file=paste(spp,'.RData',sep='')) #save out the data
	}

	#### now start summarizing
	load(paste(spp,'.RData',sep='')) #load in the spp data
	if (length(grep('output_',colnames(out))>0)) colnames(out) = gsub('output_','',colnames(out)) #confirm colnames correctly named
	thresholds = read.csv(paste(out.dir,'/current/',spp,'.summaries.csv',sep=''),as.is=TRUE)[,c(1,2,4)] #read in dataset for thresholds

	###do any scaling and calculate thresholds & do class stats...
	sum.out = NULL
	for (coi in thresholds$alg) { cat('\t',coi,'\n')
		thresh = thresholds$raw.threshold[which(thresholds$alg==coi)] #get the threshold value
		tout = data.frame(spp=spp,gcm=gcm,alg=coi)
		if (!is.na(thresh)) {
			tdata = out[,coi] #copy the data
			tdata[which(out[,coi]<=thresh)] = 0; tdata[which(out[,coi]>thresh)] = 1 #create the binary map
			tasc = baseasc; tasc[cbind(pos$row,pos$col)] = tdata
			tt2 = ClassStat(tasc,latlon=TRUE); tt2 = tt2[which(tt2$class==1),]
			if (nrow(tt2)==0) tt2[1,] = c(1,0,0,0,rep(NA,ncol(tt2)-4))
			tout = cbind(tout,tt2)
			sum.out = rbind(sum.out,tout) #record the info out
		}
		sum.out$alg = as.character(sum.out$alg)
	}
	write.csv(sum.out,paste(spp,'.summaries.csv',sep=''),row.names=FALSE) #write out the thresholds

}
