#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools) #load the libraries

###define the basic directories
base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
data.dir = paste(base.dir,'mammals/models/',sep='')
clim.dir = '/rdsi/ctbcc_data/Climate/CIAS/Australia/5km/'
out.dir = paste(base.dir,'Summaries/mammals/',sep=''); setwd(out.dir)

###start working

bkgd = read.csv(paste(base.dir,'mammals/5km_bkgd.csv',sep=''),as.is=TRUE) #read in the background data
pos = read.csv('base.positions.csv',as.is=TRUE) #read in teh base positions for Australia
baseasc = read.asc('base.asc') #read in teh base asc for Australia

GCMs = list.files(paste(clim.dir,'bioclim_asc/',sep=''),pattern='RCP85'); GCMs = GCMs[grep('2055',GCMs)] #get the list of GCMs

###cycle through and collect data
for (gcm in GCMs) { cat(gcm,'\n')
	tout = pos #copy pos
	for (coi in paste('bioclim_',sprintf("%02i",c(1,4,5,6,12,15,16,17)),sep='')) { cat(coi,'\n')
		tout[,coi] = extract.data(cbind(pos$lon,pos$lat),read.asc(paste(clim.dir,'bioclim_asc/',gcm,'/',coi,'.asc',sep='')))
	}
	tout$precip.extrap = tout$temp.extrap = 0
	for (coi in paste('bioclim_',sprintf("%02i",c(1,4,5,6)),sep='')) { cat(coi,'\n')
		roi = which(tout[,coi]>max(bkgd[,coi]) | tout[,coi]<min(bkgd[,coi]))
		if (length(roi)>0) tout$temp.extrap[roi] = tout$temp.extrap[roi] + 1
	}
	for (coi in paste('bioclim_',sprintf("%02i",c(12,15,16,17)),sep='')) { cat(coi,'\n')
		roi = which(tout[,coi]>max(bkgd[,coi]) | tout[,coi]<min(bkgd[,coi]))
		if (length(roi)>0) tout$precip.extrap[roi] = tout$precip.extrap[roi] + 1
	}
	tt = gsub('RCP85_','',gsub('_2055','',gcm))
	pos[,paste(tt,'.temp.extrap',sep='')] = tout$temp.extrap
	pos[,paste(tt,'.precip.extrap',sep='')] = tout$precip.extrap
}
write.csv(pos,'future.climate.extrapolations.csv',row.names=FALSE) ###write out raw data

### convert data and create aggregations for assessing
cois = colnames(pos)[-c(1:5)] #get hte column names to convert
for (coi in cois) {
	roi = which(pos[,coi]>0) #get the >0 rows
	if (length(roi)>0) pos[roi,coi] = 1 #convert to binary
	pos[,coi] = pos$area * pos[,coi] #multiply out the area
}
tt = colSums(pos[,cois])/1000000 #sum up the data in square km
out = as.data.frame(t(as.data.frame(strsplit(names(tt),'[.]')))[,1:2]); colnames(out) = c('gcm','var'); rownames(out) = NULL
out$area.km2 = tt
write.csv(out,'future.extrapolation.summary.csv',row.names=FALSE)



