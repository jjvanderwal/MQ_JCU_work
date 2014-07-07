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
pos = read.csv(paste(clim.dir,'baseline.76to05/base.positions.csv',sep=''),as.is=TRUE) #read in teh base positions for Australia
baseasc = read.asc(paste(clim.dir,'baseline.76to05/base.asc',sep='')) #read in teh base asc for Australia
pos$area = grid.info(pos$lat,0.05)$area

for (coi in paste('bioclim_',sprintf("%02i",c(1,4,5,6,12,15,16,17)),sep='')) { cat(coi,'\n')
	pos[,coi] = extract.data(cbind(pos$lon,pos$lat),read.asc(paste(clim.dir,'bioclim_asc/current.76to05/',coi,'.asc',sep='')))
}

###do some rounding
for (coi in paste('bioclim_',sprintf("%02i",c(1,5,6)),sep='')) { cat(coi,'\n')
	pos[,coi] = round(pos[,coi],1); bkgd[,coi] = round(bkgd[,coi],1)
}
for (coi in paste('bioclim_',sprintf("%02i",c(12,16,17)),sep='')) { cat(coi,'\n')
	pos[,coi] = round(pos[,coi]); bkgd[,coi] = round(bkgd[,coi])
}
for (coi in paste('bioclim_',sprintf("%02i",c(4,15)),sep='')) { cat(coi,'\n')
	pos[,coi] = round(pos[,coi],3); bkgd[,coi] = round(bkgd[,coi],3)
}

###write out raw data
write.asc(baseasc,'base.asc')
write.csv(pos[,c("lat","lon","row","col","area")],'base.positions.csv',row.names=FALSE)
write.csv(pos,'current.climate.data',row.names=FALSE)

###start some summarizing
##get the extrapolation areas
pos$precip.extrap = pos$temp.extrap = 0
for (coi in paste('bioclim_',sprintf("%02i",c(1,4,5,6)),sep='')) { cat(coi,'\n')
	roi = which(pos[,coi]>max(bkgd[,coi]) | pos[,coi]<min(bkgd[,coi]))
	if (length(roi)>0) pos$temp.extrap[roi] = pos$temp.extrap[roi] + 1
}
for (coi in paste('bioclim_',sprintf("%02i",c(12,15,16,17)),sep='')) { cat(coi,'\n')
	roi = which(pos[,coi]>max(bkgd[,coi]) | pos[,coi]<min(bkgd[,coi]))
	if (length(roi)>0) pos$precip.extrap[roi] = pos$precip.extrap[roi] + 1
}




