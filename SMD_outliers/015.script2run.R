#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
################################################################################

args=(commandArgs(TRUE)) #get the command line arguements
for(i in 1:length(args)) { eval(parse(text=args[[i]])) } #evaluate the arguments
#should have read in tfile
# tfile='/rdsi/ccimpacts/SDM_assessment/Summaries/mammals/current/Antechinus_leo.RData'
print(tfile)

###############################################################################
library(SDMTools) #load the libraries

###define the basic directories
base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
data.dir = paste(base.dir,'mammals/models/',sep='')
out.dir = paste(base.dir,'Summaries/mammals/',sep='')

###define the algorithms being used
Algs = c("ann","bioclim","brt","circles","convHull","cta","domain","fda","gam","gbm","geoIDW","geodist",
	"glm","mahal","mars","maxent","rf","sre","voronoiHull") #get a list of the algorithms
Algs.geo = c("circles","convHull","geoIDW","geodist","voronoiHull")
Algs.biomod = c("ann","cta","fda","gam","gbm","glm","mars","rf","sre") #define the biomod algs

###
wd = dirname(tfile) #get the working directory
gcm = basename(wd) #get the gcm name
spp = gsub('.RData','',basename(tfile)) #get the spp name
load(tfile) #load in the data
if (length(grep('output_',colnames(out))>0)) colnames(out) = gsub('output_','',colnames(out)) #confirm colnames correctly named
thresholds = read.csv(paste(out.dir,'/current/',spp,'.summaries.csv',sep=''),as.is=TRUE)[,c(1,2,4)] #read in dataset for thresholds
futdata = out[,which(colnames(out) %in% thresholds$alg)] #keep only algs of interest

###start working
pos = read.csv(paste(out.dir,'base.positions.csv',sep=''),as.is=TRUE) #read in teh base positions
baseasc = read.asc(paste(out.dir,'base.asc',sep='')) #read in teh baseasc

if (!gcm=='current') {
	extrap = read.csv(paste(out.dir,'future.climate.extrapolations.csv',sep=''),as.is=TRUE) #read in teh extrapolation positions
	pos$extrap = rowSums(extrap[,colnames(extrap)[grep(gsub('-','.',gcm),colnames(extrap))]])
	pos$extrap[which(pos$extrap>0)] = 1 #convert to binary

	load(paste(out.dir,'current/',spp,'.RData',sep='')) #load in the current
	if (length(grep('output_',colnames(out))>0)) colnames(out) = gsub('output_','',colnames(out)) #confirm colnames correctly named
	curdata = out[,which(colnames(out) %in% thresholds$alg)] #keep only algs of interest

	### cycle through and grab out the data
	out.areas = NULL #create our outputs
	binary.data.cur = binary.data.fut = futdata #copy the tdata
	for (coi in colnames(futdata)) { cat(coi,'\n') #cycle through the GCMs
		thresh = thresholds$raw.threshold[which(thresholds$alg==coi)] #get the threshold value
		if (is.na(thresh)) {
			tout = data.frame(spp=spp, gcm=gcm, cur=NA, fut=NA, overlap.cur.fut=NA, overlap.cur.fut.extrap=NA, new.fut=NA, 
				new.fut.extrap=NA, stable=NA, stable.extrap=NA, gain=NA, gain.extrap=NA)
		} else {
			tpos = pos #copy pos
			tdata = futdata[,coi] #copy the data
			tdata[which(futdata[,coi]<=thresh)] = 0; tdata[which(futdata[,coi]>thresh)] = 1 #create the data
			tpos$fut = tdata #keep a vector copy
			binary.data.fut[,coi] = tdata #store the binary data
			tdata = curdata[,coi] #copy the data
			tdata[which(curdata[,coi]<=thresh)] = 0; tdata[which(curdata[,coi]>thresh)] = 1 #create the data
			tpos$cur = tdata #keep a vector copy
			binary.data.cur[,coi] = tdata #store the binary data
			
			tpos$overlap.cur.fut = tpos$cur * tpos$fut #get the overlap of current and future
			tpos$overlap.cur.fut.extrap = tpos$overlap.cur.fut * tpos$extrap #get the overlap that is extrapolation
			tpos$new.fut = tpos$fut - tpos$cur; tpos$new.fut[which(tpos$new.fut<1)] = 0 #get just the new future space
			tpos$new.fut.extrap = tpos$new.fut * tpos$extrap #get the new future space that is extrapolation

			tout = data.frame(spp=spp,gcm=gcm,
				cur=sum(tpos$cur*tpos$area,na.rm=TRUE)/1000000,
				fut=sum(tpos$fut*tpos$area,na.rm=TRUE)/1000000,
				overlap.cur.fut=sum(tpos$overlap.cur.fut*tpos$area,na.rm=TRUE)/1000000,
				overlap.cur.fut.extrap=sum(tpos$overlap.cur.fut.extrap*tpos$area,na.rm=TRUE)/1000000,
				new.fut=sum(tpos$new.fut*tpos$area,na.rm=TRUE)/1000000,
				new.fut.extrap=sum(tpos$new.fut.extrap*tpos$area,na.rm=TRUE)/1000000)
			tout$stable = tout$overlap.cur.fut/tout$cur
			tout$stable.extrap = tout$overlap.cur.fut.extrap/tout$overlap.cur.fut
			tout$gain = tout$new.fut/tout$fut
			tout$gain.extrap = tout$new.fut.extrap/tout$new.fut
			tt = grep(NaN,tout); if (length(tt)>0) tout[1,tt] = 0 #convert NaN to 0
		}
		out.areas = rbind(out.areas,tout)	
		
	}	
	write.csv(out.areas,gsub('RData','areas.csv',tfile),row.names=FALSE)
} else {
	binary.data.fut = futdata #copy the tdata
	for (coi in colnames(futdata)) { cat(coi,'\n') #cycle through the GCMs
		thresh = thresholds$raw.threshold[which(thresholds$alg==coi)] #get the threshold value
		tdata = futdata[,coi] #copy the data
		tdata[which(futdata[,coi]<=thresh)] = 0; tdata[which(futdata[,coi]>thresh)] = 1 #create the data
		binary.data.fut[,coi] = tdata #store the binary data
	}
}	

###do the istat
out.istat = NULL #create our outputs
for (from in colnames(futdata)) { cat(from,'\n') #cycle through the GCMs
	x = futdata[,from] #get the from data
	for (to in colnames(futdata)) { cat('\t',to,'\n')
		y = futdata[,to] #get the to data
		tpos = which(is.finite(x) & is.finite(y))
		px = x[tpos]/sum(x[tpos])
		py = y[tpos]/sum(y[tpos])
		H = sqrt(sum((sqrt(px) - sqrt(py))^2))
		Istat = 1 - (H^2)/2
		prop.overlap = sum(binary.data.fut[,from]*binary.data.fut[,to]*pos$area,na.rm=TRUE)/sum(binary.data.fut[,from]*pos$area,na.rm=TRUE)
		out.istat = rbind(out.istat,data.frame(spp=spp,gcm=gcm,from=from,to=to,Istat=Istat,prop.overlap=prop.overlap))
	}
}
write.csv(out.istat,gsub('RData','istat.csv',tfile),row.names=FALSE)
	
	