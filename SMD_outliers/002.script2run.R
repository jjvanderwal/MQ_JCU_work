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

###define the algorithms being used
Algs = c("ann","bioclim","brt","circles","convHull","cta","domain","fda","gam","gbm","geoIDW","geodist",
	"glm","mahal","mars","maxent","rf","sre","voronoiHull") #get a list of the algorithms
Algs.geo = c("circles","convHull","geoIDW","geodist","voronoiHull")
Algs.biomod = c("ann","cta","fda","gam","gbm","glm","mars","rf","sre") #define the biomod algs

###start working
pos = read.csv(paste(out.dir,'base.positions.csv',sep=''),as.is=TRUE) #read in teh base positions
baseasc = read.asc(paste(out.dir,'base.asc',sep='')) #read in teh baseasc

##if needed, create the current spp distribution RData files
setwd(paste(out.dir,'current/',sep='')) #set wd as this output dir
recreate=FALSE
if (!file.exists(paste(spp,'.RData',sep='')) | recreate) {
	out = pos #define the output
	models = list.files(paste(data.dir,spp,'/5km/',sep=''),pattern = 'output_') #get a list of the models
	for (mod in models) { cat('\t',mod,'\n')
		tfile = list.files(paste(data.dir,spp,'/5km/',mod,'/',sep=''), pattern='current.76to05',full.name=TRUE)
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
occur = read.csv(paste(data.dir,spp,'/5km/occur.csv',sep=''),as.is=TRUE) #read in teh occur
occur$pa = 1; occur = occur[,c('lon','lat','pa')]
out = merge(out,occur,all=TRUE); out$pa[which(is.na(out$pa))] = 0 
n.obs = nrow(occur) #define the number of obs

###do any scaling and calculate thresholds & do class stats...
sum.out = NULL
for (coi in Algs) { cat('\t',coi,'\n')
	tdata = out[,coi]
	if ((all(is.na(tdata)) | sum(tdata)==0) & !coi %in% Algs.geo ) {
		if (is.null(sum.out)) {
			sum.out = NULL
		} else {
			tout = sum.out[1,]; tout[1,] = c(spp,coi,n.obs,rep(NA,ncol(sum.out)-2))
			sum.out = rbind(sum.out,tout)
		}
	} else if (coi %in% Algs.geo) {
			out[,coi] = NULL #remove from dataset	
	} else {
		if (coi %in% Algs.biomod) {
			tdata = tdata/1000
			tt = optim.thresh(out$pa,tdata,101); tt = as.data.frame(tt)[1,]; colnames(tt) = paste('threshold.',colnames(tt),sep='') #kappa.fun(out$pa,tdata)
			tt$maxKappa = accuracy(out$pa,tdata,tt$threshold.maxKappa[1])$Kappa; tt$AUC.full = auc(out$pa,tdata)
			tout = cbind(data.frame(spp=spp,alg=coi,n.obs=n.obs,raw.threshold=tt$threshold.max.sensitivity.specificity[1]*1000,as.data.frame(tt)[1,]),accuracy(out$pa,tdata,tt$threshold.max.sensitivity.specificity[1]))
		} else if (coi %in% c("brt","mahal")) {
			tdata = (out[,coi] + abs(min(out[,coi],na.rm=TRUE)))/diff(range(out[,coi],na.rm=TRUE))
			tt = optim.thresh(out$pa,tdata,101); tt = as.data.frame(tt)[1,]; colnames(tt) = paste('threshold.',colnames(tt),sep='') #kappa.fun(out$pa,tdata)
			tt$maxKappa = accuracy(out$pa,tdata,tt$threshold.maxKappa[1])$Kappa; tt$AUC.full = auc(out$pa,tdata)
			tout = cbind(data.frame(spp=spp,alg=coi,n.obs=n.obs,raw.threshold=tt$threshold.max.sensitivity.specificity[1]*diff(range(out[,coi],na.rm=TRUE))-abs(min(out[,coi],na.rm=TRUE)),as.data.frame(tt)[1,]),accuracy(out$pa,tdata,tt$threshold.max.sensitivity.specificity[1]))
		} else {
			tt = optim.thresh(out$pa,tdata,101); tt = as.data.frame(tt)[1,]; colnames(tt) = paste('threshold.',colnames(tt),sep='') #kappa.fun(out$pa,tdata)
			tt$maxKappa = accuracy(out$pa,tdata,tt$threshold.maxKappa[1])$Kappa; tt$AUC.full = auc(out$pa,tdata)
			tout = cbind(data.frame(spp=spp,alg=coi,n.obs=n.obs,raw.threshold=tt$threshold.max.sensitivity.specificity[1],as.data.frame(tt)[1,]),accuracy(out$pa,tdata,tt$threshold.max.sensitivity.specificity[1]))
		}
		tdata[which(tdata<tt$threshold.max.sensitivity.specificity[1])] = 0; tdata[which(tdata>0)] = 1 #apply the thresholds
		tasc = baseasc; tasc[cbind(pos$row,pos$col)] = tdata
		tt2 = ClassStat(tasc,latlon=TRUE); tt2 = tt2[which(tt2$class==1),]
		tout = cbind(tout,tt2)
		sum.out = rbind(sum.out,tout) #record the info out
	}
	sum.out$alg = as.character(sum.out$alg)
}
write.csv(sum.out,paste(spp,'.summaries.csv',sep=''),row.names=FALSE) #write out the thresholds


