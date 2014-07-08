#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools) #load the libraries

###define the basic directories
base.dir = '/rdsi/ccimpacts/SDM_assessment/' 
data.dir = paste(base.dir,'mammals/models/',sep='')
out.dir = paste(base.dir,'Summaries/mammals/',sep='')

###start working
species = list.files(data.dir); species = species[-grep('zzz',species)] #get a list of the species
pos = read.csv(paste(out.dir,'base.positions.csv',sep=''),as.is=TRUE) #read in teh base positions

##if needed, create the current spp distribution RData files
setwd(paste(out.dir,'current/',sep='')) #set wd as this output dir
recreate=FALSE
if (!all(sapply(paste(species,'.RData',sep=''), file.exists)) | recreate) { #only create if needed
	for (spp in species) {cat(spp,'\n') #cycle through each species and get the current data
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
	}
}

#### now start summarizing
Algs = c("ann","bioclim","brt","circles","convHull","cta","domain","fda","gam","gbm","geoIDW","geodist",
	"glm","mahal","mars","maxent","rf","sre","voronoiHull") #get a list of the algorithms
Algs.geo = c("circles","convHull","geoIDW","geodist","voronoiHull")
Algs.biomod = c("ann","cta","fda","gam","gbm","glm","mars","rf","sre") #define the biomod algs
for (spp in species) {cat(spp,'\n') #cycle through each species and get the current data
	load(paste(spp,'.RData',sep='')) #load in the spp data
	if (length(grep('output_',colnames(out))>0)) colnames(out) = gsub('output_','',colnames(out)) #confirm colnames correctly named
	occur = read.csv(paste(data.dir,spp,'/5km/occur.csv',sep=''),as.is=TRUE) #read in teh occur
	occur$pa = 1; occur = occur[,c('lon','lat','pa')]
	out = merge(out,occur,all=TRUE); out$pa[which(is.na(out$pa))] = 0 
	kappa.fun = function(pa,tdata) {
		K = 0; T = 0
		for (ii in seq(0,1,0.01)) { 
			mat = confusion.matrix(pa,tdata,ii)/10e10
			tval = Kappa(mat)
			if (tval>K) {K=tval;T=ii}
		}
		return(c(K=K,T=T))
	}
	##rescale non-biomod algs
	thresholds = NULL
	for (coi in Algs) { cat('\t',coi,'\n')
		tdata = out[,coi]
		if (all(is.na(tdata))) {
			thresholds = rbind(thresholds,data.frame(alg=coi,raw.threshold=NA,scaled.threshold=NA,Kappa=NA))
		} else {
			if (coi %in% Algs.biomod) {
				tdata = tdata/1000
				tt = kappa.fun(out$pa,tdata)
				thresholds = rbind(thresholds,data.frame(alg=coi,raw.threshold=tt[2]*1000,scaled.threshold=tt[2],Kappa=tt[1]))
			} else if (coi %in% Algs.geo) {
				out[,coi] = NULL #remove from dataset
			} else if (coi %in% c("brt","mahal")) {
				tdata = (out[,coi] + abs(min(out[,coi],na.rm=TRUE)))/diff(range(out[,coi],na.rm=TRUE))
				tt = kappa.fun(out$pa,tdata)
				thresholds = rbind(thresholds,data.frame(alg=coi,raw.threshold=tt[2]*diff(range(out[,coi],na.rm=TRUE))-abs(min(out[,coi],na.rm=TRUE)),scaled.threshold=tt[2],Kappa=tt[1]))
			} else {
				tt = kappa.fun(out$pa,tdata)
				thresholds = rbind(thresholds,data.frame(alg=coi,raw.threshold=tt[2]*1000,scaled.threshold=tt[2],Kappa=tt[1]))
			}
		}
	}
	write.csv(thresholds,paste(spp,'.thresholds.csv',sep=''),row.names=FALSE) #write out the thresholds
}

