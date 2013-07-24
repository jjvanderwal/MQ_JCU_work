#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#=-=-=-=-=-=-=-=-=-=-=-=-=  BIOMOD  =-=-=-=-=-=-=-=-=-=-=-=-=#
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#

# Written by Abigail Cabrelli, Senior Research Officer at Macquarie University
# abigail.cabrelli@mq.edu.au
# 5th July 2013


args = (commandArgs(TRUE))

for (i in 1:length(args)){
  eval(parse(text=args[[i]]))
}

library(biomod2)
library(maptools)

setwd(work.dir)


##### READ IN DATA FILES #####

setwd(species.dir)
occurrences<-read.csv(paste(spp,"/occur.csv",sep=""),header=T)
background<-read.csv(paste(spp,"/bkgd.csv",sep=""),header=T)

myResp<-c(rep(1,nrow(occurrences)),rep(0,nrow(background)))

myRespXY<-rbind(occurrences,background)

setwd(work.dir)
biol1<-readAsciiGrid("Climate_data/current/bioclim_01.asc.gz")
biol4<-readAsciiGrid("Climate_data/current/bioclim_04.asc.gz")
biol5<-readAsciiGrid("Climate_data/current/bioclim_05.asc.gz")
biol6<-readAsciiGrid("Climate_data/current/bioclim_06.asc.gz")
biol12<-readAsciiGrid("Climate_data/current/bioclim_12.asc.gz")
biol15<-readAsciiGrid("Climate_data/current/bioclim_15.asc.gz")
biol16<-readAsciiGrid("Climate_data/current/bioclim_16.asc.gz")
biol17<-readAsciiGrid("Climate_data/current/bioclim_17.asc.gz")
layers<-c(biol1,biol4,biol5,biol6,biol12,biol15,biol16,biol17)
myExpl<-stack(lapply(layers, raster)) 
 
 
##### FORMAT INPUT DATA #####

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = spp)

myBiomodOption = BIOMOD_ModelingOptions(GLM = list(type = 'polynomial',test = 'AIC'),
                                        GBM = list(n.trees = 2000),
                                        GAM = list(spline = 4),
                                        ANN = list(CV.ann = 20),
                                        SRE = list(quant = 0.025))


##### RUN THE MODEL #####

setwd(paste(work.dir,"/Output/",sep=""))

myBiomodModelOut = BIOMOD_Modeling(myBiomodData,
                                   models = c('GLM',
                                              'GBM',
                                              'GAM',
                                              'CTA',
                                              'ANN',
                                              'SRE',
                                              'MARS',
                                              'RF'),
                                   models.options = myBiomodOption,
                                   NbRunEval = 10,
                                   DataSplit=70, 
                                   Prevalence=0.5, 
                                   VarImport=5, 
                                   models.eval.meth = c('ROC'), 
                                   SaveObj = TRUE,
                                   rescal.all.models = TRUE,
                                   modeling.id = spp)

myBiomodModelOut 


##### EVALUATE THE MODEL #####

myBiomodModelEval = getModelsEvaluations(myBiomodModelOut)
VarImportance = getModelsVarImport(myBiomodModelOut)

# Save key info for future use
write.csv(myBiomodModelEval["ROC","Testing.data",,,],(paste(work.dir,"Output/",spp,"/Eval.ROC.csv",sep="")),row.names=T)
write.csv(VarImportance[,,"Full",],(paste(work.dir,"Output/",spp,"/VarImportance.csv",sep="")),row.names=T)


##### SAVE WORKSPACE #####

workspace_list = c("spp","background","occurrences","work.dir","species.dir","climate.dir","myBiomodOption","myBiomodData","myBiomodModelOut","myResp","myExpl","myRespXY")
save(list= workspace_list,file=(paste(work.dir,"Output/",spp,"/",spp,".RData",sep="")))


##### PROJECT THE MODEL #####

library(SDMTools)

dir.create(paste(work.dir,"Output/",spp,"/Ascii_files/",sep=""))

climate<-list.files(climate.dir)

for(k in 1:length(climate)){
  
# Load environmental data
biol1<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_01.asc.gz",sep=""))
biol4<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_04.asc.gz",sep=""))
biol5<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_05.asc.gz",sep=""))
biol6<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_06.asc.gz",sep=""))
biol12<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_12.asc.gz",sep=""))
biol15<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_15.asc.gz",sep=""))
biol16<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_16.asc.gz",sep=""))
biol17<-readAsciiGrid(paste(climate.dir,"/",climate[k],"/bioclim_17.asc.gz",sep=""))
layers<-c(biol1,biol4,biol5,biol6,biol12,biol15,biol16,biol17)
climate.stack<-stack(lapply(layers, raster)) 

# Project the model
myBiomodProj = BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                 new.env = climate.stack,
                                 proj.name = climate[k],
                                 selected.models = c(paste(spp,"_AllData_Full_GLM",sep=""),
                                                     paste(spp,"_AllData_Full_GBM",sep=""),
                                                     paste(spp,"_AllData_Full_GAM",sep=""),
                                                     paste(spp,"_AllData_Full_CTA",sep=""),
                                                     paste(spp,"_AllData_Full_ANN",sep=""),
                                                     paste(spp,"_AllData_Full_SRE",sep=""),
                                                     paste(spp,"_AllData_Full_MARS",sep=""),
                                                     paste(spp,"_AllData_Full_RF",sep="")),
                                 binary.meth = NULL,
                                 compress ='gzip',
                                 clamping.mask = TRUE)

rm(biol1,biol4,biol5,biol6,biol12,biol15,biol16,biol17,climate.stack) 


##### CONVERT OUTPUT FILES TO ASCII #####

myProj<-getProjection(myBiomodProj)

# List the methods used 
model_method = c('GLM', 'GBM', 'GAM', 'CTA', 'ANN', 'SRE', 'MARS', 'RF')

setwd(paste(work.dir,"Output/",spp,"/Ascii_files/",sep=""))
for(j in 1:length(model_method)){
  rast<-raster(myProj,layer=j)
  new.rast<-rast/1000
  model<-sub(paste(spp,"_AllData_Full_",sep=""),"",names(rast)) 
  writeRaster(new.rast,filename=paste(spp,"_",climate[k],"_",model,"_Full.asc",sep=""))
}

# Convert the clamping file to ascii
setwd(paste(work.dir,"Output/",spp,"/",sep=""))
clamp<-raster(paste("proj_",climate[k],"/proj_",climate[k],"_ClampingMask.grd",sep=""))
setwd(paste(work.dir,"Output/",spp,"/Ascii_files/",sep=""))
writeRaster(clamp,filename=paste(spp,"_",climate[k],"_Clamping_Mask.asc",sep=""),format='ascii')

##### SAVING FILES AS PDFS #####

GLM<-raster(paste(spp,"_",climate[k],"_GLM_Full.asc",sep=""))
GBM<-raster(paste(spp,"_",climate[k],"_GBM_Full.asc",sep=""))
GAM<-raster(paste(spp,"_",climate[k],"_GAM_Full.asc",sep=""))
CTA<-raster(paste(spp,"_",climate[k],"_CTA_Full.asc",sep=""))
ANN<-raster(paste(spp,"_",climate[k],"_ANN_Full.asc",sep=""))
SRE<-raster(paste(spp,"_",climate[k],"_SRE_Full.asc",sep=""))
MARS<-raster(paste(spp,"_",climate[k],"_MARS_Full.asc",sep=""))
RF<-raster(paste(spp,"_",climate[k],"_RF_Full.asc",sep=""))

setwd(paste(work.dir,"Output/",spp,"/proj_",climate[k],sep=""))

pdf((paste(spp,"_",climate[k],"_Models.pdf",sep='')),onefile=TRUE, width=6, height=12, paper="a4")
par(mfcol=c(4,2))
plot(GLM, main="GLM", zlim=c(0,1))
plot(GBM, main="GBM", zlim=c(0,1))
plot(GAM, main="GAM", zlim=c(0,1))
plot(CTA, main="CTA", zlim=c(0,1))
plot(ANN, main="ANN", zlim=c(0,1))
plot(SRE, main="SRE", zlim=c(0,1))
plot(MARS, main="MARS", zlim=c(0,1))
plot(RF, main="RF", zlim=c(0,1))
dev.off()

##### CLEAR WORKSPACE AND END LOOPS #####

t<-c("spp","species.names","background","occurrences","work.dir","species.dir","climate.dir","k","climate","myBiomodOption","myBiomodData","myBiomodModelOut","myBiomopdProj","myProj","myResp","myExpl","myRespXY")
Rem<-ls()
rm(list=(Rem[is.na(match(Rem,t))]))  

setwd(paste(work.dir,"/Output/",sep=""))

}

# Make completion file
zzz = file(paste(spp,"/Completed.txt",sep=""),"w")
cat(spp, "\n\n", sep="", file=zzz)
cat("SUCCESSFULLY COMPLETED", date(), sep=" ", file=zzz)
close(zzz)

# Clear workspace completely
rm(list = ls())



