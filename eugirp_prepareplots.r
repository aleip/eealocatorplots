print("")
print("start eugirp_prepareplots.R")
#resti<-plotmeas$imeas[plotmeas$category=="Dairy Cattle" | plotmeas$category=="Non-Dairy Cattle"]
#for(imeas in resti){   
#for(imeas in c(1:10)){
#for(imeas in c(1:nrow(plotmeas))){
runfocus<-"value"
runfocus<-"range"
runfocus<-"compare"
x1<-2;x2<-2
x1<-1;x2<-nrow(plotmeas)
if(runfocus!="compare") for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = runfocus)}
if(runfocus=="compare") for(imeas in x1:x2){plotcomparison(imeas,plotmeas,plotdata,lyear = 2014)}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")



