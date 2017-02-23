
# EXCLUDE OTHER LIVESTOCK FROM CHECKS - INHOMOGENEOUS COMPOSITION
paramdata<-paramdata[! paramdata$category=="Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Other Livestock",]
#  ---> eliminate population, not needed any more
paramdata<-paramdata[! paramdata$meastype=="POP",]

# Calculate statistical moments (call functions) ####
param<-paramdata[,c(years,"party","variableUID")]
paramq<-as.data.frame(unique(param$variableUID))
names(paramq)<-"variableUID"
# Calculate the Median Absolute Deviation (mad) 
paramq$mad<-unlist(lapply(paramq$variableUID,function(x) mad(param[param$variableUID==x,years],na.rm=TRUE)))
paramq[,c("mean","std")]<-t(Reduce(cbind,lapply(paramq$variableUID,function(x) selmeansd(param[param$variableUID==x,years]))))
paramq[,newcols]<-t(Reduce(cbind,lapply(paramq$variableUID,function(x) selquantiles(param[param$variableUID==x,years]))))
param<-merge(param,paramq,by="variableUID")

growth<-growthdata[,c(years,"party","variableUID")]
growthquantiles<-subset(growth,select=c("variableUID","party"))
growth[,c("mean","std")]<-t(Reduce(cbind,lapply(c(1:nrow(growth)),function(x) selmeansd(growth[x,years]))))
growth[,newcols]<-t(Reduce(cbind,lapply(c(1:nrow(growth)),function(x) selquantiles(growth[x,years]))))
