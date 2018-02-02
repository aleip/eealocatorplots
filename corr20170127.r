#20170127 - Corrections needed for plots
library(data.table)  #xavi20180125: added this line

#1. UK other animals
ukotheranimals<-as.data.table(allagri[allagri$party=="UK"&grepl("A.4|B.[12].4",allagri$sector_number),])
gbotheranimals<-as.data.table(allagri[allagri$party=="GB"&grepl("A.4|B.[12].4",allagri$sector_number),])

#View(ukotheranimals[category=="Poultry"&meastype=="POP"],"ukpoultry")
#View(gbotheranimals[category=="Poultry"&meastype=="POP"],"gbpoultry")
#View(gbotheranimals[category=="Deer"&meastype=="POP"],"gbdeer")
#View(gbotheranimals[category=="Horses"&meastype=="POP"],"gbhorse")

# replace GB data for other animals with UK data
repluids<-unique(ukotheranimals$variableUID)
for(ruid in repluids){
    dexist1<-(plotdata[plotdata$variableUID==ruid & plotdata$party=="UK",years])
    dexist2<-(plotdata[plotdata$variableUID==ruid & plotdata$party=="GB",years])
    if(nrow(dexist1)==nrow(dexist2)&nrow(dexist1)>0)plotdata[plotdata$variableUID==ruid & plotdata$party=="GB",years]<-dexist1
}


#2. Check Slovakia
slkswine<-as.data.table(allagri[allagri$party=="SK"&grepl("A.[3]|B.[12].[3]",allagri$sector_number),])
slksheep<-as.data.table(allagri[allagri$party=="SK"&grepl("A.[2]|B.[12].[2]",allagri$sector_number),])
