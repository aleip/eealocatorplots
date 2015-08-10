
#uniquefields: all fields without party and years - determines unique variables
#              none of them can completely be ignored, as they are important for at 
#              least one variable
uniquefields<-c("measure","meastype","gas","unit","sector_number","category","method","classification","source","target","type","notation","option","variableUID")

#metafields: all fields that are unique without uid
#

#allfields: all fields including year and party, 
#           brought to an order 
allfields<-c(uniquefields[!uniquefields%in%c("measure","variableUID")],"measure","party",years2keep,"variableUID")
unitareas<-c("ha","kha","ha/year","kha/year","ha/day")

div<-c("Cattle","Sheep","Swine","Livestock")
climateZones<-c("_Cool.*","_Temperate.*","_Warm.*")
manureSystems<-c("Anaerobic lagoon","Composting","Daily spread","Digesters","Liquid system","Solid storage and dry lot","Pasture  range and paddock","Other","Burned for fuel or as waste")
manureSysshrt<-c("anaer","compost","daily","digest","liquid","solid","pasture","other","burned")

meas2sum<-c("EM","AD","POP","FUEL","PROD","AREA","Nleach","Nvol","ADORG","ADSLUDGE","ADEFFLUENT","FLARED","RECOVERY","ADbio","ADnbio")
meas2popweight<-c("IEF","DIGEST","PREGNANT","YM","FEEDING","MILK","Milk","WORK","WEIGHT",
                  "GE","GEav","VSEXC","MASS","B0","NEXC",
                  "FracGASM","FracGASF","FracLEACH","FracBURN","FracOXIDIZED","RatioResCrop",
                  "Combustion","DM","ORGAMENDMENT","YIELD")
meas2clima<-c("CLIMA")
meas2mcf<-c("MCF")
otherlivestock<-c("Other Livestock","Buffalo","Camels","Deer","Goats","Horses",
                  "Mules and Asses","Poultry","Other Other Livestock","Rabbit",
                  "Reindeer","Ostrich","Fur-bearing Animals","Other Other Other Livestock")
otherlivecode<-c("4.","4.1","4.2","4.3","4.4","4.5","4.6","4.7","4.8",
                 "4.8.1","4.8.2","4.8.3","4.8.4","4.8.5")
otherlive<-as.data.frame(otherlivestock)
otherlive$code<-otherlivecode

tables4measures<-c("Allocation by climate region","Feeding situation",
                   "Gross energy","Average gross energy intake","Methane conversion factor",
                   "Milk yield","CH4 producing potential (average)",
                   "Nitrogen excretion rate",
                   "Pregnant","Total N excreted",
                   "Typical animal mass (average)",
                   "VS daily excretion (average)","Weight",
                   "Digestibility of feed")
tables4classifi<-c("Nitrogen excretion per MMS","Emissions","Implied Emission Factor","Population")
tables4sect<-unique(alldata[alldata$measure%in%tables4measures & 
                                grepl("^3",alldata$sector_number) &
                                !grepl("Option",alldata$sector_number),c("sector_number","measure")])
tables4sect$sector_number<-gsub("[1-9] *$","",tables4sect$sector_number)
tables4sect<-unique(tables4sect)

tables4clas<-unique(alldata[alldata$measure%in%tables4classifi & 
                                grepl("^3.[AB]",alldata$sector_number) &
                                !grepl("Option",alldata$sector_number),c("sector_number","classification")])
tables4clas$sector_number<-gsub("[1-9] *$","",tables4clas$sector_number)
tables4clas<-unique(tables4clas)

checktemp<-as.data.frame(matrix(rep(0,8),nrow=1,ncol=8))
checkname<-c("test","val1","val2","ms","yr","sec","val","obs")
