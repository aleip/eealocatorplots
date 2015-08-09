
#uniquefields: all fields without party and years - determines unique variables
#              none of them can completely be ignored, as they are important for at 
#              least one variable
uniquefields<-c("measure","meastype","gas","unit","sector_number","category","method","classification","source","target","type","notation","option","variableUID")

#allfields: all fields including year and party, 
#           brought to an order 
allfields<-c(uniquefields[!uniquefields%in%c("measure","variableUID")],"measure","party",years2keep,"variableUID")
unitareas<-c("ha","kha","ha/year","kha/year","ha/day")

div<-c("Cattle","Sheep","Swine","Livestock")
climateZones<-c("_Cool.*","_Temperate.*","_Warm.*")
manureSystems<-c("Anaerobic lagoon","Composting","Daily spread","Digesters","Liquid system","Solid storage and dry lot","Pasture  range and paddock","Other","Burned for fuel or as waste")
manureSysshrt<-c("anaer","compost","daily","digest","liquid","solid","pasture","other","burned")

meas2sum<-c("EM","AD","POP","FUEL","PROD","AREA","Nleach","Nvol")
meas2popweight<-c("IEF","DIGEST","PREGNANT","YM","FEEDING","MILK","Milk","WORK","WEIGHT",
                  "GE","GEav","VSEXC","MASS","B0","NEXC",
                  "FracGASM","FracGASF","FracLEACH","FracBURN","FracOXIDIZED","RatioResCrop",
                  "Combustion","DM","ORGAMENDMENT","YIELD")
meas2clima<-c("CLIMA")
meas2mcf<-c("MCF")
