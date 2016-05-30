
#metafields: all fields that describe 'composite variable'
sectfields<-c("sector_number","category")
metafields<-c("method","classification","source","target","type","option")
measfields<-c("meastype","gas","unit","measure")
quantfields<-c("25","50","70")
#uniquefields: all fields without party and years - determines unique variables
#              none of them can completely be ignored, as they are important for at 
#              least one variable
uniquefields<-c(measfields,sectfields,metafields,"notation","variableUID")
#docfields<-c("issuenr","issueflag","issuedate")
flagnames<-c("ne","init","recalc1","recalc2","unfccc","union","psi1","ptc","notsent","tc","revised")
flag4issues<-c("Country","Obs","question","revyear","sec","gas","invyear","par","key.ms","key.eu",flagnames)
resolved<-c("explanation","expldate","explsource","resolved","comment","followup","communication")
testfields<-c("significant","effect","relmedian","lastyr","share","note")
ipccfields<-c(c("min2006","max2006","ref2006"),c("min1997","max1997","ref1997"))
outlierfields<-c("llim","ulim","min","mean","p25","median","p75","max")
docflags<-paste0("# Flags: 0: new issue; o1: open issue action EU; o2: open issue action MS; o3: open pending (to be checked); cn: closed no issue; cs: closed solved; cu: closed unsolved - new issue opened")
#allfields: all fields including year and party, 
#           brought to an order 
allfields<-c(uniquefields[!uniquefields%in%c("measure","variableUID")],"measure","party",years2keep,"variableUID")

allcheckfields<-c("test","plot","value","range","rellim","years","correction",
                  allfields,ipccfields,resolved,"cursubm",
                  flag4issues,testfields,outlierfields)
allcheckfields4emrt<-c("test","plot","party",sectfields,"Obs","question")
allcheckfields4emrt<-c(allcheckfields4emrt,allcheckfields[!(allcheckfields%in%allcheckfields4emrt)])
listoffields<-list(sectfields=sectfields,metafields=metafields,measfields=measfields,
                   quantfields=quantfields,uniquefields=uniquefields,
                   flagnames=flagnames,flag4issues=flag4issues,resolved=resolved,
                   testfields=testfields,ipccfields=ipccfields,outlierfields=outlierfields,
                   docflags=docflags,allfields=allfields,
                   allcheckfields=allcheckfields,allcheckfields4emrt=allcheckfields4emrt)

unitareas<-c("ha","kha","ha/year","kha/year","ha/day")

div<-c("Cattle","Sheep","Swine","Livestock")
climateZones<-c("Cool","Temperate","Warm")
manureSystems<-c("Anaerobic lagoon","Composting","Daily spread","Digesters","Liquid system","Solid storage and dry lot","Pasture  range and paddock","Other","Burned for fuel or as waste")
manureSysshrt<-c("anaer","compost","daily","digest","liquid","solid","pasture","other","burned")

meas2sum<-c("EM","AD","POP","FUEL","PROD","AREA","NEXC","Nleach","Nvol","ADORG","ADSLUDGE","ADEFFLUENT","FLARED","RECOVERY","ADbio","ADnbio")
meas2popweight<-c("IEF","DIGEST","PREGNANT","YM","FEEDING","MILK","Milk","WORK","WEIGHT",
                  "GE","GEav","VSEXC","MASS","B0","NRATE",
                  "FracGASM","FracGASF","FracLEACH","FracBURN","FracOXIDIZED","RatioResCrop",
                  "Combustion","DM","ORGAMENDMENT","YIELD")
meas2clima<-c("CLIMA")
meas2mcf<-c("MCF")
measta2weight<-c("IEF","WEIGHT","YM","GE","GEav","FEEDING","Milk","PREGNANT","WORK","DIGEST")
meastb12weight<-c("IEF","VSEXC","MASS","B0","MCF","CLIMA")
meastb22weight<-c("IEF","NRATE","Nvol","Nleach")


mslivestockclass<-c("Enteric Fermentation","CH4 Emissions","N2O and NMVOC Emissions","Tier 2")
mslivestocksect<-c("3.A","3.B.1","3.B.2")
livestock<-c("Cattle","Dairy Cattle","Non-Dairy Cattle","Sheep","Swine")
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

checktemp<-as.data.frame(matrix(rep(0,10),nrow=1,ncol=10))
checkname<-c("test","val1","val2","obs","sec","cat","ms","yr","fac","val")

signthreshold<-0.0005 #min of 0.05% of national total
gases<-c("CH4","CO2","N2O","Aggregate GHGs")
gwps<-c(25,1,298,1)
eukp<-"EU28"
eukp<-"EU-KP"

countries2<-c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU","IE","IS","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK","EU28")
countries3<-c("AUT","BEL","BGR","CYP","CZE","DEU","DNM","EST","ESP","FIN","FRK","GBR","GRC","HRV","HUN","IRL","IS","ITA","LT","LUX",
              "LVA","MLT","NLD","POL","PRT","ROU","SWE","SVN","SVK","UK","EU")
countriesl<-c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Germany","Denmark","Estonia",
              "Spain","Finland","France","United Kingdom","Greece","Croatia","Hungary","Ireland",
              "Iceland","Italy","Lithuania","Luxembourg","Latvia","Malta","Netherlands","Poland",
              "Portugal","Romania","Sweden","Slovenia","Slovakia","United Kingdom",
              "EU28")
countrieslthe<-as.vector(sapply(countriesl,function(x) if(x%in%c("Netherlands","United Kingdom")){paste0("the ",x)}else{x}))