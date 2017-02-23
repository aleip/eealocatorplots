
#metafields: all fields that describe 'composite variable'
sectfields<-c("sector_number","category")
metafields<-c("method","classification","source","target","type","option")
measfields<-c("meastype","gas","unit","measure")
quantfields<-c("25","50","70")
#uniquefields: all fields without party and years - determines unique variables
#              none of them can completely be ignored, as they are important for at 
#              least one variable
uniquefields<-c(measfields,sectfields,metafields,"notation","variableUID","datasource")
uniquefields<-c(measfields,sectfields,metafields,"variableUID","datasource")
#docfields<-c("issuenr","issueflag","issuedate")
flagnames<-c("ne","init","recalc1","recalc2","unfccc","union","psi1","ptc","notsent","tc","revised")
flag4issues<-c("Country","Obs","question","revyear","sec","gas","invyear","par","key.ms","key.eu",flagnames)
resolved<-c("explanation","expldate","explsource","resolved","comment","followup","communication")
testfields<-c("significant","effect","relmedian","lastyr","share","note")
ipccfields<-c(c("min2006","max2006","ref2006"),c("min1997","max1997","ref1997"))
outlierfields<-c("llim","ulim","min","mean","p25","median","p75","max")
docflags<-paste0("# Flags: 0: new issue; o1: open issue action EU; o2: open issue action MS; o3: open pending (to be checked); cn: closed no issue; cs: closed solved; cu: closed unsolved - new issue opened; csr: closed solved - repopened for TERT recommendation")
#allfields: all fields including year and party, 
#           brought to an order 
allfields<-c(uniquefields[!uniquefields%in%c("measure","variableUID")],"measure","party",years2keep,"variableUID")

allcheckfields<-unique(c("test","plot","value","range","rellim","years","correction",
                  allfields,ipccfields,resolved,"cursubm",
                  flag4issues,testfields,outlierfields))
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

checkname<-c("test","val1","val2","obs","sec","cat","ms","yr","fac","val")
checktemp<-as.data.frame(matrix(vector(),nrow=1,ncol=10,dimnames=list(c(),checkname)))
names(checktemp)<-checkname
signthreshold<-0.0005 #min of 0.05% of national total
gases<-c("CH4","CO2","N2O","Aggregate GHGs","NMVOC")
# Values from Third Assessment Report 2001 http://www.grida.no/publications/other/ipcc_tar/?src=/climate/ipcc_tar/wg1/212.htm
gwpstar<-c(23,1,296,1,0)
# Values from Forth Assessment Report 2007 https://www.ipcc.ch/publications_and_data/ar4/wg1/en/ch2s2-10-2.html
gwpsar4<-c(25,1,298,1,0)
# Values from Fifth Assessment Report 2013 (page 714) https://ipcc.ch/pdf/assessment-report/ar5/wg1/WG1AR5_Chapter08_FINAL.pdf
gwpsar5<-c(28,1,265,1,0)

gwps<-gwpsar4


#email david 20160203
#* - GBE (CRF Party code & ‘submission file’ in the locator) = UK (‘Party code’ in the locator) = Great Britain (‘Party name’ in the locator) = EU-territory geographical coverage > EU’s submission under the Convention
#* - GBR (CRF Party code & ‘submission file’ in the locator) = GB (‘Party code’ in the locator) = United Kingdom (‘Party name’ in the locator) = KP geographical coverage > EU’s submission under KP

#email david 20150703 (quoted in email spyri 20150706)
# - 2 XML files for UK: GBE and GBR ("GBE" has been assigned code "GB" and label "Great Britain", 
#                                    "GBR" has been assigned code "UK" and label "United Kingdom").

#email riccardo 20160530
#I think the first email is right: UK=GBE=EU-territory & GB=GBR=KP
#GBR is clearly KP and should be higher than GBE. 
#The Party name is confusing because it is the United Kingdom in both instances 
#(one with overseas territories 'GBR' and the other without 'GBE').  We should improve this next year. 

countries2<-c("AT" ,"BE" ,"BG" ,"CY" ,"CZ" ,"DE" ,"DK" ,"EE" ,"ES" ,"FI" ,"FR" ,"FM" ,"GB" ,"UK" ,"GR" ,"HR" ,"HU" ,"IE" ,"IS" ,"IT" ,"LT" ,"LU" ,"LV" ,"MT" ,"NL" ,"PL" ,"PT" ,"RO" ,"SE" ,"SI" ,"SK" )
countries3<-c("AUT","BEL","BGR","CYP","CZE","DEU","DNM","EST","ESP","FIN","FRK","FRK","GBR","GBE","GRC","HRV","HUN","IRL","ISL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SWE","SVN","SVK")
eu<-c("EUA","EUC")
eum<-c("EU28","EU28+ISL")
eul<-c("EU territorial coverage (Convention=EU28)","EU geographical coverage under KP (EU28+ISL)")

countriesl<-c("Austria","Belgium","Bulgaria","Cyprus","Czech Republic","Germany","Denmark","Estonia",
              "Spain","Finland","France","France incl Mayotte",
              "United Kingdom (GB=GBR=KP geographical coverage)","United Kingdom (UK=GBE=EU territory)","Greece","Croatia","Hungary","Ireland",
              "Iceland","Italy","Lithuania","Luxembourg","Latvia","Malta","Netherlands","Poland",
              "Portugal","Romania","Sweden","Slovenia","Slovakia")

country4sub<-as.data.frame(c(countries2,eu))
country4sub$countries3<-c(countries3,eu)
country4sub$countriesl<-c(countriesl,eul)
names(country4sub)<-c("code2","code3","long")
country4sub$EU28<-1
country4sub$EUA<-1
country4sub$EUC<-1
country4sub$EU28[country4sub$code3=="ISL"]<-0
country4sub$EUA[country4sub$code3=="ISL"]<-0

# France: EU28 and EU-inventory for UNFCCC: France excluding Mayotte
#         Kyoto: France including Mayotte
country4sub$EU28[country4sub$code2=="FR"]<-0
country4sub$EUA[country4sub$code2=="FR"]<-0
country4sub$EUC[country4sub$code2=="FM"]<-0

#
country4sub$EU28[country4sub$code3=="GBR"]<-0
country4sub$EUA[country4sub$code3=="GBR"]<-0
country4sub$EUC[country4sub$code3=="GBE"]<-0
country4sub$EU28[country4sub$code3=="EUC"]<-0
country4sub$EU28[country4sub$code3=="EUA"]<-0
country4sub$EUA[country4sub$code3=="EU28"]<-0
country4sub$EUA[country4sub$code3=="EUC"]<-0
country4sub$EUC[country4sub$code3=="EU28"]<-0
country4sub$EUC[country4sub$code3=="EUA"]<-0

country4sub$name<-country4sub$long
country4sub$name[grepl("United",country4sub$name)]<-"United Kingdom"
country4sub$name[grepl("France",country4sub$name)]<-"France"
country4sub$name[country4sub$code3=="EUA"]<-"EU28"
country4sub$name[country4sub$code3=="EUC"]<-"EU28+ISL"

country4sub$thename<-as.vector(sapply(country4sub$name,function(x) if(x%in%c("Netherlands","United Kingdom")){paste0("the ",x)}else{x}))

countrieslthe<-as.vector(sapply(countriesl,function(x) if(x%in%c("Netherlands","United Kingdom")){paste0("the ",x)}else{x}))

eunames<-as.data.frame("EU28")
names(eunames)<-"EUA"
eunames$EUC<-"EU28+ISL"
