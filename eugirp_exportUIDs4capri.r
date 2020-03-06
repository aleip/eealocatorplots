selhead<-c("variableUID",setdiff(uniquefields,c("variableUID","datasource")))
selanimals<-c("Farming","Cattle","Dairy Cattle","Non-Dairy Cattle","Sheep","Swine","Poultry")

agri4capri<-unique(allagri[,selhead])
agri4capri[] <- lapply(agri4capri, as.character)
agri4capri$meta<-sapply(1:nrow(agri4capri),function(x) paste(as.character(agri4capri[x,setdiff(uniquefields,c("variableUID","datasource"))]),collapse=","))
agri4capri$meta<-gsub(",,",",*,",agri4capri$meta)
agri4capri$meta<-gsub(",,",",*,",agri4capri$meta)
agri4export<-agri4capri[,c("variableUID","meta")]

all4capri<-unique(alldata[,selhead])
all4capri[] <- lapply(all4capri, as.character)
all4capri$meta<-sapply(1:nrow(all4capri),function(x) paste(as.character(all4capri[x,setdiff(uniquefields,c("variableUID","datasource"))]),collapse=","))
all4capri$meta<-gsub(",,",",*,",all4capri$meta)
all4capri$meta<-gsub(",,",",*,",all4capri$meta)
all4export<-all4capri[,c("variableUID","meta")]


header<-paste0("*******************************************************************************")
header<-paste0(header,"\n$ontext")
header<-paste0(header,"\n\n   CAPRI project")
header<-paste0(header,"\n\n   GAMS-file   : set file created by eugirp_exportUIDs4capri.r")
header<-paste0(header,"\n\n   @purpose    : Maps CAPRI GHG inventory output with format of the NIRs via variableUIDs")
header<-paste0(header,"\n                 This file is part of the eugirp project and needs to run before running CAPRI - 'Map CAPRI to NIR'")
header<-paste0(header,"\n                 See https://github.com/aleip/eealocatorplots")
header<-paste0(header,"\n\n   @author     : Adrian Leip")
header<-paste0(header,"\n   @date       : 2018-04-30")
header<-paste0(header,"\n   @last update: ",format(Sys.time(), "%Y-%m-%d"))
header<-paste0(header,"\n   @basedon    : CAPRI_NIR_Sets.gms created by S. Marquardt 2016-02")
header<-paste0(header,"\n   @calledby   : eealocatorplots.r (step 9: FAO and CAPRI comparison)")
header<-paste0(header,"\n\n$offtext")
header<-paste0(header,"\n*******************************************************************************\n")



conf<-file("variableuid.set",open="wt")
writeLines(header,conf)
cong<-file("uid_to_ghg.set",open="wt")
writeLines(header,cong)

writeLines("******************************************************",conf)
writeLines("*  --- Define setglobals for further calculations ",conf)
writeLines("******************************************************",conf)
sel<-agri4capri$measure=="Nitrogen excretion rate"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines(paste0("$SETGLOBAL NrateCattle ",agri4capri$variableUID[sel&agri4capri$category=="Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NrateDairy ",agri4capri$variableUID[sel&agri4capri$category=="Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NrateNDairy ",agri4capri$variableUID[sel&agri4capri$category=="Non-Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NrateSheep ",agri4capri$variableUID[sel&agri4capri$category=="Sheep"]),conf)
writeLines(paste0("$SETGLOBAL NrateSwine ",agri4capri$variableUID[sel&agri4capri$category=="Swine"]),conf)

sel<-agri4capri$measure=="Total N excreted"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines(paste0("\n$SETGLOBAL NexCattle ",agri4capri$variableUID[sel&agri4capri$category=="Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NexDairy ",agri4capri$variableUID[sel&agri4capri$category=="Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NexNDairy ",agri4capri$variableUID[sel&agri4capri$category=="Non-Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL NexSheep ",agri4capri$variableUID[sel&agri4capri$category=="Sheep"]),conf)
writeLines(paste0("$SETGLOBAL NexSwine ",agri4capri$variableUID[sel&agri4capri$category=="Swine"]),conf)

sel<-agri4capri$measure=="Population"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.A",agri4capri$sector_number)
writeLines(paste0("\n$SETGLOBAL PopCattle ",agri4capri$variableUID[sel&agri4capri$category=="Cattle"]),conf)
writeLines(paste0("$SETGLOBAL PopDairy ",agri4capri$variableUID[sel&agri4capri$category=="Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL PopNDairy ",agri4capri$variableUID[sel&agri4capri$category=="Non-Dairy Cattle"]),conf)
writeLines(paste0("$SETGLOBAL PopSheep ",agri4capri$variableUID[sel&agri4capri$category=="Sheep"]),conf)
writeLines(paste0("$SETGLOBAL PopSwine ",agri4capri$variableUID[sel&agri4capri$category=="Swine"]),conf)

sel<-agri4capri$meastype=="AD"&agri4capri$gas%in%c("no gas")&grepl("3.D",agri4capri$sector_number)
writeLines(paste0("\n$SETGLOBAL NinSynfert ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.1.1"]),conf)
writeLines(paste0("$SETGLOBAL NinOrgfert ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.1.2"]),conf)
writeLines(paste0("$SETGLOBAL NinManure ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.1.2.a"]),conf)
writeLines(paste0("$SETGLOBAL NinGrazing ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.1.3"]),conf)
writeLines(paste0("$SETGLOBAL NinCropres ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.1.4"]),conf)
writeLines(paste0("$SETGLOBAL NinAtmdep ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.2.1"]),conf)
writeLines(paste0("$SETGLOBAL NinLeach ",agri4capri$variableUID[sel&agri4capri$sector_number=="3.D.2.2"]),conf)




selem<-agri4capri$meastype=="EM"
sel<-selem&agri4capri$gas=="Aggregate GHGs"
sel<-sel | selem&agri4capri$gas=="CO2"&agri4capri$sector_number%in%c("3.G.1","3.G.2","3.H","3.I")


writeLines("\n\nSET variableUID 'IPCC unique identifier in allagri'/\n",conf)
writeLines("SET UID_TO_GHG(variableUID,GHGs) 'Mapping of relevant UIDs for mapping between CAPRI and NIR'/\n",cong)
writeLines("******************************************************",conf)
writeLines("*  --- Emissions ",conf)
writeLines("******************************************************\n",conf)
writeLines("*  --- Emissions in kt CO2eq\n",conf)
writeLines("*  --- Emissions in kt CO2eq\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$sector_number=="3"],"\".(CH4ENT,CH4MAN,CH4RIC,CO2LIME,CO2UREA,N2OAPP,N2OGRA,N2OSYN,N2OHIS,N2OCRO,N2OLEA,N2OindMM,N2OindAS,N2OMAN)   \"",agri4export$meta[sel&agri4capri$sector_number=="3"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$sector_number=="3.A"],"\".(CH4ENT)   \"",agri4export$meta[sel&agri4capri$sector_number=="3.A"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$sector_number=="3.B"],"\".(CH4MAN,N2OMAN)   \"",agri4export$meta[sel&agri4capri$sector_number=="3.B"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$sector_number=="3.C"],"\".(CH4RIC)   \"",agri4export$meta[sel&agri4capri$sector_number=="3.C"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$sector_number=="3.D"],"\".(N2OSYN,N2OAPP,N2OGRA,N2OCRO,N2OHIS,N2OindAS,N2OLEA)   \"",agri4export$meta[sel&agri4capri$sector_number=="3.A"],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4","N2O","CO2")&agri4capri$sector_number=="3"
writeLines("\n*  --- Emissions in kt of gas\n",conf)
writeLines("\n*  --- Emissions in kt of gas\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$gas=="CH4"],"\".(CH4ENT,CH4MAN,CH4RIC)   \"",agri4export$meta[sel&agri4capri$gas=="CH4"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$gas=="N2O"],"\".(N2OMAN,N2OSYN,N2OAPP,N2OGRA,N2OCRO,N2OHIS,N2OindMM,N2OindAS,N2OLEA)   \"",agri4export$meta[sel&agri4capri$gas=="N2O"],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&agri4capri$gas=="CO2"],"\".(CO2LIME,CO2UREA)   \"",agri4export$meta[sel&agri4capri$gas=="CO2"],"\""),cong)




sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4")&agri4capri$category%in%selanimals&grepl("3.A",agri4capri$sector_number)
writeLines("\n*  --- Enteric Fermentation IPCC 3.A\n",conf)
writeLines("\n*  --- Enteric Fermentation IPCC 3.A\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CH4ENT)   \"",agri4export$meta[sel],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Manure Management CH4 IPCC 3.B.1\n",conf)
writeLines("\n*  --- Manure Management CH4 IPCC 3.B.1\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CH4MAN)   \"",agri4export$meta[sel],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Manure Management N2O IPCC 3.B.2\n",conf)
writeLines("\n*  --- Manure Management N2O IPCC 3.B.2\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
selxtra<-agri4capri$sector_number=="3.B.2.5"
writeLines(paste0("\"",agri4export$variableUID[sel&!selxtra],"\".(N2OMAN)   \"",agri4export$meta[sel&!selxtra],"\""),cong)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OindMM)   \"",agri4export$meta[sel&selxtra],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4")&agri4capri$category%in%selanimals&agri4capri$sector_number%in%c("3.C","3.C.1")
writeLines("\n*  --- Rice Cultivation CH4 IPCC 3.C\n",conf)
writeLines("\n*  --- Rice Cultivation CH4 IPCC 3.C\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CH4RIC)   \"",agri4export$meta[sel],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O")&grepl("3.D",agri4capri$sector_number)
writeLines("\n*  --- Agricultural Soils N2O IPCC 3.D\n",conf)
writeLines("\n*  --- Agricultural Soils N2O IPCC 3.D\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
selxtra<-agri4capri$sector_number=="3.D"
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OSYN,N2OAPP,N2OGRA,N2OCRO,N2OHIS,N2OindAS,N2OLEA)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-agri4capri$sector_number=="3.D.1"
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OSYN,N2OAPP,N2OGRA,N2OCRO,N2OHIS)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-agri4capri$sector_number=="3.D.1.1"
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OSYN)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.1.2",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OAPP)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.1.3",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OGRA)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.1.4",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OCRO)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.1.6",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OHIS)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.2$",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OindAS,N2OLEA)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.2.1",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OindAS)   \"",agri4export$meta[sel&selxtra],"\""),cong)
selxtra<-grepl("3.D.2.2",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel&selxtra],"\".(N2OLEA)   \"",agri4export$meta[sel&selxtra],"\""),cong)

selx<-agri4capri$measure=="Emissions"&grepl("3.F",agri4capri$sector_number)
writeLines("\n*  --- Field Burning IPCC 3.F\n",conf)
writeLines("\n*  --- Field Burning IPCC 3.F\n",cong)
sel<-selx&agri4capri$gas%in%c("CH4")
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CH4BUR)   \"",agri4export$meta[sel],"\""),cong)
sel<-selx&agri4capri$gas%in%c("N2O")
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(N2OBUR)   \"",agri4export$meta[sel],"\""),cong)


sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4","N2O","CO2")&grepl("3.G",agri4capri$sector_number)
writeLines("\n*  --- Liming IPCC 3.G\n",conf)
writeLines("\n*  --- Liming IPCC 3.G\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CO2LIME)   \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4","N2O","CO2")&grepl("3.H",agri4capri$sector_number)
writeLines("\n*  --- Urea Application IPCC 3.H\n",conf)
writeLines("\n*  --- Urea Application IPCC 3.H\n",cong)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(CO2UREA)   \"",agri4export$meta[sel],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.J",agri4capri$sector_number)
writeLines("\n*  --- Other agriculture emissions IPCC 3.J\n",conf)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")


writeLines("\n\n\n******************************************************",conf)
writeLines("*  --- N excretion by animals ",conf)
writeLines("******************************************************\n",conf)

sel<-agri4capri$measure=="Total N excreted"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Total N excretion IPCC 3.B.2\n",conf)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

sel<-agri4capri$measure=="Nitrogen excretion rate"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Nitrogen excretion rate IPCC 3.B.2\n",conf)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

sel<-agri4capri$measure=="Nitrogen excretion per MMS"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Nitrogen excretion per MMS IPCC 3.B.2\n",conf)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

writeLines("\n\n\n******************************************************",conf)
writeLines("*  --- N input to soils ",conf)
writeLines("******************************************************\n",conf)
sel<-agri4capri$meastype=="AD"&agri4capri$gas%in%c("no gas")&grepl("3.D",agri4capri$sector_number)
sel<-sel&!grepl("3.D.1.2.[bc]",agri4capri$sector_number)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

writeLines("\n\n\n******************************************************",conf)
writeLines("*  --- Populations ",conf)
writeLines("******************************************************\n",conf)
sel<-agri4capri$meastype=="POP"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.A",agri4capri$sector_number)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
sel<-agri4capri$meastype=="POP"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B.1",agri4capri$sector_number)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")
sel<-agri4capri$meastype=="POP"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B.2",agri4capri$sector_number)
write.table(agri4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")

writeLines("\n\n\n******************************************************",conf)
writeLines("*  --- LULUCF emissions ",conf)
writeLines("******************************************************\n",conf)
selcats<-c("Carbon stock change","Wildfires","Direct N2O Emissions from N inputs","Direct N2O Emissions from N Mineralization/Immobilization")
sel<-all4capri$meastype=="EM"&all4capri$gas%in%c("N2O","CH4","CO2")&grepl("4.[ABCDEF].[12D]",all4capri$sector_number)&all4capri$category%in%selcats
write.table(all4export[sel,],file=conf,quote=TRUE,row.names=FALSE,col.names=FALSE,sep="    ")



writeLines("/;",conf)
writeLines("/;",cong)
close(conf)

writeLines("\n\nSET UID_TO_MPACT(variableUID,MPACT) 'Mapping of relevant UIDs for mapping between CAPRI and NIR'/\n",cong)


writeLines("\n*  --- Farming\n",cong)
sx<-agri4capri$measure=="Emissions"&agri4capri$category=="Farming"&agri4capri$gas%in%c("CH4","N2O")
sx<-sx&!grepl("3.[D|1|E|C]",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(SET.MPACT)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Cattle\n",cong)
se<-agri4capri$measure%in%c("Emissions","Total N excreted","Nitrogen excretion rate","Nitrogen excretion per MMS","Population")&agri4capri$gas%in%c("CH4","N2O","no gas")
sx<-se&agri4capri$category=="Cattle"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(DCOL,DCOH,BULL,BULH,HEIL,HEIH,SCOW,HEIR,CAMF,CAFF,CAMR,CAFR)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Dairy Cattle\n",cong)
sx<-se&agri4capri$category=="Dairy Cattle"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(DCOL,DCOH)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Non-Dairy Cattle\n",cong)
sx<-se&agri4capri$category=="Non-Dairy Cattle"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(BULL,BULH,HEIL,HEIH,SCOW,HEIR,CAMF,CAFF,CAMR,CAFR)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Sheep and Goats - Note that Goats are separately!!\n",cong)
sx<-se&agri4capri$category=="Sheep"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(SHGM,SHGF)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Swine\n",cong)
sx<-se&agri4capri$category=="Swine"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(PIGF,SOWS)   \"",agri4export$meta[sx],"\""),cong)
writeLines("\n*  --- Poultry\n",cong)
sx<-se&agri4capri$category=="Poultry"
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(HENS,POUF)   \"",agri4export$meta[sx],"\""),cong)

sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CH4")&agri4capri$category%in%selanimals&agri4capri$sector_number%in%c("3.C","3.C.1")
writeLines("\n*  --- Rice Cultivation CH4 IPCC 3.C\n",cong)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(PARI)   \"",agri4export$meta[sel],"\""),cong)

writeLines("\n*  --- Agricultural Soils N2O IPCC 3.D\n",cong)
sx<-agri4capri$measure=="Emissions"&grepl("3.D",agri4capri$sector_number)&agri4capri$gas%in%c("CH4","N2O")
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(SET.MPACT)   \"",agri4export$meta[sx],"\""),cong)

sel<-agri4capri$meastype=="AD"&agri4capri$gas%in%c("no gas")&grepl("3.D",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(SET.MCACT)   \"",agri4export$meta[sel],"\""),cong)


writeLines("\n*  --- Field Burning IPCC 3.F\n",cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.1.1",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(SWHE,DWHE)           \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.1.2",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(BARL)                \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.1.3",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(MAIZ,MAIF)           \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.1.4",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(PARI,OCER,RYEM,OATS) \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.2",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(PULS)                \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("N2O","CH4")&grepl("3.F.3",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(POTA,SUGB)           \"",agri4export$meta[sel],"\""),cong)


writeLines("/;",cong)


writeLines("\n\nSET UID_TO_COLS(variableUID,DB_COLS)/",cong)
sel<-agri4capri$meastype=="EM"&agri4capri$gas=="Aggregate GHGs"
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(ImpactGWP)   \"",agri4export$meta[sel],"\""),cong)
sel<-agri4capri$measure=="Emissions"&agri4capri$gas%in%c("CO2")&agri4capri$sector_number%in%c("3","3.G","3.H")
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(UAAR)   \"",agri4export$meta[sel],"\""),cong)
writeLines("/;",cong)



writeLines("\n\nSET UID_TO_ROWS(variableUID,DB_ROWS)/",cong)


sel<-agri4capri$measure=="Total N excreted"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Total N excretion IPCC 3.B.2\n",cong)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(MANN)   \"",agri4export$meta[sel],"\""),cong)
writeLines("\n*  --- Populations \n",cong)
sel<-agri4capri$meastype=="POP"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.[AB]",agri4capri$sector_number)
writeLines(paste0("\"",agri4export$variableUID[sel],"\".(HERD)   \"",agri4export$meta[sel],"\""),cong)

writeLines("/;",cong)

writeLines("\n\nSET UID_TO_ALLTYPE(variableUID,ALLTYPE)/",cong)
sel<-agri4capri$measure=="Nitrogen excretion per MMS"&agri4capri$gas%in%c("no gas")&agri4capri$category%in%selanimals&grepl("3.B",agri4capri$sector_number)
writeLines("\n*  --- Nitrogen excretion per MMS - Liquid Systems\n",cong)
sx<-sel&grepl("Liquid|lagoon",agri4capri$source)
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(Liquid)   \"",agri4export$meta[sx],"\""),cong)

writeLines("\n*  --- Nitrogen excretion per MMS - Solid Systems\n",cong)
sx<-sel&grepl("Solid|Digesters|Composting|Daily|Burned|Other",agri4capri$source)
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(Solid)   \"",agri4export$meta[sx],"\""),cong)

writeLines("\n*  --- Nitrogen excretion per MMS - Grazing Systems\n",cong)
sx<-sel&grepl("Pasture",agri4capri$source)
writeLines(paste0("\"",agri4export$variableUID[sx],"\".(GRAZ)   \"",agri4export$meta[sx],"\""),cong)
writeLines("/;",cong)

close(cong)

# saving to Google Drive
if(nrow(drive_find(paste0("eealocatorplots/", cursubm, "/uid_to_ghg.set"))) == 0){
  drive_upload(media = "D:/dev/ghginventory/eealocatorplots/uid_to_ghg.set", 
            # Alex 2020-03-05   path = as_dribble(paste0("eealocatorplots/", cursubm, "/")),  
               path = as_dribble(paste0("eugirp/", cursubm, "/")), 
               #name = NULL, type = NULL, 
               verbose = FALSE)
  drive_upload(media = "D:/dev/ghginventory/eealocatorplots/variableuid.set", 
            # Alex 2020-03-05   path = as_dribble(paste0("eealocatorplots/", cursubm, "/")), 
               path = as_dribble(paste0("eugirp/", cursubm, "/")), 
               #name = NULL, type = NULL, 
               verbose = FALSE)
}else{
  drive_update(file = paste0("eealocatorplots/", cursubm, "/", "uid_to_ghg.set"),
               media = "D:/dev/ghginventory/eealocatorplots/uid_to_ghg.set", 
               verbose = FALSE)
  drive_update(file = paste0("eealocatorplots/", cursubm, "/", "variableuid.set"), 
               media = "D:/dev/ghginventory/eealocatorplots/variableuid.set", 
               verbose = FALSE)
}


