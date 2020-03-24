
#if(!is.null(keepNORout)) alldata <- rbind(alldata, alldata_NOR) 
if(! dir.exists(paste0(plotsdir,"/",cursubm))){dir.create(paste0(plotsdir,"/",cursubm))}
if(! dir.exists(paste0(plotsdir,"/",cursubm, "/mixplots"))){dir.create(paste0(plotsdir,"/",cursubm, "/mixplots"))}

agriselect<-grepl("^3",alldata$sector_number) 
allagri<-alldata[agriselect,]
acountry<-curcountries[variable==eusubm & value==1]$code3
o<-order(allagri$sector_number,allagri$category)
allagri<-allagri[o,]
#allagri[,years]<-apply(allagri[,years],2,function(x) as.numeric(x))

#xavi20180219: allagri<-eu28sums(A = allagri,years = years)    #xavi20180219: already calculated sooner for EUC (I think EUA is not necessary)
agriselect<-grepl("^3",allmethods$sector_number) 
agrimethods<-allmethods[agriselect,]
o<-order(agrimethods$sector_number,agrimethods$category)
agrimethods<-agrimethods[o,]


select<-grepl("\\.$",allagri$sector_number)
allagri$sector_number[select]<-(gsub("\\.$","",allagri$sector_number[select]))

agriswine<-(grepl("^3.A.3",allagri$sector_number) & allagri$category!="Swine") |
    (grepl("^3.B.[12].3",allagri$sector_number) & allagri$category!="Swine")
agrisheep<-(grepl("^3.A.2",allagri$sector_number) & allagri$category!="Sheep") |
    (grepl("^3.B.[12].2",allagri$sector_number) & allagri$category!="Sheep")
agricattle<-(grepl("^3.A.1",allagri$sector_number) & (allagri$category!="Cattle" | grepl("Option",allagri$sector_number))) |
    (grepl("^3.B.[12].1",allagri$sector_number) & (allagri$category!="Cattle" | grepl("Option",allagri$sector_number)))
agridairyA<-(grepl("^3.A.1",allagri$sector_number) & allagri$category=="Dairy Cattle" & ! grepl("Option",allagri$sector_number))
agridairyB<-(grepl("^3.B.[12].1",allagri$sector_number) & allagri$category=="Dairy Cattle" & ! grepl("Option",allagri$sector_number))
agrindairyA<-(grepl("^3.A.1",allagri$sector_number) & allagri$category=="Non-Dairy Cattle" & ! grepl("Option",allagri$sector_number))
agrindairyB<-(grepl("^3.B.[12].1",allagri$sector_number) & allagri$category=="Non-Dairy Cattle" & ! grepl("Option",allagri$sector_number))

cleanani<-!(agriswine | agrisheep | agricattle)
noagg<-allagri$gas!="Aggregate GHGs" & ! grepl("^3.1",allagri$sector_number)

d1<-nchar(allagri$sector_number)-nchar(gsub("\\.","",allagri$sector_number))==0
agrid1<-allagri[d1 & noagg,]
agrid1<-agrid1[order(agrid1$sector_number),]
d2<-nchar(allagri$sector_number)-nchar(gsub("\\.","",allagri$sector_number))==1
agrid2<-allagri[d2 & noagg & cleanani,]
agrid2<-agrid2[order(agrid2$sector_number),]

d3<-nchar(allagri$sector_number)-nchar(gsub("\\.","",allagri$sector_number))==2 
agrid3<-allagri[d3 & noagg & cleanani,]
agrid3<-agrid3[order(agrid3$sector_number),]

d4<-nchar(allagri$sector_number)-nchar(gsub("\\.","",allagri$sector_number))==3 & allagri$sector_number!="3.B.2.5 N2O Emissions per MMS"
d4<-d4 & noagg & cleanani & !(allagri$sector_number=="3.B.2.5" & allagri$meastype=="EM" & allagri$measure!="Emissions")
agrid4<-allagri[d4 | agridairyA | agrindairyA,]
agrid4<-agrid4[order(agrid4$sector_number),]

d5<-nchar(allagri$sector_number)-nchar(gsub("\\.","",allagri$sector_number))==4 
d5<-d5 & noagg & cleanani
agrid5<-allagri[d5| agridairyB | agrindairyB,]
agrid5<-agrid5[order(agrid5$sector_number),]

agrigen<-agrid2[grepl("3.[ACEFGHIi]",agrid2$sector_number),]
agrigen<-rbind(agrigen,agrid3[grepl("3.[BD]",agrid3$sector_number),])
agrigen<-agrigen[order(agrigen$sector_number),]
agrigen<-agrigen[agrigen$party%in%acountry,]

agrimix<-agrid3[grepl("3.[ACEFGHIi]",agrid3$sector_number)&agrid3$sector_number!="3.A.1",]
agrimix<-rbind(agrimix,agrid4[grepl("3.[BD]",agrid4$sector_number)&!grepl("3.B.[12].1",agrid4$sector_number),])
agrimix<-rbind(agrimix,agrid4[grepl("3.A.1",agrid4$sector_number),])
agrimix<-rbind(agrimix,agrid5[grepl("3.B.[12].1",agrid5$sector_number),])
agrimix<-rbind(agrimix,agrid2[grepl("3.[HIi]",agrid2$sector_number),])
agrimix<-agrimix[order(agrimix$sector_number),]
agrimix$sector_number[agrimix$category=="Dairy Cattle"]<-paste0(agrimix$sector_number[agrimix$category=="Dairy Cattle"],".1")
agrimix$sector_number[agrimix$category=="Non-Dairy Cattle"]<-paste0(agrimix$sector_number[agrimix$category=="Non-Dairy Cattle"],".2")
agrimix<-agrimix[agrimix$party%in%acountry,]

agridet<-agrid4[grepl("3.[AEGHIi]",agrid4$sector_number),]
agridet<-rbind(agridet,agrid3[grepl("3.A.[23]",agrid3$sector_number),])
agridet<-rbind(agridet,agrid5[grepl("3.[BD]",agrid5$sector_number),])
agridet<-rbind(agridet,agrid4[grepl("3.D",agrid4$sector_number),])
agridet<-agridet[agridet$sector_number!="3.D.1.2",]
agridet<-rbind(agridet,agrid4[grepl("3.B.[12].[23]",agrid4$sector_number),])
agridet<-rbind(agridet,agrid4[grepl("3.C.1",agrid4$sector_number),])
agridet<-rbind(agridet,agrid3[grepl("3.C.[2-9]",agrid3$sector_number),])
agridet<-rbind(agridet,agrid4[grepl("3.F.1",agrid4$sector_number),])
agridet<-rbind(agridet,agrid3[grepl("3.F.[2-9]",agrid3$sector_number),])
agridet<-rbind(agridet,agrid2[grepl("3.[HIi]",agrid2$sector_number),])
agridet<-agridet[order(agridet$sector_number),]
agridet$sector_number[agridet$category=="Dairy Cattle"]<-paste0(agridet$sector_number[agridet$category=="Dairy Cattle"],".1")
agridet$sector_number[agridet$category=="Non-Dairy Cattle"]<-paste0(agridet$sector_number[agridet$category=="Non-Dairy Cattle"],".2")
agridet<-agridet[agridet$party%in%acountry,]

agriemissions<-dt2CO2eq(allagri[allagri$meastype=="EM"&allagri$gas!="no gas"&noagg,])
agriemissions <- agriemissions[party %in% acountry]

agrigeneu<-dt2CO2eq(agrigen[party==eusubm&meastype=="EM"&gas!="no gas",])
agrimixeu<-dt2CO2eq(agrimix[party==eusubm&meastype=="EM"&gas!="no gas",])
agrideteu<-dt2CO2eq(agridet[party==eusubm&meastype=="EM"&gas!="no gas",])

lastyear<-years[length(years)]
agrigeneu<-agrigeneu[order(agrigeneu[,lastyear, with=FALSE],decreasing=TRUE),]
agrimixeu<-agrimixeu[order(agrimixeu[,lastyear, with=FALSE],decreasing=TRUE),]
agrideteu<-agrideteu[order(agrideteu[,lastyear, with=FALSE],decreasing=TRUE),]

v<-agrigeneu$sector_number
sel<-agrimixeu$category=="Farming"
agrimixeu$category[sel]<-paste(agrimixeu$classification[sel],agrimixeu$target[sel]," ")
x<-lapply(c(1:(length(v)-1)),function(x) makepie(piedata = agrimixeu,pieradius = 0.9,piename = "agrimixeu",piegrep = agrigeneu$sector_number[x]))
#makepie(agrigeneu,0.9,"agrigeneu","")
#View(agrid1);View(agrid2);View(agrid3);View(agrid4);View(agrid5)


emissionshareplot(sec = "3",DF = agrigen)
emissionshareplot(sec = "3.A",DF = agrimix)
emissionshareplot(sec = "3.B.1",DF = agrimix)
emissionshareplot(sec = "3.B.2",DF = agrimix)
emissionshareplot(sec = "3.B.2.5",DF = agrimix)
emissionshareplot(sec = "3.D.1",DF = agrimix)
emissionshareplot(sec = "3.D.2",DF = agrimix)


# Export file for comparison with CAPRI
agridetcapri<-agridet[agridet$meastype=="EM",]
agridetcapri<-agridetcapri[!grepl("Buffalo|Mules|Deer|Horses|Other",agridetcapri$category),]
agridetcapri<-agridetcapri[!agridetcapri$party=="EUC",]
agridetcapri<-agridetcapri[agridetcapri$gas%in%c("CH4","N2O","CO2"),]
agridet3bind<-allagri[grepl("3.B.2.5",allagri$sector_number),]
agridet3bind<-agridet3bind[agridet3bind$measure!="Emissions",]
agridet3bind<-agridet3bind[agridet3bind$meastype=="EM",]
agridet3bind<-agridet3bind[grepl("Indirect",agridet3bind$classification),]

#con<-file(paste0(invloc,"\\eealocator\\agridet_emissions4capri",format(Sys.time(), "%Y%m%d"),".csv"),open = "wt") # Alex 20200128 new name of the eealocator directory
f1 <- paste0(invloc,"\\tables4eu\\agridet_emissions4capri",format(Sys.time(), "%Y%m%d"),".xlsx")
f2 <- paste0(invloc,"\\tables4eu\\agridet_emissions4capri_", cursubm,".xlsx")
wb <- createWorkbook(creator = eugirp.fullname)
wbs <- addWorksheet(wb, sheetName = "agridet_emissions")
wbs <- writeData(wb, sheet = "agridet_emissions", x = "# Data Source: EU-GIRP: GHG emissions by source category (detailed - agridet)", startRow = 1)
wbs <- writeData(wb, sheet = "agridet_emissions", x = "# Processing: animal types NOT reported: Buffalo|Mules|Deer|Horses|Other", startRow = 2)
wbs <- writeData(wb, sheet = "agridet_emissions", x = paste0("# Data from submission: ",cursubm), startRow = 3)
wbs <- writeData(wb, sheet = "agridet_emissions", x = paste0("# Countries included: ",paste(unique(agridetcapri$party),collapse="-")), startRow = 4)
wbs <- writeData(wb, sheet = "agridet_emissions", x = rbind(agridetcapri,agridet3bind), startRow = 5)
saveWorkbook(wb, file = f1, overwrite = TRUE)
file.copy(from = f1, to = f2, overwrite = TRUE)
drive_upload(media = f2, path = paste0("eugirp/", cursubm, "/"), overwrite = TRUE, verbose = TRUE)

# removing again NOR data
if(!is.null(keepNORout)) alldata <- alldata[alldata$party != "NOR",] 
