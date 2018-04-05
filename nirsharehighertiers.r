selsec<-"3.A.1|3.A.2|3.A.3|3.A.4|3.B.1|3.B.2|3.D.1|3.D.2"
#selcla<-"Cattle|Dairy Cattle|Non-Dairy Cattle|Other Sheep|Other Swine|Other livestock|Farming|Direct N2O Emissions From Managed Soils|Urine and Dung Deposited by Grazing Animals"
selcla<-"Cattle|Other Cattle|Dairy Cattle|Growing Cattle|Mature Dairy Cattle|Other Mature Cattle|Other Sheep|Other Swine|Other livestock|Farming|Direct N2O Emissions From Managed Soils"
remcol<-c("variableUID","measure","submission_version","submission_year","unit","classification","value")

keytable<-agrimethods[grepl(selsec,agrimethods$sector_number) & grepl(selcla,agrimethods$category),!names(agrimethods)%in%remcol]
#keytable<-agrimethods[grepl(selsec,agrimethods$sector_number),!names(agrimethods)%in%remcol]
keytable$source<-gsub("no source","",keytable$source)
keytable$option<-gsub("no option","",keytable$option)
keytable$method<-gsub("no method","",keytable$method)
keytable$target<-gsub("no target","",keytable$target)
keytable$type<-gsub("no type","",keytable$type)



keytable$sector_number<-sapply(1:nrow(keytable),function(x) gsub(paste0(" ",keytable$category[x]),"",keytable$sector_number[x]))
keytable$sector_number<-gsub(" Other \\(please specify\\)","",keytable$sector_number)
keytable$category<-gsub("Other Sheep","Sheep",keytable$category)
keytable$category<-gsub("Other Swine","Swine",keytable$category)
keytable$category<-gsub("Other livestock","Other Livestock",keytable$category)

#keytable<-keytable[!keytable$category%in%c("Mature Dairy Cattle"),]
keytable<-keytable[!keytable$sector_number%in%c("3.A.3",
                                                #"3.A.4",
                                                #"3.B.1",
                                                "3.B.1.1","3.B.1.2","3.B.1.3","3.B.1.4",
                                                #"3.B.2",
                                                "3.B.2.1","3.B.2.2","3.B.2.3","3.B.2.4"
                                                #"3.D.1",
                                                #"3.D.1.1","3.D.1.2","3.D.1.3",
                                                #"3.D.2"
                                                #"3.D.2.1","3.D.2.2"
                                                ),]

remcol<-c("variableUID","measure","unit","datasource",years[!years%in%c(firstyear,lastyear)],"notation","meastype","classification")
agrtable<-allagri[allagri$meastype=="EM",!names(agriemissions)%in%remcol]
#agrtable$category<-gsub("Other livestock","Other Livestock",agrtable$category)
mergecol<-c("sector_number","category","party","gas","source","method","target","type")
keytable<-merge(keytable,agrtable,by=mergecol,all.x=TRUE)

keytable$high<-sapply(1:nrow(keytable),function(x) grepl("T2|T3|CS",keytable$notation[x]))
keytable$highfirst<-keytable$high*keytable[,firstyear]
keytable$highlast<-keytable$high*keytable[,lastyear]

keyaggr<-aggregate(keytable[,names(keytable)%in%c(firstyear,lastyear,"highfirst","highlast")],
                   by=list(sector_number=keytable$sector_number,
                           category=keytable$category,
                           gas=keytable$gas,
                           source=keytable$source,
                           target=keytable$target,
                           method=keytable$method,
                           type=keytable$type),
                   sum,na.rm=TRUE)
keyaggr$highfirst<-keyaggr$highfirst/keyaggr[,firstyear]
keyaggr$highlast<-keyaggr$highlast/keyaggr[,lastyear]
keyaggr<-keyaggr[!grepl("ption",keyaggr$sector_number),]

# Other cattle does not exist. No information on methods and no emissions for the 'other cattle' aggregate
keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category=="Other Cattle",firstyear]<-keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category=="Cattle",firstyear]-sum(keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category!="Cattle",firstyear],na.rm=TRUE)
keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category=="Other Cattle",lastyear]<-keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category=="Cattle",firstyear]-sum(keyaggr[keyaggr$sector_number=="3.A.1"&keyaggr$category!="Cattle",lastyear],na.rm=TRUE)
keyaggr<-keyaggr[keyaggr$category!="Cattle",]

write.csv(keyaggr,file=paste0(locplots,"/../ecir/checks/keycategories/keyaggr.csv"))

