selyear<-"2012"
sel<-c("gas","sector_number","party",selyear)
cats<-unique(agrigen[,c("sector_number","gas")])
cats<-cats[cats$gas!="no gas",]
cats$gas<-as.character(cats$gas)
cats<-cats[!grepl("land",cats$sector_number),]

agriexp<-agrigen%>%filter(sector_number==cats$sector_number[1],gas==cats$gas[1],meastype=="EM")
agriexp<-agriexp[,c("party",selyear)]
names(agriexp)<-c("party",paste(as.character(cats[1,]),collapse="."))

for(i in c(2:nrow(cats))){
    temp<-agrigen%>%filter(sector_number==cats$sector_number[i],gas==cats$gas[i],meastype=="EM")
    temp<-temp[,c("party",selyear)]
    names(temp)<-c("party",paste(as.character(cats[i,]),collapse="."))
    agriexp<-merge(agriexp,temp,by="party",all = TRUE)
}

curn<-names(agriexp[names(agriexp)!="party"])
sel<-grepl("CH4",names(agriexp))
agriexp[,"3.CH4"]<-apply(agriexp[,sel],1,sum,na.rm=TRUE)
sel<-grepl("N2O",names(agriexp))
agriexp[,"3.N2O"]<-apply(agriexp[,sel],1,sum,na.rm=TRUE)
sel<-grepl("CO2",names(agriexp))
agriexp[,"3.CO2"]<-apply(agriexp[,sel],1,sum,na.rm=TRUE)

agriexp[,"3.GHG"]<-gwps[1]*agriexp[,"3.CH4"]+gwps[3]*agriexp[,"3.N2O"]+gwps[2]*agriexp[,"3.CO2"]
curn<-c(curn,"3.D.N2O")
agriexp[,"3.D.N2O"]<-agriexp[,"3.D.1.N2O"]+agriexp[,"3.D.2.N2O"]

agriexp<-agriexp[,c("party","3.GHG","3.CH4","3.N2O","3.CO2",curn[order(curn)])]

write.csv(agriexp,file="agrigen2012.csv")
