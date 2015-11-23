curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea,]
if(cursec=="3.B.2.5")curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea&grepl("Atmospheric",allagri$measure),]
if(curcat%in%c("Cattle","Dairy Cattle","Non-Dairy Cattle","Sheep","Swine"))curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea&allagri$category==curcat,]
curemissions[is.nan(curemissions)]<-NA

# Exclude some national data with wrong unit
if(curmea=="Milk") curemissions[curemissions$party=="LU",years]<-NA

#euemissions<-filter(curemissions,party=="EU28")
euemissions<-curemissions[curemissions$party=="EU28",]
curgas<-as.character(euemissions$gas)
if(curgas=="no gas"){if(grepl("3.A|3.B.1|3.C",cursec)){curgas<-"CH4"}else if(grepl("3.D|3.B.2",cursec)){curgas<-"N2O"}}
curmeasure<-as.character(euemissions$measure)

curunit<-as.character(euemissions$unit)
if(curmea=="Milk") curunit<-"kg/head/day"

curclas<-as.character(euemissions$classification)

# Names of sector and category
# cursect: numerical sector (e.g. 3.A.1)
# curseclong: term for sector (e.g. Enteric Fermenation: this is defined not obtained from data)
# curcat: category if category if 'Farming' then it will be curseclong
# curcattext: combination of sector and category (e.g. 3.A.1 Cattle)
if(curcat=="Farming"){curcat<-curseclong}
curcattext<-paste0(cursec," - ",curcat)

if(curmeasure=="Atmospheric deposition"){
    curcattext<-paste0(cursec," - ",curmeasure," from ",curcat)
    if(curmea=="IEF")curmeasure<-"Implied emission factor"
}
curmeasure<-gsub(" \\(average\\)","",curmeasure)
if(cursec=="3.D.1.1" | cursec=="3.D.1.2"){
    curcattext<-gsub("Managed Soils",curseclong,curcattext)
}
if(cursec=="3.D.2.1" | cursec=="3.D.2.2"){
    curcattext<-paste0(cursec," - Indirect N2O Emissions From ",curseclong)
}
if(grepl("3.D.AI.1",cursec)){curcattext<-"3.D.2.1 - Indirect emissions from atmospheric deposition"}
if(grepl("3.D.AI.2",cursec)){curcattext<-"3.D.2.2 - Indirect emissions from atmospheric deposition"}
if(grepl("3.D.AI.3",cursec)){curcattext<-"3.D.2.2 - Indirect emissions from nitrogen leaching and run-off"}

curtrend<-euemissions[lastyear]/euemissions[firstyear]
curtrendabs<-euemissions[lastyear]-euemissions[firstyear]
lasttrend<-euemissions[lastyear]/euemissions[lastyear2]

eusel<-curemissions$party=="EU28"
alltrend<-cbind(curemissions[!eusel,],curemissions[!eusel,lastyear]/curemissions[!eusel,firstyear])
alltrend<-cbind(alltrend,alltrend[,lastyear]-alltrend[,firstyear])
names(alltrend)<-c(names(curemissions),"trend","diff")
alltrend<-alltrend[order(alltrend$diff),]
alltrend<-arrange(alltrend,diff)

allshare<-subset(alltrend,select=lastyear)
allshare$share<-alltrend[,lastyear]/sum(alltrend[,lastyear],na.rm=TRUE)
allshare$median<-alltrend[,lastyear]/median(alltrend[,lastyear],na.rm=TRUE)
#allshare<-allshare[order(allshare$share,decreasing = TRUE),]
allshare<-arrange(allshare,desc(median))
nshare<-min(10,nrow(allshare))

decrease<-order(alltrend$trend,decreasing = FALSE)   # gives order of countries in relativ decrease
decreaseabs<-order(alltrend$diff,decreasing = FALSE) # gives order of countries in absolute decrease
decreasedata<-alltrend%>%filter(!is.na(trend))%>%filter(diff<0)%>%arrange((trend))
decreasen<-nrow(decreasedata)                     # number of countries with decreasing trend
decreasetot<-sum(decreasedata$diff)     # total decrease in countries with decreasing trend
decreasemed<-median(decreasedata$diff)     # total decrease in countries with decreasing trend
decreasemean<-mean(decreasedata$diff)     # total decrease in countries with decreasing trend

increase<-order(alltrend$trend,decreasing = FALSE)
increaseabs<-order(alltrend$diff,decreasing = FALSE)
increasedata<-alltrend%>%filter(!is.na(trend))%>%filter(diff>0)%>%arrange(desc(trend))
increasen<-nrow(increasedata)
increasetot<-sum(increasedata$diff)
increasemed<-median(increasedata$diff)
increasemean<-mean(increasedata$diff)

if(curmea%in%meas2popweight){
    decreasecnt<-decreasedata$party[decreasedata$trend<0.75]
    decreasencnt<-length(decreasecnt)                          #Number of countries contributing with at least 10% to EU trend
    if(decreasencnt==0){
        decreasencnt<-min(3,nrow(decreasedata))
        decreasecnt<-decreasedata$party[1:min(3,decreasen)]
    }
    if(decreasencnt>4){
        decreasencnt<-4
        decreasecnt<-increasedata$party[1:min(3,increasen)]
    }
    decreasemcnt<-mean(alltrend$diff[alltrend$party%in%decreasecnt])
    
    increasecnt<-increasedata$party[increasedata$trend>1.25]
    increasencnt<-length(increasecnt)                          #Number of countries contributing with at least 10% to EU trend
    if(increasencnt==0){
        increasencnt<-min(3,increasen)
        increasecnt<-increasedata$party[1:min(3,increasen)]
    }
    if(increasencnt>4){
        increasencnt<-4
        increasecnt<-increasedata$party[1:min(4,increasen)]
    }
    increasemcnt<-mean(alltrend$diff[alltrend$party%in%increasecnt])
    
}else{
    decreasecnt<-alltrend$party[alltrend$diff/decreasetot>0.1] #Those countries contributing with at least 10% to EU trend
    decreasencnt<-length(decreasecnt)                          #Number of countries contributing with at least 10% to EU trend
    decreasescnt<-sum(alltrend$diff[alltrend$party%in%decreasecnt])
    increasecnt<-alltrend$party[alltrend$diff/increasetot>0.1] #Those countries contributing with at least 10% to EU trend
    increasencnt<-length(increasecnt)                          #Number of countries contributing with at least 10% to EU trend
    increasescnt<-sum(alltrend$diff[alltrend$party%in%increasecnt])

    trendtot<-increasetot+decreasetot
}

stablen<-sum(alltrend$trend==1,na.rm=TRUE)
missing<-sum(is.nan(alltrend$trend))+length(countriesnoeu)-nrow(alltrend)

# Exclude some national data with wrong unit
panderOptions('missing'," - ")
panderOptions('table.alignment.default', c('left','right','right','right','left','right','right'))


selyears<-c(firstyear,lastyear)
curtable<-(curemissions%>%select(party,one_of(selyears)))
curtable$party<-unlist(lapply(c(1:nrow(curtable)), function(x) countriesl[which(countries2==curtable$party[x])]))
curtable[,selyears]<-format(curtable[,selyears],digits=2)
sel<-curtable$party=="EU28"
curtable[curtable$party=="EU28",]<-paste0("**",gsub(" ","",curtable[curtable$party=="EU28",]),"**")
if(grepl("NA",curtable[sel,firstyear])&grepl("NA",curtable[sel,lastyear])) curtable<-curtable[!sel,]
for(yy in selyears){
    curtable[,yy]<-unlist(lapply(c(1:nrow(curtable)),function(x) if(grepl("NA",curtable[x,yy])){"&#09;"}else{curtable[x,yy]}))
}
rownames(curtable)<-NULL
names(curtable)<-c("Member State",selyears)


