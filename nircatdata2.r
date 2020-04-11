curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea,]
if(cursec=="3.B.2.5")curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea&grepl(curmeasure,allagri$measure),]
if(curcat%in%c("Cattle","Dairy Cattle","Non-Dairy Cattle","Sheep","Swine")){
  curemissions<-allagri[allagri$sector_number==cursec&allagri$meastype==curmea&allagri$category==curcat,]
}
curemissions_GBE <- curemissions[curemissions$party %in% c("GBE"),]
curemissions<-curemissions[curemissions$party%in%acountry,]
curemissions[is.nan(curemissions)]<-NA
curemissions <- as.data.frame(curemissions)
# Exclude some national data with wrong unit
if(curmea=="Milk") curemissions[curemissions$party=="LUX",years]<-NA

#euemissions<-filter(curemissions,party==eusubm)
euemissions<-as.data.frame(curemissions[curemissions$party==eusubm,])
curgas<-as.character(euemissions$gas)
if(curgas=="no gas"){if(grepl("3.A|3.B.1|3.C",cursec)){curgas<-"CH4"}else if(grepl("3.D|3.B.2",cursec)){curgas<-"N2O"}}
curcat<-curcatnew(curcat)
curcattext<-curcatlong(curcat,cursec)
curmeasure<-curmeasurenew(as.character(euemissions$measure))
if(grepl("Fraction of synthetic fertilizer N ",curmeasure) ||
   grepl("raction of livestock N excretion",curmeasure) ||
   grepl("Fraction of N input to managed soils",curmeasure)) curmeasure<-curmea
curunit<-as.character(euemissions$unit)
if(curmea=="Milk") curunit<-"kg/head/day"
curclas<-as.character(euemissions$classification)


curtrend<-euemissions[lastyear]/euemissions[firstyear]
curtrendabs<-euemissions[lastyear]-euemissions[firstyear]
lasttrend<-euemissions[lastyear]/euemissions[lastyear2]

eusel<-curemissions$party==eusubm
alltrend<-cbind(curemissions[!eusel,],
                curemissions[!eusel,lastyear]/curemissions[!eusel,firstyear])
names(alltrend)<-c(names(curemissions),"trend")
sel<-alltrend$trend==0 & !is.na(alltrend$trend)
if(sum(sel)>0)alltrend[sel,lastyear]<-alltrend[alltrend$trend==0,years[nyears-1]]
alltrend$diff<-alltrend[,lastyear]-alltrend[,firstyear]
alltrend<-alltrend[order(alltrend$diff),]
#al202004 - arrange library not loaded. what does this do?
#alltrend<-arrange(alltrend,diff)
alltrend<-alltrend[!is.infinite(alltrend$trend),]

allshare<-subset(alltrend,select=lastyear)
allshare$share<-alltrend[,lastyear]/sum(alltrend[,lastyear],na.rm=TRUE)
allshare$median<-alltrend[,lastyear]/median(alltrend[,lastyear],na.rm=TRUE)
allshare<-allshare[order(allshare$share,decreasing = TRUE),]
#allshare<-arrange(allshare,desc(median))
nshare<-min(10,nrow(allshare))

decrease<-order(alltrend$trend,decreasing = FALSE)   # gives order of countries in relativ decrease
decreaseabs<-order(alltrend$diff,decreasing = FALSE) # gives order of countries in absolute decrease
#decreasedata<-alltrend%>%filter(!is.na(trend))%>%filter(diff<0)%>%arrange((trend))
decreasedata <- alltrend[!is.na(alltrend$trend), ]
decreasedata <- decreasedata[decreasedata$diff < 0, ]
decreasedata <- decreasedata[order(decreasedata$diff),]
decreasen<-nrow(decreasedata)                     # number of countries with decreasing trend
decreasetot<-sum(decreasedata$diff)     # total decrease in countries with decreasing trend
decreasemed<-median(decreasedata$diff)     # total decrease in countries with decreasing trend
decreasemean<-mean(decreasedata$diff)     # total decrease in countries with decreasing trend

increase<-order(alltrend$trend,decreasing = FALSE)
increaseabs<-order(alltrend$diff,decreasing = FALSE)
#increasedata<-alltrend%>%filter(!is.na(trend))%>%filter(diff>0)%>%arrange(desc(trend))
increasedata <- alltrend[!is.na(alltrend$trend) & alltrend$diff>0, ]
increasedata <- increasedata[order(increasedata$dif, decreasing = TRUE), ]
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
missingcountries<-countriesnoeu[!countriesnoeu%in%alltrend$party]
missingcountries<-c(missingcountries,as.vector(alltrend$party[is.nan(alltrend$trend)]))
missingcountries<-missingcountries[!missingcountries%in%excludeparty]
missing<-length(missingcountries)

# Exclude some national data with wrong unit
panderOptions('missing'," - ")
panderOptions('table.alignment.default', c('left','right','right','right','left','right','right'))


selyears<-c(firstyear,lastyear)
#curtable<-(curemissions%>%select(party,one_of(selyears)))
curtable <- curemissions[, c("party", selyears)]

# Make sure countries first - EU last
curtable <- rbind(curtable[! curtable$party %in% eu,], curtable[curtable$party %in% eu,])
if(exists("curtable_GBE")){
  curtable_GBE<-(curemissions_GBE%>%select(party,one_of(selyears)))
}
curtable[curtable==0]<-NA
curtable$party<-as.character(curtable$party)
curtable$party<-unlist(lapply(c(1:nrow(curtable)), function(x) {
  
  y <- curcountries[code3==curtable$party[x] & variable==eusubm & value==1, name]
  return(y)
}))
  #country4sub$name[which(country4sub$code3==curtable$party[x])]))

if(exists("curtable_GBE")){
  if(nrow(curtable_GBE) > 0){
    curtable_GBE$party <- "United Kingdom (KP)"
    curtable <- rbind(curtable, curtable_GBE)
    
    UK_KP_pos <- which(curtable$party %in% "United Kingdom")
    curtable <- rbind(curtable[1:UK_KP_pos, ], curtable[nrow(curtable), ], curtable[(UK_KP_pos + 1):(nrow(curtable)-1), ])
  }}


curtable[,selyears]<-format(curtable[,selyears],digits=2)
sel<-curtable$party==eusubml
#eumsubm<-eum[which(eu==eusubm)]
#curtable$party[curtable$party==eusubm]<-eum[which(eu==eusubm)]

##test
curtable[curtable$party==eusubml,]<-paste0("**",gsub(" ","",curtable[curtable$party==eusubml,]),"**")
if(grepl("NA",curtable[sel,firstyear])&grepl("NA",curtable[sel,lastyear])) curtable<-curtable[!sel,]

for(yy in selyears){
    curtable[,yy]<-unlist(lapply(c(1:nrow(curtable)),function(x) if(grepl("NA",curtable[x,yy])){"&#09;"}else{curtable[x,yy]}))
    #curtable[,yy]<-unlist(lapply(c(1:nrow(curtable)),function(x) if(" 0"==curtable[x,yy]){"&#09;"}else{curtable[x,yy]}))
}

rownames(curtable)<-NULL
names(curtable)<-c("Country",selyears)

sel <- which(curtable[, firstyear] == "&#09;" & curtable[, lastyear] == "&#09;")
if(length(sel) > 0) curtable <- curtable[!row.names(curtable) %in% sel, ]


