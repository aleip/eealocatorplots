selmeasure<-"Emissions"
curemissions<-agriemissions[agriemissions$sector_number==cursec&agriemissions$category==curcat&agriemissions$measure==selmeasure,]
curemissions<-curemissions[curemissions$gas!="NMVOC",]
select<-curemissions$sector_number=="3.D.1.1" | curemissions$sector_number=="3.D.1.2" 
if(sum(select)>0) curemissions$category[select]<-as.character(curemissions$type[select])
select<-curemissions$sector_number=="3.D.2.1" | curemissions$sector_number=="3.D.2.2" 
if(sum(select)>0) curemissions$category[select]<-as.character(curemissions$target[select])
if(cursec=="3.A") curemissions<-agriemissions[agriemissions$sector_number==cursec,]

eusel<-curemissions$party==eusubm
curgas<-as.character(curemissions$gas[eusel])
curmeasure<-curmeasurenew(unique(as.character(curemissions$measure[eusel])))
curunit<-unique(as.character(curemissions$unit[eusel]))
curcat<-curcatnew(curcat)
curcattext<-curcatlong(curcat,cursec)

# Names of sector and category
# cursect: numerical sector (e.g. 3.A.1)
# curseclong: term for sector (e.g. Enteric Fermenation: this is defined not obtained from data)
if(curseclong=="")curseclong<-curcat

if("Aggregate GHGs" %in% curgas) {
    eushareghg<-curemissions[eusel & curemissions$gas=="Aggregate GHGs",lastyear]/eutotalghg[lastyear]
    eushareagrighg<-curemissions[eusel & curemissions$gas=="Aggregate GHGs",lastyear]/euagritotalghg[lastyear]
}else if(length(curgas)==1){
    eushareghg<-curemissions[eusel,lastyear]/eutotalghg[lastyear]
    eushareagrighg<-curemissions[eusel,lastyear]/euagritotalghg[lastyear]
}else{
    eushareghg<-0
    eushareagrighg<-0
    eushareagrighgghg<-0
}

if("CH4" %in% curgas) {
    eusharech4<-curemissions[eusel & curemissions$gas=="CH4",lastyear]/eutotalch4[lastyear]
    eusharech4total<-curemissions[eusel & curemissions$gas=="CH4",lastyear]/eutotalghg[lastyear]
    eushareagrich4<-curemissions[eusel & curemissions$gas=="CH4",lastyear]/euagritotalch4[lastyear]
    eushareagrich4ghg<-curemissions[eusel & curemissions$gas=="CH4",lastyear]/euagritotalghg[lastyear]
}else{
    eusharech4<-0
    eushareagrich4<-0
    eushareagrich4ghg<-0
}
if("N2O" %in% curgas) {
    eusharen2o<-curemissions[eusel & curemissions$gas=="N2O",lastyear]/eutotaln2o[lastyear]
    eusharen2ototal<-curemissions[eusel & curemissions$gas=="N2O",lastyear]/eutotalghg[lastyear]
    eushareagrin2o<-curemissions[eusel & curemissions$gas=="N2O",lastyear]/euagritotaln2o[lastyear]
    eushareagrin2oghg<-curemissions[eusel & curemissions$gas=="N2O",lastyear]/euagritotalghg[lastyear]
}else{
    eusharen2o<-0
    eushareagrin2o<-0
    eushareagrin2oghg<-0
}
if("CO2" %in% curgas) {
    eushareco2<-curemissions[eusel & curemissions$gas=="CO2",lastyear]/eutotalco2[lastyear]
    eushareco2total<-curemissions[eusel & curemissions$gas=="CO2",lastyear]/eutotalghg[lastyear]
    eushareagrico2<-curemissions[eusel & curemissions$gas=="CO2",lastyear]/euagritotalco2[lastyear]
    eushareagrico2ghg<-curemissions[eusel & curemissions$gas=="CO2",lastyear]/euagritotalghg[lastyear]
}else{
    eushareco2<-0
    eushareagrico2<-0
    eushareagrico2ghg<-0
}
eusharencgg<-eusharech4+eusharen2o
eushareagrincgg<-eushareagrich4+eushareagrin2o
eushareagrincggghg<-eushareagrich4ghg+eushareagrin2oghg

curtrend<-curemissions[eusel,lastyear]/curemissions[eusel,firstyear]
curtrendabs<-curemissions[eusel,lastyear]-curemissions[eusel,firstyear]
lasttrend<-curemissions[eusel,lastyear]/curemissions[eusel,lastyear2]

alltrend<-cbind(curemissions[!eusel,],curemissions[!eusel,lastyear]/curemissions[!eusel,firstyear])
alltrend<-cbind(alltrend,alltrend[,lastyear]-alltrend[,firstyear])
names(alltrend)<-c(names(curemissions),"trend","diff")
alltrend<-alltrend[order(alltrend$diff),]

allshare<-subset(alltrend,select=lastyear)
allshare$share<-alltrend[,lastyear]/sum(alltrend[,lastyear])
allshare<-allshare[order(allshare$share,decreasing = TRUE),]
nshare<-min(10,nrow(allshare))

decrease<-order(alltrend$trend,decreasing = FALSE)   # gives order of countries in relativ decrease
decreaseabs<-order(alltrend$diff,decreasing = FALSE) # gives order of countries in absolute decrease
decreasen<-sum(alltrend$trend<1,na.rm=TRUE)                     # number of countries with decreasing trend
decreasetot<-sum(alltrend$diff[alltrend$diff<0])     # total decrease in countries with decreasing trend
decreasecnt<-alltrend$party[alltrend$diff/decreasetot>0.1] #Those countries contributing with at least 10% to EU trend
decreasencnt<-length(decreasecnt)                          #Number of countries contributing with at least 10% to EU trend
if(decreasen>0 & decreasencnt==0){
    decreasecnt<-alltrend$party[1:3] #Those countries contributing with at least 10% to EU trend
    decreasencnt<-length(decreasecnt)                          #Number of countries contributing with at least 10% to EU trend
}

decreasescnt<-sum(alltrend$diff[alltrend$party%in%decreasecnt])

increase<-order(alltrend$trend,decreasing = FALSE)   # gives order of countries in relativ increase
increaseabs<-order(alltrend$diff,decreasing = FALSE) # gives order of countries in absolute increase
increasen<-sum(alltrend$trend>1,na.rm=TRUE)                     # number of countries with decreasing trend
increasetot<-sum(alltrend$diff[alltrend$diff>0])     # total increase in countries with decreasing trend
increasecnt<-alltrend$party[alltrend$diff/increasetot>0.1] #Those countries contributing with at least 10% to EU trend
increasencnt<-length(increasecnt)                          #Number of countries contributing with at least 10% to EU trend
if(increasen>0 & increasencnt==0){
    increasecnt<-alltrend$party[1:3] #Those countries contributing with at least 10% to EU trend
    increasencnt<-length(increasecnt)                          #Number of countries contributing with at least 10% to EU trend
}
increasescnt<-sum(alltrend$diff[alltrend$party%in%increasecnt])

trendtot<-increasetot+decreasetot

stablen<-sum(alltrend$trend==1)

subcategssel<-grepl(paste0("^",cursec,"\\."),agrimix$sector_number) & agrimix$party==eusubm&agrimix$meastype=="EM"&agrimix$gas%in%curgas
subcateg<-agrimix[subcategssel,]
subcateg<-subcateg[order(subcateg[,lastyear],decreasing = TRUE),]
nsubcateg<-sum(subcateg[,lastyear]/sum(subcateg[,lastyear])>0.1)

renameb2ind<-subcateg$sector_number=="3.B.2.5"
if(sum(renameb2ind)>0) subcateg$category[renameb2ind]<-"Indirect Emissions"
select<-subcateg$sector_number=="3.D.1.1" | subcateg$sector_number=="3.D.1.2" 
if(sum(select)>0) subcateg$category[select]<-as.character(subcateg$type[select])
select<-subcateg$sector_number=="3.D.2.1" | subcateg$sector_number=="3.D.2.2" 
if(sum(select)>0) subcateg$category[select]<-as.character(subcateg$target[select])

curheaders<-c(firstyear, years[length(years) - 1], lastyear,"GHGfirst","GHGlast-1","GHGlast")
curemissions$party<-as.character(curemissions$party)
emfirstlast<-curemissions[,c("party",firstyear, years[length(years) - 1], lastyear)]
emfirstlast[,"GHGfirst"]<-emfirstlast[,firstyear]
emfirstlast[,"GHGlast-1"]<-emfirstlast[,years[length(years) - 1]]
emfirstlast[,"GHGlast"]<-emfirstlast[,lastyear]
tmpis<-emfirstlast[emfirstlast$party=="ISL",]
emfirstlast<-emfirstlast[emfirstlast$party!="ISL",]
tmpeu<-emfirstlast[emfirstlast$party=="EUC",]
emfirstlast<-emfirstlast[emfirstlast$party%in%countriesnoeu,]
tmpeunois<-tmpeu
tmpeunois[,curheaders]<-apply(emfirstlast[,curheaders],2,sum,na.rm=TRUE)
if(nrow(tmpis)>0)tmpis$party<-"Iceland"
tmpeunois$party<-"EU-28"
tmpeu$party<-"EU-28 + ISL"


emfirstlast$party<-sapply(1:nrow(emfirstlast),function(x) country4sub$name[country4sub$code3==emfirstlast$party[x]])
emfirstlast<-emfirstlast[order(emfirstlast$party),]
emfirstlast<-rbind(emfirstlast,tmpeunois,tmpis,tmpeu)
emfirstlast[,curheaders]<-round(emfirstlast[,curheaders],0)
if(length(curgas)==1){
    names(emfirstlast)<-c("Member States",
                          paste0("GHG emissions in ",firstyear," (kt CO2 equivalents)"),
                          paste0("GHG emissions in ",years[length(years) - 1]," (kt CO2 equivalents)"),
                          paste0("GHG emissions in ",lastyear," (kt CO2 equivalents)"),
                          paste0(curgas," emissions in ",firstyear," (kt CO2 equivalents)"),
                          paste0(curgas," emissions in ",years[length(years) - 1]," (kt CO2 equivalents)"),
                          paste0(curgas," emissions in ",lastyear," (kt CO2 equivalents)"))
    row.names(emfirstlast)<-NULL
}

