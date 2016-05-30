rm(list=objects())
locplots<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"           #!!!

setwd(locplots)
figdate<-format(Sys.time(), "%Y%m%d")
invyear<-2016
invloc<-paste0("../",invyear)                                               #!!!
issuedir<-paste0(invloc,"/checks/")
lastkeyfile<-"keycategories~20151012.csv"
lastkeyfile<-paste0(issuedir,"keycatetgories/",lastkeyfile)
years2keep<-c(1990:2014)

updrec<-0
if(updrec==1){
    allagri20160322<-allagri
    alltotals20160322<-alltotals
    agridet20160322<-agridet
    saverecalc<-c(saverecalc,"allagri20160322","alltotals20160322","agridet20160322")
    save(list=saverecalc,file=paste0(invloc,"/eealocator/eealocator_recalc~",figdate,".RData"))
    save(list=saverecalc,file=paste0(invloc,"/eealocator/eealocator_recalc.RData"))
}else{
    load(file=paste0(invloc,"/eealocator/eealocator_recalc.RData"))
}
source("eugirp_functions.r")
source("eugirp_definitions.r")

assign("agridet1",agridet20160202)
assign("agridet2",agridet20160322)
mergefields<-c("party",uniquefields[-which(uniquefields=="variableUID")])

# compare GB with UK ... fake they are the same
agridet1$party[agridet1$party=="UK"]<-"GB"

agridetrecalc<-merge(agridet1,agridet2,by=mergefields,all = TRUE)
sel<-is.na(agridetrecalc$variableUID.x)
notin20160202<-agridetrecalc[sel & agridetrecalc$measure=="Emissions",]
agridetrecalc<-agridetrecalc[!sel,]
sel<-is.na(agridetrecalc$variableUID.y)
notin20160322<-agridetrecalc[sel & agridetrecalc$measure=="Emissions",]
agridetrecalc<-agridetrecalc[!sel,]
View(agridetrecalc)
View(notin20160202)
View(notin20160322)

years<-names(agridet1)[grepl("^[12]",names(agridet1),perl=TRUE)]
yearsdet2<-names(agridet2)[grepl("^[12]",names(agridet2),perl=TRUE)]


keepy<-years[c(1,length(years))]
keepY<-c("Year1","Year2")
elimy<-years[c(2:(length(years)-1))]
year1<-years[1]
year2<-years[length(years)]
lastyear<-max(as.numeric(year2),as.numeric(yearsdet2[length(yearsdet2)]))
print(year1)
print(year2)
agridetcomp<-agridetrecalc[,-which(substr(names(agridetrecalc),1,4)%in%elimy)]
agridetcomp<-agridetcomp[-which(names(agridetcomp)=="variableUID.x")]

sel<-agridetcomp$measure=="Emissions"
checkrecalc<-agridetcomp[sel,]
checkrecalc$rYear1<-checkrecalc[,paste0(year1,".y")]/checkrecalc[,paste0(year1,".x")]
checkrecalc$rYear2<-checkrecalc[,paste0(year2,".y")]/checkrecalc[,paste0(year2,".x")]
sel<-is.nan(checkrecalc$rYear1) & checkrecalc[,paste0(year1,".x")]==checkrecalc[,paste0(year1,".y")]
checkrecalc<-checkrecalc[!sel,]
sel<-is.nan(checkrecalc$rYear2) & checkrecalc[,paste0(year2,".x")]==checkrecalc[,paste0(year2,".y")]
checkrecalc<-checkrecalc[!sel,]
checkrecalc<-checkrecalc[!checkrecalc$party=="EU28",]

sel<-checkrecalc$rYear1!=1 | checkrecalc$rYear2!=1
sel[is.na(sel)]<-TRUE
checkrecalc<-checkrecalc[sel,]



# Effect in absolute values
checkrecalc$eYear1<-checkrecalc[,paste0(year1,".y")]-checkrecalc[,paste0(year1,".x")]
checkrecalc$eYear2<-checkrecalc[,paste0(year2,".y")]-checkrecalc[,paste0(year2,".x")]



# Share of national total emissions by gas
lul<-"Total (with LULUCF)"
lui<-"Total (with LULUCF  with indirect)"
totCH4<-alltotals20160322[alltotals20160322$gas=="CH4"&alltotals20160322$type==lul,c("party","gas",keepy)]
totN2O<-alltotals20160322[alltotals20160322$gas=="N2O"&alltotals20160322$type==lul,c("party","gas",keepy)]
totCO2<-alltotals20160322[alltotals20160322$gas=="CO2"&alltotals20160322$classification==lui,c("party","gas",keepy)]
tots<-rbind(totCH4,totN2O,totCO2)
tots$gas<-droplevels(tots$gas)
tots$party<-as.character(tots$party)
checkrecalc$party<-as.character(checkrecalc$party)
checkrecalc$gas<-as.character(checkrecalc$gas)
checkrecalc$tYear1<-unlist(lapply(c(1:nrow(checkrecalc)),function(x) tots[tots$gas==checkrecalc$gas[x] & tots$party==checkrecalc$party[x],year1]))
checkrecalc$tYear2<-unlist(lapply(c(1:nrow(checkrecalc)),function(x) tots[tots$gas==checkrecalc$gas[x] & tots$party==checkrecalc$party[x],year2]))
checkrecalc$sYear1<-checkrecalc$eYear1/checkrecalc$tYear1
checkrecalc$sYear2<-checkrecalc$eYear2/checkrecalc$tYear2

# Flag those where the share to total gas emissions is > 0.005
checkrecalc$fYear1<-abs(checkrecalc$sYear1)>0.005
checkrecalc$fYear2<-abs(checkrecalc$sYear2)>0.005
checkrecalc$flag<-abs(checkrecalc$sYear1)>0.005 | abs(checkrecalc$sYear2)>0.005
checkrecalc$fYear1[checkrecalc$fYear1]<-1
checkrecalc$fYear2[checkrecalc$fYear2]<-1
checkrecalc$flag[checkrecalc$flag]<-1
checkrecalc$fYear1[!checkrecalc$fYear1]<-""
checkrecalc$fYear2[!checkrecalc$fYear2]<-""
checkrecalc$flag[!checkrecalc$flag]<-""

cy<-Reduce(rbind,(lapply(c(1:nrow(checkrecalc)),function(x) 
    paste(unlist(checkrecalc[x,paste0("f",keepY)]==1)*as.numeric(keepy),collapse="-"))))
cy<-gsub("^0-","",cy)
cy<-gsub("-0$","",cy)
cy<-gsub("^0","",cy)
checkrecalc$years<-cy

checkrecalc<-filldf(checkrecalc,allcheckfields)


keycategories<-keysources()
x1<-18;x2<-19
x1<-1;x2<-nrow(checkrecalc)
test<-Reduce(rbind,lapply(c(x1:x2),function(x) unlist(flags4newissue(checkrecalc[x,],"recalc1"))))
checkrecalc[x1:x2,flag4issues]<-test
addfields<-names(checkrecalc)[!names(checkrecalc)%in%allcheckfields4emrt]
allfields<-c(allcheckfields4emrt[1:which(allcheckfields4emrt=="years")],addfields,allcheckfields4emrt[(which(allcheckfields4emrt=="years")+1):length(allcheckfields4emrt)])
write.csv(checkrecalc[allfields],file=paste0(issuedir,"recalculations/checkrecalc_20160322.vs.20160202~",figdate,".csv"))




