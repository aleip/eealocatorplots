## NE checks from EEA
#1- The subcategories: EEA lists some subcategories (other other animals) while
#   eu-girp can identify the actual animals (e.g. 3.B.1.4, 3.B.1.4 Deer, 3.B.1.4 Fur-bearing animals, etc.)
#   As IPCC default cannot exist for aggregates, this difference is OK 
#   --> keep only the most disaggregated level!
#2- We have issues for categories 3 (as a whole), 3.1 (livestock as a whole)
#   --> Filter those out
#3- Most of the other issues that R script identifies and UBA do not are related to NMVOC. Could that be the case of missing default methodology from IPCC?

checkyear<-lastyear
# UBA identifies them as variables where >=20 and <29 countries report numeric values
minncountries<-3

#con<-file(paste0(invloc,"/eealocator/IPCC_variables_MMR_560.xml"))
conipcc<-file("IPCC_variables_MMR_560.xml")
open(conipcc)
dataList <- list()
ecdfList <- list()
obluids<-vector()
while (length(oneLine <- readLines(conipcc, n = 1, warn = FALSE)) > 0) {
    myVector <- (strsplit(oneLine, ">"))
    if(grepl("uid",myVector[[1]][1])){
        curuid<-gsub("</uid","",myVector[[1]][2])
        obluids<-c(obluids,curuid)
    }    
    
} 
close(conipcc)
obluids<-unique(obluids)
emuidscat3<-as.character(unique(alldata$variableUID[alldata$measure=="Emissions"&grepl("^3",alldata$sector_number)]))
emuidsnot3<-as.character(unique(allnotations$variableUID[allnotations$measure=="Emissions"&grepl("^3",allnotations$sector_number)]))
emuidscat3<-unique(c(emuidscat3,emuidsnot3))
obluidscat3<-obluids[obluids%in%emuidscat3]
obldatacat3<-alldata[alldata$variableUID%in%obluidscat3,]

oblnotcat3<-allnotations[allnotations$variableUID%in%obluidscat3,]
oblcat3_ne<-oblnotcat3[grepl("NE",oblnotcat3$notation),]


# NEs are a problem only if there is IPCC default method
allnotcat3<-allnotations[grepl("^3",allnotations$sector_number),]
agrine<-allnotcat3[grepl("NE",allnotcat3$notation),]
agrine<-agrine[order(agrine$sector_number,agrine$category),]
agrine_em<-agrine[agrine$measure=="Emissions",]
namesnecheck<-names(agrine_em)

# foot note in table Table 3s2 of the CRF saying: 
# "The 2006 IPCC Guidelines do not provide methodologies for the calculation of methane (CH4) 
# emissions and CH4 and N2O removals from agricultural soils, 
ok1<-agrine_em$gas=="CH4" & agrine_em$sector_number=="3.D"
# or carbon dioxide (CO2) emissions 
# from prescribed burning of savannas and field burning of agricultural residues. 
# Parties that have estimated such emissions should provide, in the national inventory report (NIR), 
# additional information (activity data and emission factors) used to derive these estimates 
# and include a reference to the section of the NIR in the documentation box of the corresponding 
# Sectoral background data tables."

# No IPCC defaults are flagged in the fucntion 'ipccdefaultsexist'
okburning<-grepl("^3.[EF]",agrine_em$sector_number)

ok2<-agrine_em$sector_number=="3."
#Do not go too deep into the details .... ()
#ok3<-unlist(lapply(c(1:nrow(agrine_em)),function(x) length(strsplit(as.character(agrine_em$sector_number[x]),"\\.")[[1]])>3))
ok4<-grepl("^3.[1ABCDEFG]$",agrine_em$sector_number)
ok5<-agrine_em$gas=="NMVOC"

agrine_em<-agrine_em[!(ok1 | ok2 | ok4 | ok5),]
agrine_em<-agrine_em[remagglevel(agrine_em),]
agrine_em$meastype<-"EM"
agrine_em$nyears<-""

# Check if one of the notation keys is used for the year 2016
sel<-sapply(1:nrow(agrine_em),function(x) checkforyear(agrine_em$year[x],checkyear))
agrine_em<-agrine_em[sel,-which(names(agrine_em)%in%c("year","meastype"))]



# Add data gaps for the year 2016
noreporting<-allagri[allagri$measure=="Emissions",]
noreporting$nyears<-apply(noreporting[years],1,function(x) sum(is.na(x))+sum(x==0,na.rm=TRUE)+sum(x=="",na.rm=TRUE))
#noreporting<-noreporting[noreporting$nyears>0,]
noreporting<-noreporting[noreporting$gas!="NMVOC",]
noreporting<-noreporting[is.na(noreporting[,lastyear]),]
noreporting<-noreporting[,-which(names(noreporting)%in%years)]
noreporting<-noreporting[,-which(names(noreporting)%in%c("datasource","notation"))]
noreporting$value<-""

# List of Notation keys reported reported
agrino<-allnotations[grepl("^3",allnotations$sector_number),]
agrino<-agrino[grepl("NO|NA|IRL|NE",allnotcat3$notation),]
agrino<-agrino[order(agrino$sector_number,agrino$category),]
agrino_em<-agrino[agrino$measure=="Emissions",]
# Check if one of the notation keys is used for the year 2016
sel<-sapply(1:nrow(agrino_em),function(x) checkforyear(agrino_em$year[x],checkyear))
agrino_em<-agrino_em[sel,-which(names(agrino_em)=="year")]
namesnecheck<-c("nyears",names(agrino_em))

noreporting<-merge(noreporting,agrino_em[,c("variableUID","party","notation")],by=c("variableUID","party"),all.x=TRUE)
#Select only those for which no notation key had been used
noreporting<-noreporting[is.na(noreporting$notation),]
noreporting<-noreporting[,namesnecheck]
#noreporting$nyears<-""
if(nrow(noreporting)>0){
    noreporting$notation<-"empty"
    noreporting<-noreporting[remagglevel(noreporting),]
    noreporting<-noreporting[,names(agrine_em)]
}

# temp<-allnotations
# temp$variableUID<-as.character(temp$variableUID)
# temp$party<-as.character(temp$party)
# noreporting$notation<-sapply(1:nrow(noreporting),function(x) 
#     paste(temp$notation[temp$variableUID==noreporting$variableUID[x]&temp$party==noreporting$party[x]],collapse="-"))
# noreporting$year<-sapply(1:nrow(noreporting),function(x) 
#     paste(temp$year[temp$variableUID==noreporting$variableUID[x]&temp$party==noreporting$party[x]],collapse="-"))
 agrineempty<-rbind(agrine_em,noreporting)
 for(tosp in c("target","source","option","type","method")){
     no2emtpy<-paste("no",tosp)
     agrineempty[agrineempty[,tosp]==no2emtpy,tosp]<-""
 }

#h<-agrineempty[agrineempty$sector_number=="3.H",]
#h<-ipccdefaultexists(h)

agrineempty<-ipccdefaultexists(agrineempty)
o<-order(agrineempty$sector_number,agrineempty$party,agrineempty$category)
agrineempty<-agrineempty[o,]
agrineempty$issuenr<-""
#View(agrineempty)


# Add info from last year
oldnecheck<-read.csv(file=paste0(issuedir,"nechecks/necheck.csv"),header=TRUE,comment.char="#")
mergefields<-c("party","sector_number","category","gas","unit","variableUID","notation")
keepfields<-c(c("issuenr","flag","Last.action.date","Last.action","comments.reference"))

# Separate in those for which there was an issue opened those for which this was not the case
# All issues with issue number should be kept.
# All the others just merge but keep only if it matches with a current one.
oldnecheckinr<-oldnecheck[oldnecheck$issuenr!="",c(mergefields,keepfields)]
names(oldnecheckinr)[which(names(oldnecheckinr)=="issuenr")]<-"lastissuenr"
oldnechecknis<-oldnecheck[oldnecheck$issuenr=="",c(mergefields,keepfields)]


necheck<-merge(agrineempty,oldnecheckinr,by=mergefields,all=TRUE)
if(nrow(oldnechecknis)>0)necheck<-merge(necheck,oldnechecknis,by=c(mergefields,keepfields),all.x=TRUE)
#necheck$issuenr<-""

necheck<-convert2char(necheck)
necheck[is.na(necheck)]<-""
necheck$ncountriesNK<-""
necheck$ncountriesValue<-""
# The 'no issues' can be removed directly
nechecknames<-c("party","notation","nyears","ncountriesNK","ncountriesValue","uidIEF","IPCC2006","IPCC1997",
                "sector_number","category","gas","unit",
                "lastissuenr","issuenr","flag","Last.action.date","Last.action","comments.reference",
                "variableUID","value","classification","source","target","method","option","type",
                "measure")
nechecksclosed<-necheck[necheck$flag == "cn",nechecknames]
nechecksclosed<-nechecksclosed[!is.na(nechecksclosed$party),]
nechecksopen<-necheck[necheck$flag != "cn",nechecknames]
nechecksopen<-nechecksopen[!is.na(nechecksopen$party),]

o<-order(nechecksopen$party,nechecksopen$sector_number,nechecksopen$category)
nechecksopen<-nechecksopen[o,nechecknames]

o<-order(nechecksclosed$Last.action.date)
nechecksclosed<-nechecksclosed[o,nechecknames]

con<-file(paste0(invloc,"/checks/nechecks/nenochecks",cursubm,"~",curdate(),".csv"), open="wt")
writeLines(paste0("######################  NE AND EMPTY CELL CHECK for the year ",lastyear,"  #################################\n",
                  "#\n",
                  "#Check on notation key NE and data gaps for the year ",lastyear,".\n",
                  "#NE-check: Use of the Notation key 'NE' for the measure 'Emissions' is flagged.\n",
                  "#          Only NEs used at the highest disaggregated level are reported. \n",
                  "#          E.g. if a country reports 'NE' for 3.D.1.1 and 3.D.1 then only 3.D.1.1 is listed.\n",
                  "#Gap-check: Issues show if there was no reporting (empty cells) of emissions for the year ",lastyear,".\n",
                  "#          Missing of emissions of NMCOC are not listed. Again, the list is for the highest level of disaggregation.\n",
                  "#For all selected issue the existence of IPCC default vaues is checked.\n",
                  "#column 'notation': notation key. If different notation keys have been used for different years they are separated by a '-'.\n",
                  "#column 'year': years for which the notation key has been used. If different notation keys have been used, the (range of) years are separated by a '-'.\n",
                  "#column 'nyears': NE check: not used. Gap-check: number of years cell was empty.\n",
                  "#column 'uidIEF': variableUID corresponding to the IEF of the emissions selected.\n",
                  "#"),con)
writeLines(paste0("######################  CHECK ON THE USE OF NOTATION KEYS WHERE MOST COUNTRIES REPORT VALUES  for the year ",lastyear,"   #################################\n",
                  "#\n",
                  "#Check on notation keys NO -NE -IRL -NA for the year ",lastyear,".\n",
                  "#All isues are selected which use one of the notation keys and",
                  "# - at least ",minncountries," have reported a value.",
                  "# column 'ncountriesNK' indicates the number of countries using a notation key and",
                  "# column 'ncountriesValue' indicates the number of countries reporting a value.",
                  "# column 'uidIEF': variableUID corresponding to the IEF of the emissions selected.\n",
                  "#"),con)
writeLines(docflags,con)
if(nrow(nechecksopen)>0){
    writeLines(paste0("#\n######################  NE AND EMPTY CELL CHECK for the year ",lastyear,"  #################################"),con)
    writeLines("#\n# Open Issues",con)
    write.csv(nechecksopen,con)
}
if(nrow(nechecksclosed)>0){
    writeLines("#\n#\n# Closed Issues",con)
    write.csv(nechecksclosed,con)
}
#Leave open and append NO check
#close(con)
write.csv(agrishares,file=paste0(issuedir,"/significant/agrishares~",curdate(),".csv"))

# List of Notation keys reported reported

allnotcat3<-allnotations[grepl("^3",allnotations$sector_number),]
agrino<-allnotcat3[grepl("NO|NA|IRL|NE",allnotcat3$notation),]
agrino<-agrino[order(agrino$sector_number,agrino$category),]
agrino_em<-agrino[agrino$measure=="Emissions",]
namesnecheck<-names(agrino_em)

# Check if one of the notation keys is used for the year 2016
sel<-sapply(1:nrow(agrino_em),function(x) checkforyear(agrino_em$year[x],checkyear))
agrino_em<-agrino_em[sel,-which(names(agrino_em)=="year")]

ok1<-agrino_em$gas=="CH4" & agrino_em$sector_number=="3.D"
ok2<-agrino_em$sector_number=="3."
#ok3<-unlist(lapply(c(1:nrow(agrino_em)),function(x) length(strsplit(as.character(agrino_em$sector_number[x]),"\\.")[[1]])>3))
#ok4 not needed if remagglevel is applied
ok4<-grepl("^3.[1ABCDEFGHIJ]$",agrino_em$sector_number)
ok5<-agrino_em$gas=="NMVOC"
agrino_em<-agrino_em[!(ok1 | ok2 | ok5),]
agrino_em<-agrino_em[remagglevel(agrino_em),]
for(tosp in c("target","source","option","type","method")){
    no2emtpy<-paste("no",tosp)
    agrino_em[agrino_em[,tosp]==no2emtpy,tosp]<-""
}

xx<-agrino_em
cols2leave<-paste(names(xx)[!names(xx)%in%c("party","notation")],collapse="+")
arrange<-as.formula(paste(cols2leave,"~ party"))
xx<-dcast(xx,arrange,function(x) paste(x,collapse="-"),value.var="notation")
xx<-xx[,!names(xx)%in%eu]
#Remove "FM" is both "FM" and "FRK" are present
if(sum(names(xx)%in%c("FRK","FM"))==2) xx<-xx[,!names(xx)%in%"FM"]
#Remove "GBK" is both "GBK" and "GBE" are present
if(sum(names(xx)%in%c("GBK","GBE"))==2) xx<-xx[,!names(xx)%in%"GBK"]
xx$ncountriesNK<-apply(xx[,names(xx)%in%countries3],1,function(x) sum(x!=""))
o<-order(xx$sector_number,xx$category)
agrinotations<-xx[o,]
write.csv(agrinotations,paste0(issuedir,"nechecks/notationkeys~",curdate(),".csv"))


# xxv<-aggregate(xx[,"notation"],by=list(xx$variableUID),function(x) paste(x,collapse="-"))
# xxn<-aggregate(xx[,"ncountriesNK"],by=list(xx$variableUID),function(x) paste(x,collapse="-"))
# xy<-merge(xxv,xxn,by="Group.1")
# names(xy)<-c("variableUID","notation","ncountries")
# xy<-merge(xy,agrino_em[,-which(names(agrino_em)=="notation")],by="variableUID")
# noreportingyears<-xy[remagglevel(xy),]

# UBA identifies them as variables where >=20 and <29 countries report numeric values
countriesNO<-merge(agrino_em,agrinotations[,c("variableUID","ncountriesNK")],by="variableUID",all.x=TRUE)

# Add data gaps for the year 2015
reporting<-allagri[allagri$measure=="Emissions",]
reporting$nyears<-apply(reporting[years],1,function(x) sum(is.na(x))+sum(x==0,na.rm=TRUE)+sum(x=="",na.rm=TRUE))
#reporting<-reporting[reporting$nyears>0,]
reporting<-reporting[reporting$gas!="NMVOC",]

reporting<-reporting[!is.na(reporting[,checkyear]),]
reporting<-reporting[,-which(names(reporting)%in%years)]
reporting$notation<-""
reporting<-reporting[,-which(names(reporting)%in%c("datasource"))]
reporting$value<-""
#xavi20180125: reporting$year<-2015
reporting$year<-checkyear
reporting<-reporting[!reporting$party%in%eu,]
#reporting<-reporting[remagglevel(reporting),]
reporting<-reporting[,names(agrine_em)]

reporting<-as.data.frame(reporting)
cols2leave<-paste(names(reporting)[!names(reporting)%in%c("party","nyears")],collapse="+")
arrange<-as.formula(paste(cols2leave,"~ party"))
xz<-dcast(reporting,arrange,value.var="nyears")
#Remove "FM" is both "FM" and "FRK" are present
if(sum(names(xz)%in%c("FRK","FM"))==2) xz<-xz[,!names(xz)%in%"FM"]
#Remove "GBK" is both "GBK" and "GBE" are present
if(sum(names(xz)%in%c("GBK","GBE"))==2) xz<-xz[,!names(xz)%in%"GBK"]
xz$ncountriesValue<-apply(xz[,names(xz)%in%countries3],1,function(x) sum(!is.na(x)))


countriesNO<-merge(countriesNO,xz[,c("variableUID","ncountriesValue")],by="variableUID",all.x=TRUE)
countriesNO<-convert2char(countriesNO)
countriesNO[is.na(countriesNO)]<-""


countriesNO$ncountriesValue<-as.numeric(countriesNO$ncountriesValue)
countriesNO$ncountriesValue[is.na(countriesNO$ncountriesValue)]<-0
sel<-countriesNO$ncountriesValue>minncountries
countriesx<-countriesNO[sel,]
countriesx<-ipccdefaultexists(countriesx)

#write.csv(countriesx,file="countriesNO-NA-IRL.csv")

# Merge the two files
# somehow 'method' generates an error in combination with all=TRUE...
tempfields<-c("option","classification","source","target","type","option")
commfields<-c("issuenr","flag","Last.action.date","Last.action","comments")

#nechecknames<-c(sectfields,"ncountriesNK","ncountriesValue","notation",
#                "uidIEF","IPCC2006","IPCC1997",commfields,"variableUID","gas","unit",tempfields)
necheck<-countriesx

#necheck[,c("issuenr","flag","Last.action.date","Last.action","comments")]<-""
necheck<-filldf(necheck,cols=nechecknames,fillwith = "")
o<-order(necheck$party,necheck$sector_number,necheck$category)
necheck<-necheck[o,nechecknames]

#Appending on NE check
#con<-file(paste0(invloc,"/checks/nechecks/nocheck",cursubm,"~",curdate(),".csv"), open="wt")
#writeLines(docflags,con)
if(nrow(necheck)>0){
    writeLines(paste0("#\n######################  CHECK ON THE USE OF NOTATION KEYS WHERE MOST COUNTRIES REPORT VALUES  for the year ",lastyear,"   #################################"),con)
    write.csv(necheck,con)
}
close(con)

