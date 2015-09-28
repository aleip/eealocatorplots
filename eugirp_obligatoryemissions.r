con<-file(paste0(invloc,"/eealocator/IPCC_variables_MMR_560.xml"))
open(con)
dataList <- list()
ecdfList <- list()
obluids<-vector()
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
    myVector <- (strsplit(oneLine, ">"))
    if(grepl("uid",myVector[[1]][1])){
        curuid<-gsub("</uid","",myVector[[1]][2])
        obluids<-c(obluids,curuid)
    }    
    
} 
close(con)
obluids<-unique(obluids)
emuidscat3<-as.character(unique(alldata$variableUID[alldata$measure=="Emissions"&grepl("^3",alldata$sector_number)]))
emuidsnot3<-as.character(unique(allnotations$variableUID[allnotations$measure=="Emissions"&grepl("^3",allnotations$sector_number)]))
emuidscat3<-unique(c(emuidscat3,emuidsnot3))
obluidscat3<-obluids[obluids%in%emuidscat3]
obldatacat3<-alldata[alldata$variableUID%in%obluidscat3,]

oblnotcat3<-allnotations[allnotations$variableUID%in%obluidscat3,]
oblcat3_ne<-oblnotcat3[grepl("NE",oblnotcat3$notation),]

allnotcat3<-allnotations[grepl("^3",allnotations$sector_number),]
allcat3_ne<-allnotcat3[grepl("NE",allnotcat3$notation),]
allcat3_ne<-allcat3_ne[order(allcat3_ne$sector_number,allcat3_ne$category),]
allcat3_ne_em<-allcat3_ne[allcat3_ne$measure=="Emissions",]
namesnecheck<-names(allcat3_ne_em)
nechecknames<-c("party","notation","sector_number","category","gas","unit","year",
                "Issue.nr","flag","Last.action.date","Last.action","comments",
                "variableUID","value","classification","source","target","method","option","type",
                "measure")

ok1<-allcat3_ne_em$gas=="CH4" & allcat3_ne_em$sector_number=="3.D"
ok2<-allcat3_ne_em$gas=="CO2" & allcat3_ne_em$sector_number=="3"
ok3<-unlist(lapply(c(1:nrow(allcat3_ne_em)),function(x) length(strsplit(as.character(allcat3_ne_em$sector_number[x]),"\\.")[[1]])>3))
ok3<-ok3 & grepl("^3.F",allcat3_ne_em$sector_number)
allcat3_ne_em<-allcat3_ne_em[!(ok1 | ok2 | ok3),]

checked<-read.csv(file=paste0(invloc,"/checks/nechecks/NEchecks.csv"),header = TRUE,comment.char = "#")
select<-is.na(checked$source)
checked$source[select]<-""
select<-is.na(checked$target)
checked$target[select]<-""
select<-is.na(checked$method)
checked$method[select]<-""
select<-is.na(checked$option)
checked$option[select]<-""
necheck<-merge(checked,allcat3_ne_em,by=namesnecheck,all=TRUE)

# The 'no issues' can be removed directly
nechecksclosed<-necheck[necheck$flag == "cn",nechecknames]
nechecksclosed<-nechecksclosed[!is.na(nechecksclosed$party),]
nechecksopen<-necheck[necheck$flag != "cn",nechecknames]
nechecksopen<-nechecksopen[!is.na(nechecksopen$party),]

o<-order(nechecksopen$party,nechecksopen$sector_number,nechecksopen$category)
nechecksopen<-nechecksopen[o,nechecknames]

o<-order(nechecksclosed$Last.action.date)
nechecksclosed<-nechecksclosed[o,nechecknames]

con<-file(paste0(invloc,"/checks/nechecks/necheck",cursubm,".csv"), open="wt")
writeLines(docflags,con)
if(nrow(nechecksopen)>0){
    writeLines("#\n# Open Issues",con)
    write.csv(nechecksopen,con)
}
if(nrow(nechecksclosed)>0){
    writeLines("#\n#\n# Closed Issues",con)
    write.csv(nechecksclosed,con)
}
close(con)


