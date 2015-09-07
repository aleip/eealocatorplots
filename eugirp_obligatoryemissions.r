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

ok1<-allcat3_ne_em$gas=="CH4" & allcat3_ne_em$sector_number=="3.D"
ok2<-allcat3_ne_em$gas=="CO2" & allcat3_ne_em$sector_number=="3"
ok3<-unlist(lapply(c(1:nrow(allcat3_ne_em)),function(x) length(strsplit(as.character(allcat3_ne_em$sector_number[x]),"\\.")[[1]])>3))
ok3<-ok3 & grepl("^3.F",allcat3_ne_em$sector_number)
allcat3_ne_em<-allcat3_ne_em[!(ok1 | ok2 | ok3),]
if(nrow(ademoutl)>0){write.csv(allcat3_ne_em,file=paste0(invloc,"/checks/checks",cursubm,"NEs.csv"))}

