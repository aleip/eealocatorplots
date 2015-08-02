# See for which animal types ADs are given in the various tables
#allanimals<-merge(cat3Aanimals,cat3B1animals,by=c("sector_number","allmethods","climate"),all=TRUE)
#allanimals<-merge(allanimals,cat3B2animals,by=c("sector_number","allmethods","climate"),all=TRUE)
allanimals<-merge(cat3Aanimals,cat3B1animals,by=c("sector_number"),all=TRUE)
allanimals<-merge(allanimals,cat3B2animals,by=c("sector_number"),all=TRUE)
checkuids<-allanimals

cats2check<-c("3.A","3.B.1","3.B.2")
uids2check<-"AD"

for (cat2check in cats2check){
    parn<-paste0(gsub("3","",gsub("\\.","",cat2check)),uids2check)
    checkuids[,parn]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
        getuid(1,checkuids,ok=1,cat2check,met="",cli="",mymeastype=uids2check,gas="no gas",x=x)))
    checkuids[,parn]<-unlist(lapply(c(1:nrow(checkuids)), function(x) 
        getuid(2,checkuids,ok=checkuids[x,parn],cat2check,met="",cli="",mymeastype=uids2check,gas="no gas",x=x)))
}


# Testresults
checks<-checktemp
ncheck<-1
for (x in c(1:nrow(checkuids))){
    
    if(exists("tmpA")) rm(tmpA)
    if(exists("tmpB1")) rm(tmpB1)
    if(exists("tmpB2")) rm(tmpB2)
    checkA<-F;checkB1<-F;checkB2<-F
    if(nchar(checkuids$AAD[x])!=0) checkA<-T
    if(nchar(checkuids$B1AD[x])!=0) checkB1<-T
    if(nchar(checkuids$B2AD[x])!=0) checkB2<-T
    if(checkA)   {tmpA <-extractuiddata(cat3all,checkuids$AAD[x],allcountries)}
    if(checkB1)  {tmpB1<-extractuiddata(cat3all,checkuids$B1AD[x],allcountries)}
    if(checkB2)  {tmpB2<-extractuiddata(cat3all,checkuids$B2AD[x],allcountries)}
    
    if(checkA & checkB1) {
        diffmatout<-diffmatrix(checks,ncheck,tmpA,tmpB1,"ADcheck tables","3A","3B1",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
    if(checkA & checkB2) {
        diffmatout<-diffmatrix(checks,ncheck,tmpA,tmpB2,"ADcheck tables","3A","3B2",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
    if(checkB1 & checkB2) {
        diffmatout<-diffmatrix(checks,ncheck,tmpB1,tmpB2,"ADcheck tables","3B1","3B2",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
}
checks<-simplifytestmatrix(checks,"yr",years)
checks<-simplifytestmatrix(checks,"ms",as.vector(unlist(allcountries)))
checks<-checks[order(checks$ms,checks$sec,checks$yr),names(checks)]

