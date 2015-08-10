# See for which animal types ADs are given in the various tables
#allanimals<-merge(cat3Aanimals,cat3B1animals,by=c("sector_number","allmethods","climate"),all=TRUE)
cat3all<-alldata[grepl("^3",alldata$sector_number),allfields]
cat3alltab<-unique(subset(cat3all,select=allfields[!allfields %in% c("notation","party",years)]))
cat3all<-cat3all[order(cat3all$sector_number,cat3all$category),]
cat3alltab<-cat3alltab[order(cat3alltab$sector_number,cat3alltab$category),]

cat3emp<-as.data.frame(matrix(rep(0,ncol(cat3all)),nrow=1,ncol=ncol(cat3all)))
names(cat3emp)<-names(cat3all)
# Identify the animal types available
#cat3Aanimals<-subset(cat3all,grepl("^3.A",sector_number),select=c(sector_number,allmethods,climate))
cat3Aanimals<-subset(cat3all,grepl("^3.A",sector_number),select=c(sector_number,category))
cat3Aanimals<-unique(cat3Aanimals)
cat3Aanimals$sector_number<-gsub("3.A","",cat3Aanimals$sector_number)

# Methods are lumped together - here the MMS and climate region must be eliminated again
#cat3B1animals<-subset(cat3all,grepl("^3.B.1",sector_number),select=c(sector_number,allmethods,climate))
cat3B1animals<-subset(cat3all,grepl("^3.B.1",sector_number),select=c(sector_number,category))
cat3B1animals<-unique(cat3B1animals)
cat3B1animals$sector_number<-gsub("3.B.1","",cat3B1animals$sector_number)

#cat3B2animals<-subset(cat3all,grepl("^3.B.2",sector_number),select=c(sector_number,allmethods,climate))
cat3B2animals<-subset(cat3all,grepl("^3.B.2",sector_number),select=c(sector_number,category))
cat3B2animals<-unique(cat3B2animals)
cat3B2animals$sector_number<-gsub("3.B.2","",cat3B2animals$sector_number)
#allanimals<-merge(allanimals,cat3B2animals,by=c("sector_number","category","climate"),all=TRUE)
allanimals<-merge(cat3Aanimals,cat3B1animals,by=c("sector_number","category"),all=TRUE)
allanimals<-merge(allanimals,cat3B2animals,by=c("sector_number","category"),all=TRUE)
checkuids<-allanimals

cats2check<-c("3.A","3.B.1","3.B.2")
uids2check<-"POP"

for (cat2check in cats2check){
    parn<-paste0(gsub("3","",gsub("\\.","",cat2check)),uids2check)
    checkuids[,parn]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
        # Add "^" and "$" to sec and cat to get the exact values!
        getuid(1,ok=1,,mea=uids2check,gas="no gas",x=x,
               sec=paste0("^",cat2check,checkuids$sector_number[x],"$"),
               cat=paste0("^",checkuids$category[x],"$"))))
}


# Testresults
checks<-checktemp
ncheck<-1
for (x in c(1:nrow(checkuids))){
    #print(paste0("x=",x))
    if(exists("tmpA")) rm(tmpA)
    if(exists("tmpB1")) rm(tmpB1)
    if(exists("tmpB2")) rm(tmpB2)
    checkA<-F;checkB1<-F;checkB2<-F
    if(nchar(checkuids$APOP[x])!=0) checkA<-T
    if(nchar(checkuids$B1POP[x])!=0) checkB1<-T
    if(nchar(checkuids$B2POP[x])!=0) checkB2<-T
    if(checkA)   {tmpA <-extractuiddata(cat3all,checkuids$APOP[x],countries)}
    if(checkB1)  {tmpB1<-extractuiddata(cat3all,checkuids$B1POP[x],countries)}
    if(checkB2)  {tmpB2<-extractuiddata(cat3all,checkuids$B2POP[x],countries)}
    
    if(checkA & checkB1) {
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3A","3B1",checkuids$sector_number[x],sep="-"))
        diffmatout<-diffmatrix(checks,ncheck,tmpA,tmpB1,"POPcheck tables","3A","3B1",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
    if(checkA & checkB2) {
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3A","3B2",checkuids$sector_number[x],sep="-"))
        diffmatout<-diffmatrix(checks,ncheck,tmpA,tmpB2,"POPcheck tables","3A","3B2",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
    if(checkB1 & checkB2) {
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3B1","3B2",checkuids$sector_number[x],sep="-"))
        diffmatout<-diffmatrix(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3B1","3B2",checkuids$sector_number[x],roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
}

checks<-unique(checks)
checks<-simplifytestmatrix(checks,"yr",years)
checks<-simplifytestmatrix(checks,"ms",as.vector(unlist(allcountries)))
checks<-simplifytestmatrix(checks,"val2",NULL)
checks<-checks[order(checks$ms,checks$sec,checks$yr),checkname]

