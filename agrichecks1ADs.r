
# See for which animal types ADs are given in the various tables
allagritab<-unique(subset(allagri,select=uniquefields))
allagri<-allagri[order(allagri$sector_number,allagri$category),]
allagritab<-allagritab[order(allagritab$sector_number,allagritab$category),]

# Identify the animal types available
#agriAanimals<-subset(allagri,grepl("^3.A",sector_number),select=c(sector_number,allmethods,climate))
agriAanimals<-subset(allagri,grepl("^3.A",sector_number),select=c(sector_number,category))
agriAanimals<-unique(agriAanimals)
agriAanimals$sector_number<-gsub("3.A","",agriAanimals$sector_number)

# Methods are lumped together - here the MMS and climate region must be eliminated again
#agriB1animals<-subset(allagri,grepl("^3.B.1",sector_number),select=c(sector_number,allmethods,climate))
agriB1animals<-subset(allagri,grepl("^3.B.1",sector_number),select=c(sector_number,category))
agriB1animals<-unique(agriB1animals)
agriB1animals$sector_number<-gsub("3.B.1","",agriB1animals$sector_number)

#agriB2animals<-subset(allagri,grepl("^3.B.2",sector_number),select=c(sector_number,allmethods,climate))
agriB2animals<-subset(allagri,grepl("^3.B.2",sector_number),select=c(sector_number,category))
agriB2animals<-unique(agriB2animals)
agriB2animals$sector_number<-gsub("3.B.2","",agriB2animals$sector_number)
#allanimals<-merge(allanimals,agriB2animals,by=c("sector_number","category","climate"),all=TRUE)
allanimals<-merge(agriAanimals,agriB1animals,by=c("sector_number","category"),all=TRUE)
allanimals<-merge(allanimals,agriB2animals,by=c("sector_number","category"),all=TRUE)
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
    if(checkuids$APOP[x]!=0) checkA<-T
    if(checkuids$B1POP[x]!=0) checkB1<-T
    if(checkuids$B2POP[x]!=0) checkB2<-T
    if(checkA)   {tmpA <-extractuiddata(allagri,checkuids$APOP[x],countries,noeu = TRUE)}
    if(checkB1)  {tmpB1<-extractuiddata(allagri,checkuids$B1POP[x],countries,noeu = TRUE)}
    if(checkB2)  {tmpB2<-extractuiddata(allagri,checkuids$B2POP[x],countries,noeu = TRUE)}
    
    sec<-checkuids$sector_number[x]
    cat<-checkuids$category[x]
    if(checkA & checkB1) {
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3A","3B1",checkuids$sector_number[x],sep="-"))
        A<-tmpA;B<-tmpB1
        #save.image(file="A-B1.RData")
        diffmatout<-diffmatrix(checks,ncheck,A,B,"POPcheck tables","3A","3B1",sec,cat,roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]
        }
    if(checkA & checkB2) {
        A<-tmpA;B<-tmpB2
        #save.image(file="A-B2.RData")
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3A","3B2",checkuids$sector_number[x],sep="-"))
        diffmatout<-diffmatrix(checks,ncheck,A,B,"POPcheck tables","3A","3B2",sec,cat,roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
    if(checkB1 & checkB2) {
        A<-tmpB1;B<-tmpB2
        #save.image(file="B1-B2.RData")
        #print(paste(checks,ncheck,tmpB1,tmpB2,"POPcheck tables","3B1","3B2",checkuids$sector_number[x],sep="-"))
        diffmatout<-diffmatrix(checks,ncheck,A,B,"POPcheck tables","3B1","3B2",sec,cat,roundn=0)
        ncheck<-diffmatout[[1]]
        checks<-diffmatout[[2]]}
}

checks<-unique(checks)
checks<-simplifytestmatrix(checks,"yr",years)
#checks<-simplifytestmatrix(checks,"ms",as.vector(unlist(allcountries[!allcountries%in%eu])))
checks<-simplifytestmatrix(checks,"val2",NULL)
checks<-checks[order(checks$ms,checks$sec,checks$yr),checkname]
checks<-checks[!grepl("Other",checks$cat),]
