# Group cattle to 'dairy' and 'non-dairy' for all countries
#curagri<-tempagri

#Remove parent 'Cattle' as we want to keep the differentiation
selection<-curagri$category=="Cattle"
cattle<-curagri[selection,]
curagri<-curagri[!selection,]

#Specific cleaning
selection<-curagri$option=="Option C"&curagri$category=="Other Cattle"
curagri<-curagri[!selection,]
selection<-curagri$party=="PL"&curagri$sector_number=="3.A.1"&curagri$category=="Other Cattle.Non-dairy cattle"
#curagri$sector_number[selection]<-"3.B.1.1"
#curagri$classification[selection]<-"CH4 Emissions"
curagri<-curagri[!selection,]



# Calculate summable items for Dairy and Non-Dairy Cattle
ssummable<-curagri$meastype%in%c("POP","EM","NEXC")
allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
alldairy<-c("Dairy Cattle","Mature Dairy Cattle","Dairy Cows","Other Cattle.Dairy cattle")
allnondairy<-unique(curagri$category[curagri$meastype=="POP"&grepl("3.A.1",curagri$sector_number)&(!curagri$category%in%alldairy)])

cattleuid<-unique(curagri$variableUID[curagri$category%in%allcattle & ssummable])
cattleuid<-unique(curagri[curagri$category%in%allcattle & ssummable & !(grepl("Option",curagri$sector_number)),uniquefields])
cattleuid<-unique(curagri[curagri$category%in%allcattle & ssummable ,uniquefields])
for(cat in allcattle){
    for(sec in c("3.A.1","3.B.1.1","3.B.2.1")){
        for(mea in c("POP","EM","NEXC")){
            for(gas in c("no gas","N2O","CH4","NMVOC")){
                for(sou in unique(allagri$source)){
                    select<-sec==cattleuid$sector_number & 
                        cat==cattleuid$category & 
                        mea==cattleuid$meastype &
                        gas==cattleuid$gas &
                        sou==cattleuid$source
                    if(sum(select)==0){
                        newcuid<-cattleuid[1,]
                        newcuid[,names(newuid)]<-""
                        newcuid$sector_number<-sec
                        newcuid$category<-cat
                        newcuid$meastype<-mea
                        newcuid$gas<-gas
                        newcuid$source<-sou
                        newcuid$variableUID<-newuid(sector = sec,categ = cat,meast = mea,
                                                    units = "",metho = "",sourc = sou,targe = "",
                                                    opti = "",gasun = gas)
                        #print(newcuid)
                        cattleuid<-rbind(cattleuid,newcuid)
                    }
                }
            }
        }
    }
}
cattleuid<-cattleuid[order(cattleuid$sector_number,cattleuid$category),]


sdairy<-curagri$category%in%alldairy[!alldairy%in%"Dairy Cattle"]
dairy<-curagri[sdairy & ssummable,]
aggfields<-dairy[,allfields[!allfields%in%c("category",years,"variableUID")]]
aggvalues<-dairy[,years]
dairyagg<-aggregate(aggvalues,by=as.list(aggfields),sum,na.rm=TRUE)
dairyagg$category<-"Dairy Cattle"
#dairyagg$variableUID<-unlist(lapply(c(1:nrow(dairyagg)),function(x) newuid()))

getcatuid<-function(line,cattleuid){
    selection<-line$sector_number==cattleuid$sector_number & 
        line$category==cattleuid$category & 
        line$meastype==cattleuid$meastype &
        line$gas==cattleuid$gas &
        line$source==cattleuid$source
    uid<-unique(as.character(cattleuid$variableUID[selection]))
    n<-length(uid)
    if(n>1){print(line);print(uid);stop("There is more than one UID")}
    if(n==0){print(line);stop("There is no UID")}
    return(uid)
}

temp<-unlist(lapply(c(1:nrow(dairyagg)),function(x) getcatuid(dairyagg[x,],cattleuid)))
dairyagg$variableUID<-temp
dairyagg<-dairyagg[,allfields]

snondairy<-curagri$category%in%allnondairy[!allnondairy%in%"Non-Dairy Cattle"]
nondairy<-curagri[snondairy & ssummable,]
aggfields<-nondairy[,allfields[!allfields%in%c("category",years,"variableUID")]]
aggvalues<-nondairy[,years]
nondairyagg<-aggregate(aggvalues,by=as.list(aggfields),sum,na.rm=TRUE)
nondairyagg$category<-"Non-Dairy Cattle"
temp<-unlist(lapply(c(1:nrow(nondairyagg)),function(x) getcatuid(nondairyagg[x,],cattleuid)))
nondairyagg$variableUID<-temp
nondairyagg<-nondairyagg[,allfields]

dairyagg$method<-""
nondairyagg$method<-""
curagri<-rbind(curagri,dairyagg,nondairyagg)
ssummable<-curagri$meastype%in%c("POP","EM","NEXC")

# Now sum-up dairy and non-dairy
scattle<-curagri$category%in%allcattle[!allcattle%in%"Cattle"]
cattle<-curagri[scattle & ssummable,]
aggfields<-cattle[,allfields[!allfields%in%c("category",years,"variableUID")]]
aggvalues<-cattle[,years]
cattleagg<-aggregate(aggvalues,by=as.list(aggfields),sum,na.rm=TRUE)
cattleagg$category<-"Cattle"
temp<-unlist(lapply(c(1:nrow(cattleagg)),function(x) getcatuid(cattleagg[x,],cattleuid)))
cattleagg$variableUID<-temp
cattleagg<-cattleagg[,allfields]
cattleagg$method<-""

curagri<-rbind(curagri,cattleagg)
allagricattle112<-curagri
sheepswine<-c("Dairy Cattle","Non-Dairy Cattle")
addparentanimal<-curagri #needs addparentanimal
source("eugirp_aggparentanimal.r")

allagricattle116<-addparentanimal
curagri<-addparentanimal
sheepswine<-c("Cattle")
source("eugirp_aggparentanimal.r")
curagri<-addparentanimal
