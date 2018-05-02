# 3.As1: Average gross energy intake, Average CH4 conversion rate
# 3.As2: Weight, feeding situation, Milk yield, work, pregnant, Digestibility of feed, gross energy
# 3.B(a)s1: Allocation by climate region, Typical animal mass, VS daily excretion, CH4 producing potential
# 3.B(a)s2: Allocation by climate region, MCF
# 3.B(b): Nitrogen excretion per MMS
# 3.C: Organic amendments added
# 3.E: Average above-ground biomass density, Fraction of savannah burned, Nitrogen fraction in biomass
# 3.F: Biomass available, Combustion factor


usemeas<-c(meas2mcf)

report<-unique(subset(allagri[allagri$meastype%in%usemeas,],select=which(!names(allagri)%in%c(years,"party"))))
reportad<-unique(allagri[allagri$meastype%in%c("AD","POP","AREA"),c(sectfields,"variableUID")])
#use population data from cat A
reportad<-reportad[!grepl("3.B.1",reportad$sector_number),]
sel<-grepl("3.A",reportad$sector_number)
reportad$sector_number[sel]<-gsub("3.A","3.B.1",reportad$sector_number[sel])
reportuids<-merge(report,reportad,by=sectfields)
reportuids$adpars<-0
names(reportuids)[which(names(reportuids)=="variableUID.y")]<-"aduids"
names(reportuids)[which(names(reportuids)=="variableUID.x")]<-"variableUID"

test<-reportuids[,]
res<-euvalue(todo = "weight",E = test,D = allagri,y = years,c = countriesnoeu)
res<-data.frame(res,row.names = NULL)
names(res)<-years
res<-cbind(test,res)
res$party<-"EU28"

ok<-apply(res[,years],1,sum)
res<-res[!is.na(ok),]



usemeas<-c(meas2clima)

report<-unique(subset(allagri[allagri$meastype%in%usemeas,],select=which(!names(allagri)%in%c(years,"party"))))
reportad<-unique(allagri[allagri$meastype%in%c("AD","POP","AREA"),c(sectfields,"variableUID")])
#use population data from cat A
reportad<-reportad[!grepl("3.B.1",reportad$sector_number),]
sel<-grepl("3.A",reportad$sector_number)
reportad$sector_number[sel]<-gsub("3.A","3.B.1",reportad$sector_number[sel])
reportuids<-merge(report,reportad,by=sectfields)
reportuids$adpars<-0
names(reportuids)[which(names(reportuids)=="variableUID.y")]<-"aduids"
names(reportuids)[which(names(reportuids)=="variableUID.x")]<-"variableUID"

test<-reportuids[,]
resclima<-euvalue(todo = "weight",E = test,D = allagri,y = years,c = countriesnoeu)
resclima<-data.frame(resclima,row.names = NULL)
names(resclima)<-years
resclima<-cbind(test,resclima)
resclima$party<-"EU28"

#correct for errors
resclima[resclima$party=="NLD"&resclima$source!="",years]<-resclima[resclima$party=="NLD"&resclima$source!="",years]/3
resclima[resclima$party=="PRT"&resclima$source!="",years]<-resclima[resclima$party=="NLD"&resclima$source!="",years]/1.2
sel<-resclima$party=="LVA"&resclima$source!=""&resclima$category=="Poultry"
resclima[sel,as.character(c(1990:2009))]<-resclima[sel,as.character(c(1990:2009))]/2
resclima[sel,as.character(c(2010:2013))]<-resclima[sel,as.character(c(2010:2013))]/3


ok<-apply(res[,years],1,sum)
res<-res[!is.na(ok),]
    
euparam<-subset(allagri,party=="EU28"&meastype%in%meas2popweight)
row.names(euparam)<-NULL
euparam[,years]<-round(euparam[,years],5)
digest<-euparam[euparam$sector_number=="3.A.1"&euparam$meastype=="DIGEST"&euparam$category=="Cattle",]
euparam<-unique(euparam)
eucateg<-unique(c(agrimix$category,agridet$category,agrigen$category))
euparam<-euparam[euparam$category%in%eucateg,]

euparam<-rbind(euparam,res[allfields])
euparam<-rbind(euparam,resclima[allfields])
euparam<-euparam[order(euparam$sector_number,euparam$category,euparam$meastype,euparam$target),]

write.table(euparam,file=paste0(csvfil,"_agrieupars.csv"),sep=",")
stop()

#check
checkmeas<-"VSEXC"
cats<-unique(euparam$category[euparam$meastype==checkmeas])
low<-0.1
hig<-2
cur<-allagri[allagri$meastype==checkmeas&allagri$category%in%cats,]
curcheck<-apply(cur[,years],1,mean)<low | apply(cur[,years],1,mean)>hig
View(cur[curcheck,])

checkmeas<-"MCF"
for(cat in euparam$category[euparam$meastype==checkmeas]){
    View(allagri[allagri$meastype==checkmeas&allagri$category==cat,],cat)
}
cats<-unique(agridet$category[agridet$meastype==checkmeas])
for(cat in cats){
    View(allagri[allagri$meastype==checkmeas&allagri$category==cat,],cat)
}


    
