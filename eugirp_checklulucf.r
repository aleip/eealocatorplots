
# Check 1: Table 4(I) input of mineral and organic fertilizer to lands
#          Issue: if countries cannot separate they should report IE and report data under 3.D
#          Check: ratio should be small. Flag if ratio > 10%
ninlulucf<-(alldata[alldata$sector_number=="4."&alldata$category=="Direct N2O Emissions from N inputs"&alldata$meastype=="AD",])
ninlulucf<-subset(ninlulucf,select=c("party",years))
minfinagri<-allagri[allagri$meastype=="AD"&(allagri$sector_number=="3.D.1.1" |allagri$sector_number=="3.D.1.2")&allagri$party%in%as.vector(ninlulucf$party),]
minfinagri<-subset(minfinagri,select=c("party",years))
minfinagri<-aggregate(minfinagri[,years],by = list(minfinagri$party),sum)

ninlulucfratio<-function(v1,v2,x){
    if(nrow(v1)==0&nrow(v2)==0){
        ratio<-rep("no agri and lulucf",nyears)
    }else{
        if(nrow(v1)>0){
            if(nrow(v2)>0){
                if(sum(v2,na.rm=TRUE)>0){
                    ratio<-v1/v2
                }else{
                    ratio<-rep("NA",nyears)
                }
            }else{
                ratio<-rep("no agri",nyears)
                ratio<-paste(v1," no agri")
            }
        }else{
            ratio<-rep("no lulucf",nyears)
            ratio<-paste(v2," no lulucf")
        }
    }
    return(ratio)
}
    
nratio<-ninlulucf
nratio[,years]<-Reduce(rbind,lapply(c(1:nrow(ninlulucf)),function(x) 
    (ninlulucfratio(ninlulucf[x,years]/1000000,minfinagri[x,years]))
    ))
nratioflag<-nratio[,years]>0.1


# Check 2: Area of organic soils
# Issue: part of grassland in LULUCF could be NOT cultivated
#        thus the sum of orgsoilincropland+orsoilingrassland >= orgsoilinagri
# Check: flag if sum is ==, >, or <
orgsoilincropland<-(alldata[alldata$sector_number=="4.B"&alldata$measure=="Area of organic soil",])
orgsoilingrassland<-(alldata[alldata$sector_number=="4.C"&alldata$measure=="Area of organic soil",])
tmp<-rbind(orgsoilincropland,orgsoilingrassland)
orgsoilinlulucf<-aggregate(tmp[,years],by=list(tmp$party),sum)
names(orgsoilinlulucf)<-c("party",years)
orgsoilinagri<-allagri[grepl("3.D",allagri$sector_number)&allagri$meastype=="AREA",]
orgsoilinagri[,years]<-orgsoilinagri[,years]/1000

sratio<-Reduce(rbind,lapply(as.vector(unlist(countries)),function(x) 
    ninlulucfratio(orgsoilinlulucf[orgsoilinlulucf$party==x,years],orgsoilinagri[orgsoilinagri$party==x,years],x)
))
sratio$party<-countries$party
names(sratio)<-c(years,"party")
sratio<-sratio[,c("party",years)]

# Check 3: Net carbon stock change in soils in Table 4.B.1
# Issue: Consistency between C losses and N mineralized in Table 3.D
# Check: Caluclate the C/N ratio and flag those which are very different from default (to be checked)
ktcreleasedmineral<-alldata[grepl("4.B.1",alldata$sector_number)&alldata$measure=="Net carbon stock change in soils"&alldata$type=="Mineral soils",]
kgnmineralised<-allagri[grepl("3.D.1.5",allagri$sector_number)&allagri$meastype=="AD",]

cnratio<-Reduce(rbind,lapply(as.vector(unlist(countries)),function(x) 
    ninlulucfratio(ktcreleasedmineral[ktcreleasedmineral$party==x,years],kgnmineralised[kgnmineralised$party==x,years],x)
))
cnratio$party<-countries$party
names(cnratio)<-c(years,"party")
cnratio<-cnratio[,c("party",years)]

write.csv(nratio,file=paste0(invloc,"/checks/lulucf/ratioNinlulucf_vs_ninagri.csv"))
write.csv(sratio,file=paste0(invloc,"/checks/lulucf/ratioOrgsoilsinlulucf_vs_Orgsoilsinagri.csv"))
write.csv(cnratio,file=paste0(invloc,"/checks/lulucf/CNratioCinlulucf_vs_Ninagri.csv"))




