#source("curplot.r")
require(openxlsx)

# Check 1: Table 4(I) input of mineral and organic fertilizer to lands
#          Issue: if countries cannot separate they should report IRL and report data under 3.D
#          Check: ratio should be small. Flag if ratio > 10%
ninlulucf<-(alldata[alldata$sector_number=="4."&alldata$category=="Direct N2O Emissions from N inputs"&alldata$meastype=="AD",])
ninlulucf<-subset(ninlulucf,select=c("party",years))
ninlulucf[is.na(ninlulucf)]<-0
ninlulucf[,years]<-as.numeric(as.matrix(ninlulucf[,years]))
minfinagri<-allagri[allagri$meastype=="AD"&(allagri$sector_number=="3.D.1.1" |allagri$sector_number=="3.D.1.2")&allagri$party%in%as.vector(ninlulucf$party),]
minfinagri<-subset(minfinagri,select=c("party",years))
minfinagri<-aggregate(minfinagri[,years],by = list(minfinagri$party),sum)

ninlulucfratio<-function(x,v1,v2,vy){
  cat(x, " ")
  if(nrow(v1)==0&nrow(v2)==0){
    flag <- "no agri and lulucf"
    ratio <- 0
  }else{
    if(nrow(v1)>0){
      if(nrow(v2)>0){
        if(sum(v2,na.rm=TRUE)>0){
          ratio<-v1/v2
          ratio <- signif(ratio, 3)
        }else{
          ratio<-NA
        }
        flag <- "OK"
      }else{
        flag <-"no agri"
        ratio<-signif(v1, 3)
      }
    }else{
      flag <-"no lulucf"
      ratio<-signif(v2, 3)
    }
  }
  return(list(ratio, flag))
}

nratio<-ninlulucf
nratio[,years]<-Reduce(rbind,lapply(c(1:nrow(ninlulucf)),function(x) 
  (ninlulucfratio(x, ninlulucf[x,years]/1000000,minfinagri[x,years],x)[[1]])
))
nratioflag<-nratio[,years]>0.1


# Check 2: Area of organic soils
# Issue: part of grassland in LULUCF could be NOT cultivated
#        thus the sum of orgsoilincropland+orsoilingrassland >= orgsoilinagri
# Check: flag if sum is ==, >, or <
orgsoilincropland<-(alldata[alldata$sector_number=="4.B"&alldata$measure=="Area of organic soil",])
orgsoilingrassland<-(alldata[alldata$sector_number=="4.C"&alldata$measure=="Area of organic soil",])
tmp<-rbind(orgsoilincropland,orgsoilingrassland)
tmp[,years]<-as.numeric(as.matrix(tmp[,years]))
orgsoilinlulucf<-aggregate(tmp[,years],by=list(tmp$party),sum)
names(orgsoilinlulucf)<-c("party",years)
orgsoilinagri<-allagri[grepl("3.D",allagri$sector_number)&allagri$meastype=="AREA",]
orgsoilinagri[,years]<-orgsoilinagri[,years]/1000

sratio<-Reduce(rbind,lapply(as.vector(unlist(countries)),function(x) 
  ninlulucfratio(x, orgsoilinlulucf[orgsoilinlulucf$party==x,years],orgsoilinagri[orgsoilinagri$party==x,years],x)[[1]]
))
sratio$party<-countries$party
names(sratio)<-c(years,"party")
sratio<-sratio[,c("party",years)]

# Check 3: Net carbon stock change in soils in Table 4.B.1
# Issue: Consistency between C losses and N mineralized in Table 3.D
# Check: Caluclate the C/N ratio and flag those which are very different from default (to be checked)

# Unit in CRF (unchanged): kt C/yr
# (4) The signs for estimates of gains in carbon stocks are positive (+) and for losses in carbon stocks are negative (-).  
ktcreleasedmineral<-alldata[grepl("4.B",alldata$sector_number)&
                              alldata$measure=="Net carbon stock change in soils"&
                              alldata$type=="Mineral soils"&
                              alldata$source=="",]
ktcreleasedmineral<-alldata[grepl("4.B",alldata$sector_number)&alldata$measure=="Net carbon stock change in soils"&alldata$type=="Mineral soils",]
ktcreleasedmineral<-alldata[grepl("4.B.1",alldata$sector_number)&alldata$measure=="Net carbon stock change in soils"&alldata$type=="Mineral soils",]
ktcreleasedmineral <- as.data.table(ktcreleasedmineral)

# Get Carbon stock changes from Cropland remaining Cropland from CRF-files
# The data for mineral soils have been extracted for the years 1990, 2005 and 2018 into a single file for all countries
# Note (email Raul 20200226 - N emissions from land converted to cropland are reported in Table4(III))
# Unit in CRF (unchanged): kt C/yr
# (4) The signs for estimates of 
# gains in carbon stocks are positive # (+) and 
# for losses in carbon stocks are negative (-) ==> Consider only losses  
loadfromcrfs_v1 <- function(){
  table4b <- loadWorkbook(file = crffile)
  cntr <- names(table4b)
  
  cmin <- lapply(1:length(cntr), function(x) {
    data <- as.data.table(read.xlsx(crffile, sheet=cntr[x], startRow = 2))
    data$party <- cntr[x]
    return(data)
  })
  cmin <- Reduce(rbind, cmin)
  cmin <- melt.data.table(cmin, id.vars = c("X1", "party"), variable.name = "years")
  cmin <- cmin[! value %in% c("NO", "NE")]
  cmin <- cmin[, value := as.numeric(value)]
  cmin <- cmin[value < 0]
  cmin <- cmin[X1 != "1. Cropland remaining cropland" ]
  
  cmintot <- cmin[, sum(value), by=.(party, years)]
  ktcreleasedmineral <- dcast.data.table(cmintot, party ~ years, value.var = "V1")
  ktcreleasedmineral$element <- "C"
}
# Load from preprocessed
# Would be good to add an interactive element that asks if this shoudl run (if it hasn't yet for new CRF files)
# or if it has already, then skip.
# Maybe with a safety net (i) with a condition to do at certain dates / after certain periods
# or to wait a certain amount of time then continues in case it runs 'alone' 
source(paste0("prepareCRFs4lulucf.r"))
load(paste0(invloc, "/checks/lulucf/table4B1_Clossesmineralsoils.rdata"))
ktC <- table4bsum
ktC$element <- "C"

# Note in CRF data in kg N/yr. Converted to 1000 t N/yr (dividing by 1000,000)
kgnmineralised <- as.data.table(allagri[grepl("3.D.1.5",allagri$sector_number)&allagri$meastype=="AD",])
ktN <- kgnmineralised[, .SD, .SDcols = c("party", years)]
ktN$element <- "N"

ktCN <- rbind(ktC, ktN)
ktCN <- melt.data.table(ktCN, id.vars = c("party", "element"))
ktCN <- dcast.data.table(ktCN, party + variable ~ element, value.var = "value")
ktCN <- ktCN[, .(party, year=variable, C, N, cnratio = C/N)]
ktCN <- ktCN[! is.na(cnratio)]

cnratio <- dcast.data.table(ktCN, party ~ year, value.var="cnratio")

lulucffile <- paste0(invloc, "/checks/lulucf/table4B1_Clossesmineralsoils.xlsx")
if(file.exists(lulucffile)){unlink(lulucffile)}
wb <- createWorkbook(creator = "Adrian Leip", title = "LULUCF check 2020 Jan", subject="Consistency between C and N losses from mineralization of mineral soils", category="EUGIRP-agricheckshecks")
addWorksheet(wb, sheetName="C and N losses")
addWorksheet(wb, sheetName="CN ratio")
writeData(wb, sheet="CN ratio", x=cnratio)
writeData(wb, sheet="C and N losses", x=ktCN)
saveWorkbook(wb, file=lulucffile, overwrite=TRUE)



cnratio<-Reduce(rbind,lapply(as.vector(unlist(countries)),function(x) {
  aa <- 
    ninlulucfratio(x, ktcreleasedmineral[party==x,years, with=FALSE],
                   kgnmineralised[party==x,years, with=FALSE],x)
  ab <- cbind(aa[[1]])
  ab <- cbind(aa[[2]], aa[[1]])
  names(ab)[1] <- 'flag'
  if(ncol(ab)<(1+length(years))) {
    ab <- as.data.table(t(c("no agri and lulucf", as.numeric(rep(0, length(years))))))
    names(ab)<-c("flag", seq(1990, lastyear, 1))
  }
  cat(ncol(ab))
  return(ab)
}
))
cnratio[is.na(cnratio)]<-"-999"
cnratio <- cnratio[, (years) := lapply(.SD, as.numeric), .SDcols=years]
cnratio[cnratio==-999]<- NA
cnratio$party<-countries$party
yrs <- as.character(seq(1990, lastyear)) 

cnratio <- cnratio[, min := min(.SD, na.rm=TRUE), .SDcols = yrs, by=1:nrow(cnratio)]
cnratio <- cnratio[, max := max(.SD, na.rm=TRUE), .SDcols = yrs, by=1:nrow(cnratio)]

cnratio<-cnratio[, .SD, .SDcols=c("party", "flag", "min", "max", years)]

cnresults <- ktcreleasedmineral[, .SD, .SDcols=c("party", yrs)]
cnresults$element <- "C"
cnresults2 <- kgnmineralised[, .SD, .SDcols=c("party", yrs)]
cnresults2$element <- "N"
cnresults <- rbind(cnresults, cnresults2)


if (! file.exists(paste0(invloc,"/checks/lulucf"))){dir.create(file.path(paste0(invloc,"/checks/lulucf")),showWarnings=FALSE)}
write.csv(nratio,file=paste0(invloc,"/checks/lulucf/ratioNinlulucf_vs_ninagri.csv"))
write.csv(sratio,file=paste0(invloc,"/checks/lulucf/ratioOrgsoilsinlulucf_vs_Orgsoilsinagri.csv"))
write.csv(cnratio,file=paste0(invloc,"/checks/lulucf/CNratioCinlulucf_vs_Ninagri.csv"))

ktcreleasedmineral[party=='AUT', .(source, `1990`, `2000`, `2010`, `2018`)]
cnresults[party=='AUT', .(party, element, `1990`, `2000`, `2010`, `2018`)]

crffile <- paste0(invloc, "/checks/lulucf", "/Croplands_ALL_1990-2005-2018.xlsx")
