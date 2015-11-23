alldata<-alldata[alldata$party!="IS",]
allagri<-allagri[allagri$party!="IS",]
alltotals<-alldata[grepl("^Sector",alldata$sector_number),]
calceu<-alldata
calceu<-eu28sums(calceu)
alldata<-calceu
alltotals<-alldata[grepl("^Sector",alldata$sector_number),]
totalval<-alltotals[alltotals$classification=="Total (with LULUCF  with indirect)",]
View(totalval)
