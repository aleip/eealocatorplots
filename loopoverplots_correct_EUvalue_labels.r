source("curplot.r")
load("C:/Users/adrian/google/projects/eugirp/rdatabase/eealocator_20200508_clean.RData")
datasource<-"nir"
runfocus<-"value"
rundata<-"ief"
#source("eugirp_prepareplots.r")
temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
plotdata<-temp[[1]]
plotmeas<-temp[[2]]
adddefault<-temp[[3]]
sharesexist<-temp[[4]]
eukp <- 'EUC'

namesnoref <- setdiff(names(plotmeas), c("ref2006", "ref1997"))
michaela <- as.data.table(read.xlsx(xlsxFile = "C:/Users/adrian/google/projects/eugirp/20200508/tables4eu/timeseries_Non-Dairy_Cattle.xlsx", 
          sheet = "Sheet1"))

imeas = 10 #114, 276
# Check content
load("C:/dev/ghginventory/eealocatorplots/plotnow.rdata")

# Prepare correct data and run loop for single plots with 'load("a.rdata")' uncommented
a <- michaela[Origin=="Michaela" & party=="EUC" & sector_number=="3.A.1.2", as.character(c(1990:2018)), with=FALSE]
eu28 <- a
save(eu28, file="a.rdata")
loopoverplots(imeas=10, runfocus="range", eusubm="EUC")

b1 <- michaela[Origin=="Michaela" & party=="EUC" & sector_number=="3.B.1.1.2", as.character(c(1990:2018)), with=FALSE]
eu28 <- b1
save(eu28, file="a.rdata")
loopoverplots(imeas=114, runfocus="range", eusubm="EUC")

b2 <- michaela[Origin=="Michaela" & party=="EUC" & sector_number=="3.B.2.1.2", as.character(c(1990:2018)), with=FALSE]
eu28 <- b2
save(eu28, file="a.rdata")
loopoverplots(imeas=276, runfocus="range", eusubm="EUC")
