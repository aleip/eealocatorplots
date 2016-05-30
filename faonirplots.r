#source("curplot.r")
#library(ggplot2)
load("../2016/eealocator/eealocator_20160322_clean_plotmeasademnir-fao.RData")
#selection<-plotdata$measure=="Total N excreted"&plotdata$datasource=="fao"
#plotdata[selection,years]<-plotdata[selection,years]/1000000
#selection<-plotdata$meastype=="AREA"&plotdata$datasource=="fao"&grepl("3.C",plotdata$sector_number)
#plotdata[selection,years]<-plotdata[selection,years]/1000000

stop()
runfocus<-"compare"
for (imeas in c(1:nrow(plotmeas))){
    plotcomparison(imeas,plotmeas,plotdata)
}