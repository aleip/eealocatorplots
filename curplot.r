# File curplot.csv
# File required for the EU-GIRP R script eealocatorplots.r
# Purpose: steers the R script defining which plots need to be generated
#          see below for information how to use this file!!
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
#
# Content of file: 
#   - general settings
#   - definition of plot-requests for EU-GIRP
#
# All parameters that MUST be adapted by the user are indicated by an #!!! at the and of the line

require(ggplot2) #for graphics
require(reshape2) #required for melt function - for outls
require(dplyr) #for pipes and many data manipulation functions
require(data.table)
#require(dtplyr)
require(knitr)
require(Matrix)
require(compare)
require(RColorBrewer)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)
rm(list=objects())

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
mypc<-Sys.info()[4]
if(mypc=="L01RI1203587"){ #checks machine name
    adrian<-"c:/Adrian/"
}else if(mypc=="D01RI1600881"){
    adrian<-"x:/Adrian/"
}else{
    adrian<-"C:/Adrian/"
}
if(mypc=="D01RI1600881") iam="adrianjrc"
if(mypc=="L01RI1203587") iam="testcapri"
if(mypc=="L01RI1203587") iam="adrianlaptop"
eugirpok<-FALSE
if(grepl("adrian",iam)) eugirpok<-TRUE
if(grepl("adrian",iam))locplots<-paste0(adrian,"/data/inventories/ghg/unfccc/eealocatorplots")           #!!!
if(iam=="testcapri")locplots<-paste0("c:\\ecampa3\\gams\\comparisonplots") 
   
setwd(locplots)
searchline<-FALSE

##############################################################################
#
#     GENERAL SETTINGS
#
##############################################################################
# Initialisation ####
eugirp.fullname<-"EU-Greenhouse gas Inventory Reporting and Plots"
eugirp.web<-"https://github.com/aleip/eealocatorplots.git"
eugirp.version<-"2.1" #20160531
eugirp.version<-"2.2" #20160820
# Define current submission.
cursubm <- "20151030"                                                       #!!!
cursubm <- "20160202"                                                       #!!!
cursubm <- "20160322"                                                       #!!!
cursubm <- "20160420"                                                       #!!!
cursubm <- "20160810"                                                       #!!!
cursubm <- "20170123"                                                       #!!!
cursubm <- "20170317"                                                       #!!!
cursubm <- "20170509"                                                       #!!!
cursubm <- "20171011"                                                       #!!!
invyear<-2017
# Define location of the *RData files.This is generally NOT in 
#    the same folder of the EU-GIRP tool.
invloc<-paste0(adrian,"google/projects/ecir")                                               #!!!
csvfil <- paste0(locplots,"/../",invyear,"/eealocator/eealocator_",cursubm)   
if(iam=="testcapri")csvfil<-paste0(locplots,"/eealocator_",cursubm)
if(iam=="testcapri")invloc<-paste0(locplots,"/../../output/results/inventories")
#!!!
# Years to be used (adapt the last year at the 
# beginning of each inventory-cycle)
years2keep<-c(1990:(invyear-2))
signyear<-years2keep[length(years2keep)]
signclass<-"Total (with LULUCF  with indirect)"
signclass<-"Sectors/Totals"
signtype<-"Total (with LULUCF)"
signtype<-"Total (without LULUCF)"
if(signclass=="Total (with LULUCF  with indirect)") signtype<-""

figdate<-format(Sys.time(), "%Y%m%d")
issuedir<-paste0(invloc,"/checks/")
plotsdir<-paste0(invloc,"/plots/test/")
plotsdir<-paste0(invloc,"/plots/")
if(iam=="testcapri")plotsdir<-invloc
plotsdir<-gsub("\\\\","/",plotsdir)
faodir<-paste0(invloc,"/faocomparison/")
if (! file.exists(issuedir)){dir.create(file.path(issuedir),showWarnings=FALSE)}
rdatallem <- paste0(csvfil,"_clean.RData")
rdatmeasu <- paste0(csvfil,"_measures.RData")
rdatmeta <- paste0(csvfil,"_metadata.RData")
rdatagri <- paste0(csvfil,"_agri.RData")
lastkeyfile<-"keycategories~20151012.csv"
lastkeyfile<-"keycategories~20160606.csv"
lastkeyfile<-paste0(issuedir,"keycatetgories/",lastkeyfile)
excludeparty<-NULL
eusubm<-"EUC"

# Settings for plots
# --> number of countries which are listed in the legend
doemissionplots<-FALSE #TRUE/FALSE                                           #!!!
plotformat<-"jpg"     #Options: pdf, png, jpg                               #!!!
plotresolution<-400   #Needed for png and jpg (200 is low, 600 high)        #!!!
restrictsector<-""
restrictcategory<-""
topn<-10

##############################################################################
#
#       Load functions and definitions required for the calculations
#
#       Reload data if already calculated
#
##############################################################################
# Load functions
source("eugirp_functions.r")
# Load general definitions
source("eugirp_definitions.r")
generatealldata <- 1
if(file.exists(rdatallem)){
    #if(file.info(paste0(csvfil,".txt"))$mtime<file.info(rdatallem)$mtime){
    print(paste0("Load existing file ",rdatallem))
    load(rdatallem)    
    generatealldata <- 0
    #}
}

##############################################################################
#
#       Additional settings to be done...
#
##############################################################################
if(exists("alldata")){
    years<-names(alldata)[grepl("^[12]",names(alldata),perl=TRUE)]
    yearsnum<-as.numeric(years)
    nyears<-length(years)
    lastyear<-years[length(years)]
    countries<-unique(subset(alldata,select=party))
    allcountries<-as.character(countries$party)
    x<-allcountries[order(allcountries)]
    eucountries<-c("EU28","EU29")
    allcountries<-c(x[!(x %in% eucountries)],x[ (x %in% eucountries)])
    countriesnoeu<-allcountries[!allcountries %in% eucountries]
    countrynames<-unlist(lapply(c(1:length(allcountries)),function(x) countriesl[which(countries2==allcountries[x])]))

    countries<-as.data.frame(allcountries)
    names(countries)<-"party"
    countriesnoic<-allcountries[!allcountries %in% "IC"]
    countriesic<-allcountries 
    
    # Explanations for the outlier files to be reported back to countries
    # method 2 : box-whisker methods (from outlier tool)
    trendoutlmethod<-3
    if(trendoutlmethod==2)bxplf <- 1.5*0.953 #
    if(trendoutlmethod==3)bxplf <- 1.5 #times standard deviation
    
    
    maxr<-5
    source("eugirp_texts.r")    
}
