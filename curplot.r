# EU-GIRP (EU-Greenhouse gas Inventory Reporting and Plots; eealocatorplots)
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

library(ggplot2) #for graphics
library(reshape2) #required for melt function - for outls
library(dplyr) #for pipes and many data manipulation functions
library(data.table)
library(knitr)
library(compare)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)
rm(list=objects())

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
locplots<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"           #!!!
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
eugirp.version<-"2.0"
# Define current submission.
cursubm <- "20151030"                                                       #!!!
invyear<-2015
cursubm <- "20160202"                                                       #!!!
cursubm <- "20160322"                                                       #!!!
invyear<-2016
# Define location of the *RData files.This is generally NOT in 
#    the same folder of the EU-GIRP tool.
invloc<-paste0("../",invyear)                                               #!!!
csvfil <- paste0(invloc,"/eealocator/eealocator_",cursubm)                  #!!!
# Years to be used (adapt the last year at the 
# beginning of each inventory-cycle)
years2keep<-c(1990:2014)
signyear<-years2keep[length(years2keep)]
signclass<-"Total (with LULUCF  with indirect)"
excludeparty<-"UK"

figdate<-format(Sys.time(), "%Y%m%d")
issuedir<-paste0(invloc,"/checks/")
plotsdir<-paste0(invloc,"/plots/test/")
plotsdir<-paste0(invloc,"/plots/")
if (! file.exists(issuedir)){dir.create(file.path(issuedir),showWarnings=FALSE)}
rdatallem <- paste0(csvfil,"_clean.RData")
rdatmeasu <- paste0(csvfil,"_measures.RData")
rdatmeta <- paste0(csvfil,"_metadata.RData")
rdatagri <- paste0(csvfil,"_agri.RData")
lastkeyfile<-"keycategories~20151012.csv"
lastkeyfile<-paste0(issuedir,"keycatetgories/",lastkeyfile)

# Settings for plots
# --> number of countries which are listed in the legend
doemissionplots<-TRUE #TRUE/FALSE                                           #!!!
plotformat<-"jpg"     #Options: pdf, png, jpg                               #!!!
plotresolution<-250   #Needed for png and jpg (200 is low, 600 high)        #!!!
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
