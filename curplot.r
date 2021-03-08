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

#if(require(ggplot2)==FALSE){install.packages("ggplot2", repos = "https://cloud.r-project.org"); library(ggplot2)} else {library(ggplot2)}
if(require(reshape2)==FALSE){install.packages("reshape2", repos = "https://cloud.r-project.org"); library(reshape2)} else {library(reshape2)}
#if(require(dplyr)==FALSE){install.packages("dplyr", repos = "https://cloud.r-project.org"); library(dplyr)} else {library(dplyr)}
#if(require(tidyr)==FALSE){install.packages("tidyr", repos = "https://cloud.r-project.org"); library(tidyr)} else {library(tidyr)}
if(TRUE){
if(require(data.table)==FALSE){install.packages("data.table", repos = "https://cloud.r-project.org"); library(data.table)} else {library(data.table)}
if(require(knitr)==FALSE){install.packages("knitr", repos = "https://cloud.r-project.org"); library(knitr)} else {library(knitr)}
if(require(Matrix)==FALSE){install.packages("Matrix", repos = "https://cloud.r-project.org"); library(Matrix)} else {library(Matrix)}
if(require(compare)==FALSE){install.packages("compare", repos = "https://cloud.r-project.org"); library(compare)} else {library(compare)}
if(require(RColorBrewer)==FALSE){install.packages("RColorBrewer", repos = "https://cloud.r-project.org"); library(RColorBrewer)} else {library(RColorBrewer)}
if(require(googledrive)==FALSE){install.packages("googledrive", repos = "https://cloud.r-project.org"); library(googledrive)} else {library(googledrive)}
if(require(zip)==FALSE){install.packages("zip", repos = "https://cloud.r-project.org"); library(zip)} else {library(zip)}
#if(require(httpuv)==FALSE){install.packages("httpuv", repos = "https://cloud.r-project.org"); library(httpuv)} else {library(httpuv)}
#if(require(readxl)==FALSE){install.packages("readxl", repos = "https://cloud.r-project.org"); library(readxl)} else {library(readxl)}
if(require(plyr)==FALSE){install.packages("plyr", repos = "https://cloud.r-project.org"); library(plyr)} else {library(plyr)}
if(require(reshape2)==FALSE){install.packages("reshape2", repos = "https://cloud.r-project.org"); library(reshape2)} else {library(reshape2)}
if(require(openxlsx)==FALSE){install.packages("openxlsx", repos = "https://cloud.r-project.org")}; library(openxlsx)
}

#require(dtplyr)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)
rm(list=setdiff(objects(), c("adrian", "gdrive", "locplots")))
# Define the folder all the process should run, usually the folder of the 
#       current inventory year

if (!grepl("CAPRI", getwd())) {
  mypc<-Sys.info()[4]
  if(mypc=="L01RI1203587"){ #checks machine name
    adrian<-"X:/adrian/"
  }else if(mypc=="D01RI1701864"){
    adrian<-"E:/ghginventory/"
  }else if(mypc=="D01RI1600850"){# Gema PC
    adrian<-"D:\\Users\\carmoge\\Documents\\GitHub\\"
  }else if(mypc=="D01RI1600881"){# Adrian
    adrian<-"\\\\s-jrciprap246p.jrc.it\\dev\\ghginventory/"
  }else if(mypc=="S-JRCIPRAP246P"){ 
    adrian<-"D:\\dev\\ghginventory\\"
  }else if(mypc=="BARAGOON"){ 
    adrian<-"c:\\dev\\ghginventory\\"
  }else if (mypc == 'DESKTOP-B50NUH2'){                  # added by Efisio 
      adrian   <- 'E:\\D_extra\\work\\EU_GIRP\\ghginventory\\'
      iam      <- 'ltEfisio' 
      locplots <- paste0(adrian,"eealocatorplots-master")  
      drive_folder_owner <- "efsolazzo@gmail.com"            # this is my account in this gDrive folder.
      drive_auth(email   =  'eealocatorplots.jrc@gmail.com') # previously authorized account using drive_auth()
      gdrive <- 'eugirp/'
  }else{
    adrian<-"C:/Adrian/"
  }
  #if(Sys.info()["user"] == "rotllxa")  source("https://raw.githubusercontent.com/xavi-rp/xavi_functions/master/xavi_functions.r")
  if(mypc=="D01RI1600881") iam="adrianjrc"
  if(mypc=="L01RI1203587") iam="testcapri"
  if(mypc=="L01RI1203587") iam="adrianlaptop"
  if(mypc=="BARAGOON") iam="adrianbaragoon"
  if(mypc=="S-JRCIPRAP246P") iam="serverJRC"
  if(mypc=="D01RI1701864") iam="PCxavi"
  if(mypc=="D01RI1600850") iam="PCGema"
  eugirpok<-FALSE
  if(grepl("adrian",iam)) eugirpok<-TRUE
  if(grepl("adrian",iam))locplots<-paste0(adrian,"/data/inventories/eealocatorplots")           #!!!
  if(grepl("adrianjrc",iam))locplots<-paste0(adrian,"/eealocatorplots")           #!!!
  if(iam=="testcapri")locplots<-paste0("c:\\ecampa3\\gams\\comparisonplots") 
  if(iam=="serverJRC")locplots<-paste0(adrian,"/eealocatorplots")
  if(iam=="adrianbaragoon")locplots<-paste0(adrian,"/eealocatorplots")
  if(iam=="PCGema")locplots<-paste0(adrian,"/eealocatorplots")
  if(iam=="PCxavi")locplots<-paste0(adrian,"/eealocatorplots")
  #remote2server <- 1
  remote2server <- NULL
  if(!is.null(remote2server)) locplots <- "\\\\s-jrciprap246p.jrc.it\\dev\\ghginventory\\eealocatorplots"
  setwd(locplots)
  searchline<-FALSE
}


##############################################################################
#
#     GENERAL SETTINGS
#
##############################################################################
# Initialisation ####
eugirp.fullname<-"EU-Greenhouse gas Inventory Reporting and Plots"
eugirp.web <- "https://github.com/aleip/eealocatorplots.git"
eugirp.version<-"2.1" #20160531
eugirp.version<-"2.2" #20160820
eugirp.version<-"3.0" #20200315
eugirp.version <- paste0(eugirp.version, " (commit: ", 
                         system("git rev-parse --short HEAD", intern = TRUE), ")")  

if (grepl("CAPRI", getwd())) {  #This need to be generalized!!!!
  setwd(paste0("D:\\dev\\ghginventory\\","/eealocatorplots"))
  setwd("D:/dev/CAPRImodel/Trunk_as_in_repository/gams/comparisonplots")
}


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
cursubm <- "20180122"                                                       #!!!
cursubm <- "20180319"                                                       #!!!
cursubm <- "20180508"                                                       #!!!
cursubm <- "20190115"                                                       #!!!
cursubm <- "20190315"                                                       #!!!
cursubm <- "20190315"                                                       #!!!
cursubm <- "20190508"                                                       #!!!
cursubm <- "20190115"                                                       #!!!
cursubm <- "20200115"                                                       #!!!
cursubm <- "20200315"                                                       #!!!
cursubm <- "20200508"                                                       #!!!

invyear<-2020
# Define location of the *RData files.This is generally NOT in 
#    the same folder of the EU-GIRP tool.

if (!grepl("CAPRI", getwd())) {
  invloc<-paste0(adrian,"google/projects/ecir")#!!!
  invloc<-paste0(adrian,"ecir")#!!!
  if(mypc=="L01RI1203587") invloc<-paste0("C:/Adrian/google/projects/ecir")#!!!
  csvfil <- paste0(locplots,"/../",invyear,"/eealocator/eealocator_",cursubm)   
  csvfil1 <- paste0(locplots,"/../",invyear,"/eealocator/")   
  if(iam=="testcapri")csvfil<-paste0(locplots,"/eealocator_",cursubm)
  if(iam=="testcapri")invloc<-paste0(locplots,"/../../output/results/inventories")
  #if(iam=="serverJRC")invloc<-"\\\\tsclient\\X\\adrian\\google\\projects\\ecir"
  if(iam=="serverJRC")invloc<-"D:\\dev\\ghginventory\\ecir"    #xavi20180125: new path to ecir in the server 
  if(iam=="PCxavi")invloc<-"E:\\ghginventory\\ecir"    #xavi20180125: new path to ecir in xavi's PC 
  if(iam=="PCGema")invloc<-"D:\\Users\\carmoge\\Documents\\GitHub\\ecir"    #gema20181206: new path to ecir in Gema's PC 
  if(!is.null(remote2server)) invloc <- "\\\\s-jrciprap246p.jrc.it\\dev\\ghginventory\\ecir"
}else{
  invloc <- ""
  csvfil <- ""
  csvfil1 <- ""
}
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

# 2019: Keeping Norway data OUT for the plots and for calculating EU averages, but IN for the checks (and agriplots - Step6)
keepNORout <- NULL # this is to keep Norway IN for everithing
keepNORout <- 1

# Settings for plots
# --> number of countries which are listed in the legend
doemissionplots<-FALSE #TRUE/FALSE                                           #!!!
plotformat<-"png"     #Options: pdf, png, jpg                               #!!!
plotformat<-"pdf"     #Options: pdf, png, jpg                               #!!!
plotformat<-"jpg"     #Options: pdf, png, jpg                               #!!!
plotresolution<-400   #Needed for png and jpg (200 is low, 600 high)        #!!!
#plotresolution<-1200   #Needed for png and jpg (200 is low, 600 high)        #!!!
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
if (!grepl("CAPRI", getwd())) {
  generatealldata <- 1
  if(file.exists(rdatallem)){
    #if(file.info(paste0(csvfil,".txt"))$mtime<file.info(rdatallem)$mtime){
    print(paste0("Load existing file ",rdatallem))
    load(rdatallem)    
    generatealldata <- 0
    #}
  }
}
if(Sys.Date() == "2019-05-16") country4sub$thename<-as.vector(sapply(country4sub$name,function(x) if(x%in%c("Czech Republic","Netherlands","United Kingdom")){paste0("the ",x)}else{x}))
source("eugirp_definitions.r")


##############################################################################
#
#       Additional settings to be done...
#
##############################################################################
if(exists("alldata")){
    years<-names(alldata)[grepl("^[12]",names(alldata),perl=TRUE)]
    yearsnum<-as.numeric(years)
    nyears<-length(years)
    firstyear<-years[1]
    lastyear<-years[length(years)]
    countries<-unique(subset(alldata,select=party))
    allcountries<-as.character(countries$party)
    x<-allcountries[order(allcountries)]
    eucountries<-c("EU28","EU29")
    allcountries<-c(x[!(x %in% eucountries)],x[ (x %in% eucountries)])
    countriesnoeu<-allcountries[!allcountries %in% eucountries]
    countrynames<-unlist(lapply(c(1:length(allcountries)),function(x) countriesl[which(countries3==allcountries[x])]))

    countries<-as.data.frame(allcountries)
    names(countries)<-"party"
    countriesnoic<-allcountries[!allcountries %in% "IC"]
    countriesic<-allcountries 
    
    # Explanations for the outlier files to be reported back to countries
    # method 2 : box-whisker methods (from outlier tool)
    trendoutlmethod<-3
    if(trendoutlmethod==2)bxplf <- 1.5*0.953 #
    if(trendoutlmethod==3)bxplf <- 1.5 #times standard deviation
    #if(trendoutlmethod==3)bxplf <- 2 #times standard deviation

    
    maxr<-5
    source("eugirp_texts.r")    
}


##############################################################################
#       Additional settings for Uncertainty calculations
##############################################################################

yr <- c(2015:2018)
yr <- c(2017)
yr <- invyear

#options(error=NULL) #error=recover goes into debug mode


##############################################################################
#       Additional settings for CAPRI comparison (only for NIR Chapter 5)
##############################################################################

invloc2 <- "D:\\dev\\CAPRImodel\\Trunk_as_in_repository\\gams\\comparisonplots/"
if(!is.null(remote2server)) invloc2 <- "\\\\s-jrciprap246p.jrc.it\\dev\\CAPRImodel\\Trunk_as_in_repository\\gams\\comparisonplots/"
capri_comp_plots <- "D:\\dev\\CAPRImodel\\CAPRI_R_TRUNK_15022019\\results\\inventories/"
if(!is.null(remote2server)) capri_comp_plots <- "\\\\s-jrciprap246p.jrc.it\\dev\\CAPRImodel\\CAPRI_R_TRUNK_15022019\\results\\inventories/"




