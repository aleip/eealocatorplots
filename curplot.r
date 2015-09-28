# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
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

library(ggplot2)
library(reshape2) #required for melt function - for outls
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
# Define current submission.
cursubm <- "20150903"                                                       #!!!
# Define location of the *RData files.This is generally NOT in 
#    the same folder of the EU-GIRP tool.
invyear<-2015
invloc<-paste0("../",invyear)                                               #!!!
csvfil <- paste0(invloc,"/eealocator/eealocator_",cursubm)                  #!!!
# Years to be used (adapt the last year at the 
# beginning of each inventory-cycle)
years2keep<-c(1990:2013)

figdate<-format(Sys.time(), "%Y%m%d")
issuedir<-paste0(invloc,"/checks/")
if (! file.exists(issuedir)){dir.create(file.path(issuedir),showWarnings=FALSE)}
rdatallem <- paste0(csvfil,"_clean.RData")
rdatmeasu <- paste0(csvfil,"_measures.RData")
rdatmeta <- paste0(csvfil,"_metadata.RData")
rdatagri <- paste0(csvfil,"_agri.RData")

# Settings for plots
# --> number of countries which are listed in the legend
doemissionplots<-FALSE #TRUE/FALSE                                           #!!!
plotformat<-"jpg"     #Options: pdf, png, jpg                               #!!!
plotresolution<-200   #Needed for png and jpg (200 is low, 600 high)        #!!!
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
    countries<-unique(subset(alldata,select=party))
    allcountries<-as.character(countries$party)
    x<-allcountries[order(allcountries)]
    eucountries<-c("EU28","EU29")
    allcountries<-c(x[!(x %in% eucountries)],x[ (x %in% eucountries)])
    countriesnoeu<-allcountries[!allcountries %in% eucountries]

    countries<-as.data.frame(allcountries)
    names(countries)<-"party"
    countriesnoic<-allcountries[!allcountries %in% "IC"]
    countriesic<-allcountries 
    
    # Explanations for the outlier files to be reported back to countries
    trendoutlmethod<-3
    maxr<-5
    filoutliers<-paste0(invloc,"/checks/countryoutliers/checks",cursubm,"_countryoutliers.csv")
    colexpl1<-paste0("#\n# Explanation of column names (different from CRF-dimensions)\n",
                    "# min-max: minimum (ignoring reported zeroes) and maximum values")
    whisksexpl<-paste0("# lwhisk-uwhisk: lower and upper 'whisker'. Values below the lower whisker",
                    " or above the upper whisker are considered as outliers.\n",
                    " The whiskers are determined to cover ca. 80% of values when they were normally distributed.")
    lulimexpl<-paste0("#llim-ulim: lower and upper limit of values not considered as outliers.")
    colexpl2<-paste0("# p25-median-p75: 25percentile-50percentile (median)-75percentile ",
                    "of the distribution of growth rates for the variable in the ",
                    "country over time period",years[1],"-",years[length(years)],"\n",
                    "# range: max:min ratio - zeros excluded\n",
                    "# value (trend-outlier): list of outlier values identified-space separated\n",
                    "# value (country-outlier): average value of outlier values identified\n",
                    "# years: list of years for which outlier values were identified\n")
    colexpl3<-paste0("# resolved-flag: 0=not resolved - 1=resolved - 2=not significant (provide reason) - 3=parent (explanation given in more detailed categories)\n",
                     "# explanation date and source: meta information about the issue: what is the explanation-when has it been solved and what was the source of information")
    coutlexp1<-paste0("# File created by EU-GIRP v.4 on ",figdate,"\n",
                     "# List of country outliers. Outliers were identified with the following criteria")
    outlmethod2<-paste0("# (1) Country value outside the range of median +/- 1.953 x (median-75percentile/25percentile)")
    outlmethod3<-paste0("# (1) Country value outside the range of mean +/- 1.5 standard deviations")
    coutlexp2<-paste0("#      of the values reported for the variable during the time period and ",
                     "by all countries by which this variable is reported\n",
                     "# (2) To exclude minimim deviations though the country values ",
                     "which are not more different that 10% from the median ",
                     "are NOT considered as outliers\n",
                     "# (3) A wide distribution indicates systematic errors (e.g. wrong unit used by MS(s))")
    
}
