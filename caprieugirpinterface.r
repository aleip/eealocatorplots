# EU-GIRP
# File caprieugirpinterface.r
# Interface between CAPRI output and EU-GIRP plotting routines
# Purpose: A: 
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
# 12.02.2016
# 


#Set default directory
locplots<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
setwd(locplots)
options(warn=0)

# curplot.r: steering of run
#   a) Load required libraries
#   b) Erase global environment and 
#   c) Define file/folder structure
#   d) Load definitions/sets and functions
#   e) Load existing inventory data
source("curplot.r")

# Import/interface to CARPI data
# As long as no CAPRI data are available, copy allagri
# and multipy with ranodom number
# Work on a subset to save time.

#restrictsector is also in curplot.r defined ...
restrictsector<-c("3.A.1")
restrictsector<-c("3.A.1","3.A.2","3.B.1.3","3.B.2.1","3.D.1")
capinv<-allagri[allagri$sector_number%in%restrictsector,]
multwithrandom<-function(x,D){
    n<-length(D)
    E<-D*runif(n,min=0.9,max=1.1)*runif(1,0.6,1.5)
    #E<-D*runif(1,0.6,1.5)
    return(E)
}

# capinv data frame must have exact column (names) as allagri!
capinv[,years]<-Reduce(rbind,lapply(c(1:nrow(capinv)),function(x) 
    Reduce(cbind,multwithrandom(x,capinv[x,years]))))
capinv$datasource<-"capri"

# Print plots for 'summable data' 
adempars<-c("AD","EM") #Note also areas can be included here

# rundata defines datatype: adem or ief
# adem: activity data, emissions etc
# ief: implied emission factors and other variables expressed per activity unit
rundata<-"ief"
rundata<-"adem"

# runfocus defines type of plot (so far: value, trend, countries..)
# .. however best tested is only 'value' as the other not recently used
runfocus<-"value"

# The vector 'datasource' determines which (and how many) 
datasource<-c("nir")
datasource<-c("nir","capri")


# Still to do
# - EUsum not yet correct for CAPRI (because of random factor; sums need to be calculated before!)
# - Increase page width for ief plots (ranges currently not plotted)
source("eugirp_prepareplots.r")




