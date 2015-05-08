library(ggplot2)
library(reshape2) #required for melt function - for outls
library(data.table)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)
rm(list=objects())
#dev.off()

#Link with EEA locator tool
#########################################

# Define if data from xls-file need to be updated (1) or not (0)
#        Note that the update requires sign. time.
loadnew <- 0

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
invyear<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
csvfil <-"eealocatortest_2014_AllAllAll.csv"
csvfil <- "EEA_GHG_MMR_locator_20150323.cub_2015.csv"
csvfil <- "eealocator_20150115_20150401.txt"
setwd(invyear)

# CSV file generated as follows:
# - Load EEA locator cube in excel pivot table
#   - Report Filter: Submission Year
#   - Column Label: Inventory Year
#   - Row Labels: Variable name, Party code, Notation key
# Export table to csv eealocatortest.csv
# Process csv with eealocator_csv.bash to alldata_allbrf.csv
#   Main tasks
#   - Replace long names with acronyms (defined in excel file meta_data_dimensions.xlsx
#     which needs to be exported as csv meta_data_dimensions.csv)
#   - Combine dimension-columns and remove unnecessary ones
#     Currently: 1. category and source (categorysource)
#                2. measure and gas (measuregas)
#                3. unit
#                4. party
#                5. values for years 1990ff (not those before 1990)
alldata<-read.csv(csvfil,na.string="-999")

# Selection: keep all category 4 and all emissions
#select <- (substr(alldata$sector_number,1,1)==3) | 
#    (alldata$measure == "Emissions")
select <- (substr(alldata$sector_number,1,1)==3) 
alldata<-alldata[select,]
# Select gases
gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O","NOx")
#selectgas<-
allgases<-sort(unique(alldata$gas))
# Create vectors for categories, years, and unique rows (not considering years)
# allcategories is created as 'factor'
allcategories<-sort(unique(alldata$sector_number))
# allyears is created as 'integer'
allyears<-sort(unique(alldata$year))
allsources<-sort(unique(alldata$source))
allmethods<-sort(unique(alldata$method))
alltargets<-sort(unique(alldata$target))
alloptions<-sort(unique(alldata$option))
allnotations<-sort(unique(alldata$notation))
alltypes<-sort(unique(alldata$type))
allmeasures<-sort(unique(alldata$measure))

# ---> Fields giving additional info on the 'method'
fields2merge<-sort(c("source","method","target","option","type"))

fields2keep<-c("sector_number","category","gas","unit","allmethods","party","meastype")

# ---> Fields that will be needed for information but they are not needed 
#      to identify the cells
fields4info<-sort(c("notation","classification","country_name"))
# ---> Fields that are identical per country 
fields4coun<-c("submission_version","submission_year")
listofuniquefields<-sort(c(fields2keep,fields2merge))

# -----------------------------------------------------------------------------
# Keep long text with the exception of 'measure' which is needed to identify
# if it is an activity data, emissions, emission factor or parameter or other
# -----------------------------------------------------------------------------
#alldata$measurelong<-alldata$measure
measureacronyms<-read.csv("metadim_row8measure.txt")
temp1<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
alldata<-subset(temp1,select=-dummy)
#          measureacronyms[measureacronyms[,"name"]==alldata$measure,"type"])
# temp1<-tapply(1:nrow(alldata),alldata$measure,function(x) 
#     cbind(x,alldata[x,0],if(alldata$measure[x]=="Emissions"){"EM"}else 
#         if(alldata$measure[x]=="Implied emission factor"){"IEF"}else
#             if(alldata$measure[x]=="Method"){"METHOD"}else
#                 if(! is.na(match("information",strsplit(as.character(alldata$measure[x])," ")[[1]]))){"INFO"}else
#                 {"AD"}))
# temp2<-Reduce(rbind,temp1)
# temp2<-subset(temp2,select=-x)
# names(temp2)<-"measureshort"
# alldata<-merge(alldata,temp2,by.x=0,by.y=0)

# -----------------------------------------------------------------------------
# Replace all 'method' fields which will not be needed individually with
# a field where the content is concantenated and simplified
# -----------------------------------------------------------------------------
alldatauniq<-lapply(fields2merge,function(x) as.vector(alldata[,x]))
alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))

# ---> Concetenate
alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
# ---> Eleminate the 'no' texts .... they do not add information
no<-"no method_|no option_|no source_|no target_|no type_|Option A_"
t<-gsub(no,"",alldatauniq)
alldatauniq<-as.data.frame(gsub("_$","",t))
listofoptions<-unique(alldatauniq)
names(alldatauniq)<-"allmethods"
alldata<-cbind(alldata,alldatauniq)
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2merge])
#View(alldatauniq)
#View(t)

# -----------------------------------------------------------------------------
# Replace all fields which are needed to identify the rows (except year)
# a field where the content is concantenated and simplified
# -----------------------------------------------------------------------------
alldatauniq<-lapply(fields2keep,function(x) as.vector(alldata[,x]))
alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))
alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
alldatauniq<-gsub("__","_",alldatauniq)
alldatauniq<-as.data.frame(gsub("_$","",alldatauniq))
names(alldatauniq)<-"unique"
listofuniq<-unique(alldatauniq)
alldata<-cbind(alldata,alldatauniq)

# -----------------------------------------------------------------------------
# Store away columns not needed immediately together with unique fields
# -----------------------------------------------------------------------------
allnotations<-alldata[alldata[,"meastype"]=="METHOD",]
alldata<-alldata[alldata[,"meastype"]!="METHOD",]
subfields<-c(fields4info[! fields4info %in% "notation"],"value")
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% subfields])
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields4coun])
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields2keep])

#allinfos<-alldata[alldata[,"meastype"]=="INFO" | alldata[,"allmethods"]=="Additional Information",]
#alldata <-alldata[alldata[,"meastype"]!="INFO" & alldata[,"allmethods"]!="Additional Information",]
allinfos<-alldata[alldata[,"meastype"]=="INFO",]
alldata <-alldata[alldata[,"meastype"]!="INFO",]
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4info])
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4coun])
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields2keep])

alldata4info<-subset(alldata,select=c(fields4info,"unique"))
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4info])

alldata4coun<-subset(alldata,select=c(fields4coun,"unique"))
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4coun])

measname<-as.matrix(unique(subset(alldata,select=c("meastype","measnameshort","sector_number","measure"))))
#alldata<-subset(alldata,select=-measname)

alldata4uniq<-subset(alldata,select=c(fields2keep,"unique"))
alldata4uniq<-unique(alldata4uniq)
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2keep])

# -----------------------------------------------------------------------------
#Generate data frame with one columns per year and the value in the corresponding column
#zeros in all other columns
#http://stackoverflow.com/questions/11350537/convert-a-factor-column-to-multiple-boolean-columns
# Alternative method could be: #http://stackoverflow.com/questions/9084439/r-colsums-by-group
# -----------------------------------------------------------------------------
temp1<-as.data.frame(model.matrix(~factor(year)-1,data=alldata)*alldata$value)
temp2<-cbind(alldata,temp1)
names(temp2)<-c(names(alldata),allyears)
temp2<-subset(temp2,select=c("unique",allyears))

# seems to work better: https://stat.ethz.ch/pipermail/r-help/2008-March/157414.html
#temp3<-lapply(split(temp1,temp1$category),function(x) colSums(x[,-1]))
alldatanames<-as.vector(sort(unique(temp2$unique)))
temp3<-lapply(split(temp2,temp2$unique,drop=TRUE),function(x) colSums(x[,-1],na.rm=TRUE))
temp4<-as.data.frame(Reduce(cbind,temp3))
names(temp4)<-alldatanames
temp4<-as.data.frame(t(temp4))

alldata<-merge(alldata4uniq,temp4,by.x="unique",by.y=0)

toremove<-c("temp1","temp2","temp3","temp4")
rm(toremove)



# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
source("lists.txt")

# Load the current tasks to do
# See file curplot.csv for information on how to set it up
curtasks <- readLines("curplot.csv")
cursubm <- "201503"
starttasks<-0
dotrendoutl<-1
adempars<-c("AD","EM")
for (icurtasks in 1:length(curtasks)){
    #icurtasks<-33
    curtask<-curtasks[icurtasks]
    if(starttasks==1 & ! substr(curtask,1,1)=="#"){
        curtaskdetails<-unlist(strsplit(curtask,","))
        
        print("# Basic selection: souce cagegory and gas")
        curcateg<-curtaskdetails[1]
        cursourc<-curtaskdetails[2]
        curgases<-curtaskdetails[3]
        print(paste(curtask,curcateg,cursourc,curgases))
        
        
        focus<-as.data.frame(matrix(NA, nrow=10, ncol=4))
        names(focus)<-c("ad","parameter","datatype","focus")
        nfocus<-1
        
        ################################################################################
        #Clean up memory to avoid
        torm1<-c("actcount","categorymatrix","eealocator")
        torm2<-torm1[torm1 %in% ls()]
        if(length(torm2)>0){rm(list=torm2)}
        
        ################################################################################
        #First create data frame with AD
        tmp0<-subset(alldata,sector_number==curcateg,select=-c(sector_number,unique));
        categorymatrix<-subset(tmp0,category==cursourc,select=-category);
        
        #Remove rows which are zero for all parties
        categorymatrix<-categorymatrix[categorymatrix[,"meastype"]!="",]
        categorymatrix<-categorymatrix[categorymatrix[,"meastype"]!="INFO",]
        
        curunits<-as.vector(unique(subset(categorymatrix,select=unit))[,1])
        curparts<-as.vector(unique(subset(categorymatrix,select=party))[,1])
        curmeasu<-as.vector(unique(subset(categorymatrix,select=meastype))[,1])
        
        tmp1<-subset(categorymatrix,select=-c(gas,unit,party,meastype,allmethods))
        years<-as.numeric(gsub("X","",gsub("X","",colnames(tmp1))))
        nyears<-length(years)
        #colnames(categorymatrix)[7:(6+nyears)]<-years
        
        # Analyse available measures
        # List of measures which are not numerical
        listnomeasures<-c("INFO","METHOD",NA)
        curmeasu<-curmeasu[! curmeasu %in% listnomeasures]
        
        if("AD" %in% curmeasu){
            tmp1<-subset(categorymatrix,meastype=="AD",select=-meastype)
            unitad<-as.vector(unique(subset(tmp1,select=unit))[,1])
            tmp2<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods))
            actcount<-unique(tmp2)
            acteu28<-colSums(actcount)
            rownames(actcount)<-curparts
            colnames(actcount)<-years
        }
        
        if("EM" %in% curmeasu){
            tmp1<-subset(categorymatrix,meastype=="EM",select=-meastype)
            unitem<-as.vector(unique(subset(tmp1,select=unit))[,1])
            tmp2<-subset(tmp1,gas==curgases,select=c(-gas,-unit,-party,-allmethods))
            emicount<-unique(tmp2)
            emieu28<-colSums(emicount)
            rownames(emicount)<-curparts
            colnames(emicount)<-years
        }
        
        if("IEF" %in% curmeasu){
            tmp1<-subset(categorymatrix,meastype=="IEF",select=-meastype)
            unitief<-as.vector(unique(subset(tmp1,select=unit))[,1])
            tmp2<-subset(tmp1,gas==curgases,select=c(-gas,-unit,-party,-allmethods))
            iefcount<-unique(tmp2)
            iefeu28<-colSums(iefcount*actcount)/acteu28
            rownames(iefcount)<-curparts
            colnames(iefcount)<-years
        }
        
        print("#Generate matrix 'focus' of tasks from curplot.csv indicating: ")
        # - Activity data (are required for AD plots and IEF plots for weighting)
        # - Parameter to plot (AD, EM, IEF, or other factor)
        # - Data type (determined automatically: summable (AD) or averageable (IEF))
        # - Focus: value plot (as trend), 
        #          trend plot (as inter-annual changes), 
        #          country plot (range as values from trend)
        if(curtaskdetails[4]=="all"){
            domeasu<-curmeasu
            nfocus<-length(domeasu)
            
            focusmatrix<-as.data.frame(matrix(NA,nrow=nfocus,ncol=3))
            names(focusmatrix)<-c("par","allfoci","nrep")
            focusmatrix$par<-domeasu
            focusmatrix$allfoci<-unlist(lapply(focusmatrix$par,function(x) 
                (!sum(adempars %in% x))*7 + (sum(adempars %in% x)*3) ))
            focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
                (!sum(adempars %in% x))*3 + (sum(adempars %in% x)*2) ))
            focuscols<-sum(focusmatrix$nrep)
            
        }else{
            domeasu<-curtaskdetails[seq(4,length(curtaskdetails),2)]
            nfocus<-length(domeasu)
            
            focusmatrix<-as.data.frame(matrix(NA,nrow=nfocus,ncol=3))
            allfoci<-as.numeric(curtaskdetails[seq(5,length(curtaskdetails),2)])
            names(focusmatrix)<-c("par","allfoci","nrep")
            focusmatrix$par<-domeasu
            focusmatrix$allfoci<-allfoci
            focusmatrix$nrep<-unlist(lapply(allfoci,function(x) sum((as.integer(intToBits(x))[1:3]))))
            focuscols<-sum(focusmatrix$nrep)
            
        }
        
        #print("Generate focus-data frame")
        focus<-as.data.frame(matrix(NA,nrow=focuscols,ncol=4))
        names(focus)<-c("ad","parameter","datatype","focus")
        
        focus[,1]<-"AD"
        focus[,2]<-unlist(lapply(domeasu, function(x) 
            rep(x,focusmatrix$nrep[focusmatrix$par==x])))
        focus[,3]<-unlist(lapply(domeasu,function(x)
            rep(if(x %in% adempars){"adem"}else{"ief"},focusmatrix$nrep[focusmatrix$par==x])))
        
        if(curtaskdetails[4]=="all"){
            # Count occurences of parameters and give them IDs
            focusid <- tapply(1:nrow(focus), focus$parameter, function(x) cbind(x,1:length(x)))
            focusid <- Reduce(rbind, focusid)
            focus$focus <- 1:nrow(focusid)
            focus$focus[focusid[,1]] <- 2**(focusid[,2]-1)
        }else{
            # Convert allfoci to binary and see at which position there is a '1' and convert position to 2**
            focus[,4]<-unlist(lapply(allfoci,function(x) 2**((which((as.integer(intToBits(x))[1:3]) %in% 1)-1))))
        }
        
#Select focus        
        for (numrun in 1:(focuscols)){
#        for (numrun in 5:5){
            #numrun<-1            
            #print(paste0("#Copy plot-requests to current plot for numrun=",numrun))
            curad   <-focus[numrun,1]
            curpar  <-focus[numrun,2]
            curdata <-focus[numrun,3]
            curfocus<-focus[numrun,4]
            
            if(curfocus==1){curfocus<-"value"}
            if(curfocus==2){curfocus<-"trend"}
            if(curfocus==4){curfocus<-"countries"}
            
            print(paste("Current task ",numrun,". AD=",curad,". Par=",curpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))
            
            
            #             if(numrun==1){curdata="adem";curfocus="value"}
            #             if(numrun==2){curdata="adem";curfocus="trend"}
            #             if(numrun==3){curdata="ief";curfocus="value"}
            #             if(numrun==4){curdata="ief";curfocus="trend"}
            #             if(numrun==5){curdata="ief";curfocus="countries"}
            
            
            # Copy data of relevance to the eealocator data frame
            # Get unit for parameter
            if((curpar=="AD") & (curpar %in% curmeasu)){
                eealocator<-actcount
                curunit<-unitad
            } else if((curpar=="EM") & (curpar %in% curmeasu)){
                eealocator<-emicount
                curunit<-unitem
            } else if((curpar=="IEF") & (curpar %in% curmeasu)){
                eealocator<-iefcount
                curunit<-unitief
            } else {
                
                tmp1<-subset(categorymatrix,meastype==curpar,select=-meastype)
                curunit<-as.vector(unique(subset(tmp1,select=unit))[,1])
                parparties<-t(as.vector(subset(tmp1,select=party)))
                eealocator<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods))
                row.names(eealocator)<-parparties                
            }
            
            if(sum(eealocator)!=0){
                #rm(list=c("tmp0","tmp1","tmp2","tmp3"))
                
                #print("# FOR TEST PURPOSE: ADD RANDOM NUMBER TO INCREASE NUMBER OF COUNTRIES")
                addrandomcountries<-0
                if(addrandomcountries==1){
                    attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
                    allcountries<-row.names(attributes)
                    allcountries<-allcountries[allcountries != "Other"]
                    restcountries<-allcountries[! allcountries %in% party]
                    #Add countries
                    nnewc<-25
                    newcn<-restcountries[sample(1:length(restcountries),nnewc,replace=FALSE)]
                    #Multiplicate with random between 0.7 and 1.3 times values
                    randommulp<-matrix(runif(nyears*nnewc,min=0.9,max=1.2),ncol=nyears)
                    
                    pick<-matrix(sample(0:1,nyears*length(party),replace=T),ncol=nyears)
                    pick[2,]<-1-pick[1,]
                    pick<-as.vector(t(colSums(eealocator*pick)))
                    newcv<-randommulp*pick
                    rownames(newcv)<-newcn
                    colnames(newcv)<-years
                    eealocator2<-rbind(eealocator,newcv)
                    eealocator<-eealocator2
                }
                
                #print("Think about text-elements later")
                #print(curunit)
                dimensions<-read.csv("meta_data_dimensions.txt", sep=",")
                if(curunit=="kg/head/yr"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"}
                if(curunit=="kg/head/year"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"}
                if(curunit=="MJ/head/day"){textunit<-"[ MJ " ~~ head^{-1}  ~~ day^{-1} ~"]"}
                if(curunit=="Gg"){textunit<-"[Gg" ~"]"}
                if(curunit=="kg"){textunit<-"[kg" ~"]"}
                if(curunit=="kt"){textunit<-"[kt" ~"]"}
                if(curunit=="1000s"){textunit<-"[1000s" ~"]"}
                if(curunit=="%"){textunit<-"[%" ~"]"}
                
                textsou<-cursourc #dimensions[dimensions[2]==cursourc][1]
                #   ---> If the category name includes the "source" (eg animal type) - remove
                textcat<-gsub(textsou,"",curcateg) #dimensions[dimensions[2]==curcateg][1]
                textpar<-dimensions[dimensions[2]==curpar][1]
                textpar<-measname[measname[,"meastype"]==curpar & measname[,"sector_number"]==curcateg,"measure"]
                #textsou<-gsub("Dairy CATT","Dairy Cattle",textsou)
                
                #print("#Groth rates calculated as Y_i/Y_(i-1) - 1")
                growth<-eealocator[,2:nyears]/eealocator[,1:nyears-1]-1
                
                # see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
                is.nan.data.frame <- function(x)
                    do.call(cbind, lapply(x, is.nan))
                growth[is.nan(growth)] <- 0
                
                abstrend<-eealocator[,2:nyears]-eealocator[,1:nyears-1]
                
                if (curdata=="adem"){eeatrend<-abstrend}
                if (curdata=="ief") {eeatrend<-growth}
                if (curfocus=="trend" && curpar=="EM"){
                    # From run_emi_EU15_data.sh
                    # c:\adrian\data\inventories\ghg\unfccc\inventorysystem\eea_locator_tool\plots\scripts\run_emi_EU15_data.sh
                    #  pct_act = sprintf("%.0f",100 * ( ACT[country,endy] * ( vstarty / ACT[country,starty] ) - vstarty ) / ( vendy - vstarty ) )
                    pct_act1<-100*(actcount[,nyears]*(sum(eealocator[,1])/actcount[,1])-sum(eealocator[,1]))/(sum(eealocator[,nyears])-sum(eealocator[,1]))
                    
                    # Alternative:
                    # DeltaE = En - Ea = ADn * IEFn - ADa * IEFa
                    # DeltaE* = (ADn - ADa) * IEFa
                    # DeltaE& = (IEFn - IEFa) * ADn
                    # DeltaE* + DeltaE& = DeltaE
                    # Share AD: DeltaE*/DeltaE
                    
                    deltaEM_EU28<-sum(eealocator[,nyears])-sum(eealocator[,1])
                    deltaEM<-eealocator[,nyears]-eealocator[,1]
                    deltaAD<-actcount[,nyears]-actcount[,1]
                    deltaIF<-iefcount[,nyears]-iefcount[,1]
                    pct_act2<-100*((actcount[,nyears]-actcount[,1])
                                   *iefcount[,1])/
                        (sum(eealocator[,nyears])-sum(eealocator[,1]))/1000
                    
                    # Share of AD and IEF on EU absolute trend
                    act_pcteu<-100*(deltaAD*iefcount[,1]/1000)/deltaEM_EU28
                    ief_pcteu<-100*(deltaIF*actcount[,nyears]/1000)/deltaEM_EU28
                    # Share of AD and IEF on country absolute trend
                    act_pctcountry<-100*(deltaAD*iefcount[,1]/1000)/deltaEM
                    ief_pctcountry<-100*(deltaIF*actcount[,nyears]/1000)/deltaEM
                    act_pctcouneu<-100*((acteu28[nyears]-acteu28[1])*iefeu28[1]/1000)/(emieu28[nyears]-emieu28[1])
                    ief_pctcouneu<-100*((iefeu28[nyears]-iefeu28[1])*acteu28[nyears]/1000)/(emieu28[nyears]-emieu28[1])
                    
                }
                
                
                if(dotrendoutl==1){
                    trendoutlmethod<-2
                    
                    # Save quantiles for growth rate
                    growthquantiles<-t(apply(growth, 1, quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE))
                    eeaquantiles<-t(apply(eealocator, 1, quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE))
                    
                    # Median absolute deviation of the absolute deviations from the median
                    growthmad<-apply(growth, 1, mad,  na.rm = TRUE)
                    eealocatormad<-apply(eealocator, 1, mad,  na.rm = TRUE)
                    
                    #print("# Mulitply with factor according to outl Tool")
                    #growthmadn<-growthmad/0.6745
                    
                    if(trendoutlmethod==1){
                        # Method 1 in outl tool: Deviation from median
                        growthmeddevtest<-abs(growth-growthquantiles[2,])>2*growthmadn
                        meddevtest<-abs(eealocator-eeaquantiles[2,])>2*eealocatormadn
                        
                    } else if (trendoutlmethod==2){
                        # Method 2 in outl tool
                        bxplf <- 0.953
                        #uwhisk = mdian + (1 + bxplf) * (uquart - mdian)
                        #lwhisk = mdian + (1 + bxplf) * (lquart - mdian)
                        growthuwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,3]-growthquantiles[,2])
                        growthlwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,1]-growthquantiles[,2])
                        growthmeddevtest<-(growth>growthuwhisk) | (growth<growthlwhisk)
                        
                        uwhisk<-eeaquantiles[,2]+(1 + bxplf)*(eeaquantiles[,3]-eeaquantiles[,2])
                        lwhisk<-eeaquantiles[,2]+(1 + bxplf)*(eeaquantiles[,1]-eeaquantiles[,2])
                        meddevtest<-(eealocator>uwhisk) | (eealocator<lwhisk)
                    }
                    growthoutl<-growthmeddevtest*growth
                    eealocatoroutl<-meddevtest*eealocator
                    ngrowthoutl<-sum(growthoutl!=0)
                    neealocatoroutl<-sum(eealocatoroutl!=0)
                    #print("#unlist(growthoutl)")
                    
                    if(ngrowthoutl>0){
                        listofoutls<-which(unlist(growthoutl)!=0)
                        growthoutl$country<-row.names(growthoutl)
                        growthoutllist<-melt(growthoutl,na.rm=T)[listofoutls,]
                        cntyid<-nrow(growth)-(listofoutls %% nrow(growth))
                        #get year-ID
                        yearid<-(listofoutls-(cntyid))/2+1
                        growthoutllist<-cbind(cursubm,curcateg,cursourc,curpar,"growth",
                                              growthoutllist,growthquantiles[cntyid,])
                    }
                    #paste0(curcateg,cursourc,curpar)
                }
                
                
                
                
                topn<-10
                
                #print("#Define trend data frame")
                if (curfocus=="trend"){curmatrix<-eeatrend}
                if (curfocus=="value"){curmatrix<-eealocator}
                if (curfocus=="countries"){curmatrix<-eealocator}
                
                if(curdata=="adem"){
                    #print("Calculate EU data")
                    # Value-plot
                    eu28<-colSums(curmatrix,na.rm=T)
                    rel<-t(t(curmatrix)/eu28)
                    relav<-rowMeans(rel)
                    
                    # Trend-plot 
                    eu28.trend<-colSums(eeatrend)
                    rel.trend<-t(t(eeatrend)/eu28.trend)
                    relav.trend<-rowMeans(rel.trend)
                    
                    topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                    topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
                    
                }
                
                if(curdata=="ief"){
                    eu28mean<-colMeans(curmatrix,na.rm=T)
                    if((curfocus=="value") & ("AD" %in% curmeasu)){
                        #Some parameter are not reported from all countries
                        eu28<-colSums(curmatrix*actcount[rownames(actcount) %in% rownames(curmatrix),])/
                              colSums(actcount[rownames(actcount) %in% rownames(curmatrix),])
                    }else{
                        eu28<-eu28mean
                    }
                    
                    
                    #Calculate relative value
                    if(curfocus=="value"){rel<-t(t(curmatrix)/eu28)}
                    if(curfocus=="countries"){rel<-t(t(curmatrix)/eu28)}
                    if(curfocus=="trend"){rel<-eeatrend}
                    
                    #Keep only those values to plot which are not overlaying the boxplot
                    #Attention: the endrange might make 'disappear' points that would be expected...
                    endrange<-0.1
                    
                    lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
                    upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
                    lowerok<-(curmatrix<lowerend)
                    upperok<-(curmatrix>upperend)
                    relna<-lowerok+upperok
                    
                    #Use absolute relative deviation as sorting criterium
                    #Store the maximum absolute relative deviation
                    relabs<-abs(1-rel)
                    relav<-rowMeans(relabs)
                    relav<-apply(relabs,1,max)
                    
                    #Trend from time series (do not use the mean trend)
                    eu28.trendmean<-colMeans(eeatrend,na.rm=T)
                    eu28.trend<-eu28[2:nyears]/eu28[1:(nyears-1)]
                    
                    
                    rel.trend<-t(t(eeatrend)/eu28.trend)
                    relav.trend<-rowMeans(rel.trend,na.rm=T)
                    
                    #Keep code below in case such a criterion will be applied later
                    #if(curfocus=="value"){reldiff=0.1}
                    #if(curfocus=="trend"){reldiff=0.03}
                    #if(curfocus=="countries"){reldiff=0.03}
                    #rel[is.na(rel)]<- 999
                    #relna<-rel
                    #relna[relna==-999]<- 999
                    ##keep only those which down-deviation larger that relidiff 
                    #relna[relna<1-reldiff]<-1
                    #relna[relna==999]<--999
                    ##keep only those which up-deviation larger that relidiff 
                    #relna[relna>1+reldiff]<-1
                    #relna[relna!=1]<-0
                    ##Now delete all the value with small deviations
                    relplot=curmatrix*relna
                    
                    if(curfocus=="value"){
                        relplot[relplot==0]<-NA
                        #Criterion for selecting country: largest average deviation from EU average
                        # --nr.rm=F keeps only those countries which have large deviations for the whole time series
                        #relav<-rowMeans(relplot,na.rm=F)
                    }
                    
                    if(curfocus=="trend"){
                        #Criterion for selecting country: largest internnual change in timeseries
                        #relav<-apply(relplot,1,max)
                        relplot[relplot==0]<-NA
                    }  
                    
                    #---> first determine number of non-NA elements
                    topn<-min(10,length(sort(relav,decreasing=F)))
                    topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                    topneu28<-sort(topneu28,decreasing=F)
                    topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
                }
                
                #print("Determine the the top n countries contributing on average most to EU28 values")
                topnnames<-names(topneu28)
                toponames<-names(topother)
                ncountries<-min(topn,length(topnnames))+min(1,length(toponames))
                
                
                #print("Extract top n countries from dataset and group other together for plotting")
                eu28main<-curmatrix[topnnames,]
                eu28rest<-curmatrix[toponames,]
                if(curdata=="ief" && curfocus=="trend"){
                    eu28main<-relplot[topnnames,]
                    eu28rest<-relplot[toponames,]
                }
                if(curdata=="adem"){
                    Other<-colSums(eu28rest,na.rm=T)
                }else{
                    Other<-colMeans(eu28rest,na.rm=T)
                }
                
                #print("# ---> combine Main countries with the 'other' countries")
                if(sum(Other,na.rm=TRUE)>0){
                    eu28fin<-rbind(eu28main,Other)
                }else{
                    eu28fin<-eu28main
                }
                if(length(toponames)>0){
                    finnames<-c(row.names(eu28fin)[1:(ncountries-1)],"Other")
                }else{
                    finnames<-row.names(eu28fin)
                }
                eu28fin<-as.matrix(eu28fin)
                
                #rownames(eu28fin)<-finnames
                finshares<-round(rowMeans(eu28fin,na.rm=T)/mean(eu28)*100,1)
                
                if(curfocus=="trend"){
                    # Calculate the finshares from trend over total time period
                    
                }
                source("nirplots.r")
                #nirplotsdone<-nirplots(curmatrix,eu28fin,eu28,curdata,curfocus,curcateg,curpar,curfoc)
            }
        }
    }
    if(substr(curtasks[icurtasks],0,3)=="cat"){starttasks=1}
}

