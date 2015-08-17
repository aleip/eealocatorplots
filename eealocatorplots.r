# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
# File eealocatorplots.r
# Main file required for the EU-GIRP
# Purpose: A: connect to eealocator file (run eealocator_csv.bash first)
#             and generate focus
#          B: loop over focus and prepare for current plot 
#             (selection of parameter, trend/value/country-plot, etc.)
#          C. run nirplots.r for current plot
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
# version 3 - 28.07.2015
# 
# Initialisation ####

library(ggplot2)
library(reshape2) #required for melt function - for outls
library(data.table)
library(knitr)
library(compare)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)

#       current inventory year
locplots<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
setwd(locplots)
source("curplot.r")


# PART A: Link with EEA locator tool ----
# A.1 Load eea-locator data (either from text file or from pre-processed Rdata file) ####
# Return:
# - alldata: data frame containing (almost) all EEA-locator data. Years are transposed to columns.
#            Some cleaning on uids has been done, data rows with irrelevant gases, UK removed (GB
#            kept), sector_numbers "-" removed. 
#            Infos (e.g. documentation boxes) and notation keys stored in separate files
# - allnotations: Notations used
# - allinfos: Information given, e.g. in documentation boxes
# - alldatanovalues
# - measname
source("eugirpA.1_eealocator.r")

# A.2 Clean animal type names ####
# Some animal types are given under 'allmethods', the options either 'sector_number' and/or 'allmethods'
# ---> put them all to 'sector-number': sector option animal
# Approach:
# - First separate cat3 from other data to speed up processing
# - Clean animal types in source file "eugirp_Acleananimaltypes.r"
# - Recombine with the other sectors
if(stepsdone==1){
    print("Clean animal types and generate list of measures")
    source("eugirpA.2_meastype.r")
    stepsdone<-2
    savelist<-c("stepsdone","savelist","alldata","allnotations","allinfos","allnotations")
    save(list=savelist,file=rdatallem)
    source("curplot.r")
}else if(stepsdone>1){
    print("Clean animal types and generate list of measures ... already done")
}

# B.2 Calculate trend and growth rates ####
nyears<-length(years)
period1<-as.character(years[1]:years[nyears-1])
period2<-as.character(years[2]:years[nyears])
o<-order(alldata$sector_number,alldata$category)

if(stepsdone==2){
    print("Calculating trends and growth rates")
    alldata<-alldata[o,]
    alltrend<-as.data.frame(matrix(0,rep(0,ncol(alldata)),ncol=ncol(alldata)))
    alltrend<-alldata[alldata$meastype %in% meas2sum,]
    alltrend[,period2]<-alldata[alldata$meastype %in% meas2sum,period2]-alldata[alldata$meastype %in% meas2sum,period1]
    alltrend[,years[1]]<-NA
    
    mgrowth<-c(meas2popweight,meas2clima,meas2mcf)
    allgrowth<-as.data.frame(matrix(0,rep(0,ncol(alldata)),ncol=ncol(alldata)))
    #allgrowth<-alldata[alldata$meastype %in% mgrowth,]
    #allgrowth[,period2]<-alldata[alldata$meastype %in% mgrowth,period2]/alldata[alldata$meastype %in% mgrowth,period1]
    allgrowth<-alldata
    allgrowth[,period2]<-alldata[,period2]/alldata[,period1]
    allgrowth[is.nan(allgrowth)] <- 0
    allgrowth[is.infinite(allgrowth)] <- 1
    allgrowth[,years]<-round(allgrowth[,years],3)
    allgrowth[,years[1]]<-NA
    
    # Add other livestock sector_numbers
    source("eugirpB.2_otherlivestock.r")
    stepsdone<-3
    savelist<-c(savelist,"alltrend","allgrowth")
    save(list=savelist,file=rdatallem)
    source("curplot.r")
}else if(stepsdone>2){
    print("Trends and growth rates already calculated")
}

# A.3 Check for outlier errors ####
if(stepsdone==3){
    print("Check for outlier errors")
    #calcmeas<-allmeas
    source("eugirpA.3_outlier.r")
    
    #     stepsdone<-4
    #     save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    #     savelist<-c(savelist,"growthcheck","paramcheck")
    #     save(list=savelist,file=rdatallem)
    #     source("curplot.r")
}else if(stepsdone>3){
    print("Check for outlier errors already done")
}

stop("Not further developed")

# B.1 Calculate EU sums and weighted averages ####
# 
if(stepsdone==4){
    print("Calculate EU sums and weighted averages")
    #calcmeas<-allmeas
    source("eugirpB.1_euvalues.r")

    stepsdone<-5
    save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    save(stepsdone,alldata,allnotations,allinfos,assignad2par,file=rdatallem)
    source("curplot.r")
}else if(stepsdone>4){
    print("EU sums and weighted averages already calculated")
}

#Update countries
allcountries<-unique(as.vector(alldata$party))
allcountries<-allcountries[order(allcountries)]
countries<-as.data.frame(allcountries)
names(countries)<-"party"



# C - Make checks for sector 3 ####
if(stepsdone==5) {
    load(rdatmeasu)

    source("checkcat3_1ADs.r")
    source("checkcat3_2Nex.r")
    
    cat3checks<-rbind(checks,check1,check2,check3,check4,check5)
    stepsdone<-6
    save(stepsdone,cat3all,cat3alltab,checkuids,cat3checks,file=rdatcat3)
    save(stepsdone,alldata,alltrend,allgrowth,allnotations,allinfos,assignad2par,file=rdatallem)
    source("curplot.r")
}else if(stepsdone>5){
    print("Sector 3 checks already done")
}

# D - Plots 1. Prepare the plots to be done ####
if(stepsdone==6){
    #load(rdatmeasu)
    #source("eugirpD.1_preparetask.r")
    
    adempars<-c("AD","EM")
    if(doemissionplots==TRUE){
        
    }
}


# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
#source("lists.txt")


#listnomeasures<-c("INFO","METHOD",NA)
#curmeasu<-curmeasu[! curmeasu %in% listnomeasures]
#colnames(categorymatrix)[7:(6+nyears)]<-years


print("#Generate matrix 'focus' of tasks from curplot.csv indicating: ")
# - Activity data (are required for AD plots and IEF plots for weighting)
# - Parameter to plot (AD, EM, IEF, or other factor)
# - Data type (determined automatically: summable (AD) or averageable (IEF))
# - Focus: value plot (as trend), 
#          trend plot (as inter-annual changes), 
#          country plot (range as values from trend)
#if(dotaskdetails[4]=="all"){
#domeasu<-as.vector(curcatmeas$meastype)
#docateg<-as.vector(curcatmeas$sector_number)

# focusmatrix ####
#nfocus<-length(domeasu)
focusmatrix<-as.data.frame(unique(subset(categorymatrix,select=c(sector_number,allmethods,gas,meastype,variableUID)))[,1:5])
if(domeasu[1]=="all") domeasu<-as.vector(unique(focusmatrix$meastype))

nfocus<-nrow(focusmatrix)
names(focusmatrix)<-c("cat","met","gas","par","uid")

# AD-uid is required to calculate weighted IEFs 
#   Problem: sometimes the 'category' contains the level with the AD
#            sometimes AD is differentiated by 'allmethods'
#            if no AD-uid is found (mainly aggregated levels) - delete the row
#            if there is multiple AD (e.g. population, N excretion per MMS) - this must be corrected (i.e. N exc per MMS ==> EM)

focusmatrix$met<-as.character(focusmatrix$met)
focusmatrixlength<-subset(focusmatrix,select=c(cat,met,par))
focusmatrixlength$ncat<-unlist(lapply(c(1:nfocus),function(x) 
    length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                      focusmatrix[,"met"]==focusmatrix$met[x] & 
                                      focusmatrix[,"par"]=="AD"]))))
focusmatrixlength$nsec<-unlist(lapply(c(1:nfocus),function(x) 
    length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                      focusmatrix[,"par"]=="AD"]))))

focusmatrix$aduid<-unlist(lapply(c(1:nfocus),function(x) 
    if(focusmatrixlength$ncat[x]==1){
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"met"]==focusmatrix$met[x] & focusmatrix[,"par"]=="AD"])
    }else if(focusmatrixlength$nsec[x]==1){
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"par"]=="AD"])
    } else {focusmatrixlength$ncat[x]}
))
focusmatrix<-focusmatrix[focusmatrix$aduid!=0,]

# Check for multiple ADs ####
# Multiple ADs for a measure detected: generate data frame to investigate the problem
if(searchline)print(436)
multipleads<-focusmatrix[nchar(focusmatrix[,"aduid"])<=30,]
datamultipleaduids<-alldatanovalues[alldatanovalues$variableUID %in% focusmatrix$uid,]
if(nrow(multipleads)>0){
    totads<-sum(as.numeric(multipleads$aduid))
    totmad<-nrow(multipleads)
    multipleaduids<-multipleads$uid
    
    datamultipleadsuid<-as.data.frame(unlist(lapply(c(1:totmad),function(x) rep(multipleads$uid[x],multipleads$aduid[x]))))
    for(tmpnames in c("cat","met","gas","par")){
        print(tmpnames)
        datamultipleadsuid[,tmpnames]<-unlist(lapply(c(1:totmad),function(x) 
            rep(multipleads[x,tmpnames],multipleads$aduid[x])))
    }
    
    datamultipleadsuid$aduid<-unlist(lapply(c(1:totmad),function(x) 
        multipleads$uid[multipleads[,"cat"]==multipleads$cat[x] & multipleads[,"met"]==multipleads$met[x] & multipleads[,"par"]=="AD"]))
    
    datamultipleaduids<-alldatanovalues[alldatanovalues$variableUID %in% datamultipleadsuid$aduid,]
    
    View(datamultipleadsuid)
    View(datamultipleaduids)
    View(multipleads)
    
    stop("Multiple ADs found for the measures",.call=FALSE)
    #Check with: 
    #View(alldatanovalues[grepl("3.B.1.1 Non-Dairy Cattle",alldatanovalues$sector_number),])
    
}
if(nrow(focusmatrix)==0){stop("Nothing to do!")}

if(docategall){
    focusmatrix$allfoci<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*1 + (sum(adempars %in% x)*1) ))
    focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*1 + (sum(adempars %in% x)*1) ))
}else{ 
    focusmatrix$allfoci<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*7 + (sum(adempars %in% x)*3) ))
        #(!sum(adempars %in% x))*(doplots) + (sum(adempars %in% x)*3) ))
    focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*3 + (sum(adempars %in% x)*2) ))
        #(!sum(adempars %in% x))*(doplots%%4) + (sum(adempars %in% x)*2) ))
}    

if(searchline)print(481)
focuscols<-sum(focusmatrix$nrep)
focusmeas<-focusmatrix$par
focuscats<-focusmatrix$cat
focusmets<-focusmatrix$met
focusgass<-focusmatrix$gas

focusrows<-c(1:nrow(focusmatrix))

focusfoc<-focusmatrix$allfoci
print("Generate focus-data frame")
focus<-as.data.frame(matrix(NA,nrow=focuscols,ncol=9))
names(focus)<-c("ad","parameter","datatype","focus","category","method","gas","uid","aduid")
focus[,1]<-"AD"
focus[,2]<-unlist(lapply(focusmeas, function(x) rep(x,focusmatrix$nrep[focusmatrix$par==x][1])))
focus[,5]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$cat[x],focusmatrix$nrep[x])))
focus[,6]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$met[x],focusmatrix$nrep[x])))
focus[,7]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$gas[x],focusmatrix$nrep[x])))
focus[,8]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$uid[x],focusmatrix$nrep[x])))
focus[,9]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$aduid[x],focusmatrix$nrep[x])))
focus[,3]<-unlist(lapply(focusmeas, function(x) rep(if(x %in% adempars){"adem"}else{"ief"},focusmatrix$nrep[focusmatrix$par==x][1])))
focus[,4]<-unlist(lapply(focusfoc,  function(x) 2**((which((as.integer(intToBits(x))[1:3]) %in% 1)-1))))

focus<-focus[focus$focus %in% doplotsv,]
focuscols<-nrow(focus)



# PART B loop over focuscols ####
#Select focus        
#for (numrun in 1:(focuscols)){
for (numrun in 800:902){
    #        for (numrun in 5:5){
    #B1.Copy run-parameter #### 
    # numrun<-1            
    print(paste0("#Copy plot-requests to current plot for numrun=",numrun))
    stophere<-FALSE
    runad   <-as.vector(focus[numrun,1])
    runpar  <-as.vector(focus[numrun,2])
    rundata <-as.vector(focus[numrun,3])
    runfocus<-as.vector(focus[numrun,4])
    runcateg<-as.vector(focus[numrun,5])
    runmethod<-as.vector(focus[numrun,6])
    rungas<-as.vector(focus[numrun,7])
    runuid<-as.vector(focus[numrun,8])
    runaduid<-as.vector(focus[numrun,9])
    runmatrix<-subset(categorymatrix,variableUID==runuid & sector_number==runcateg)
    #selectcountries<-curparts[curparts %in% as.vector(runmatrix$party)]
    runparts<-curparts[curparts %in% runmatrix$party]
    admatrix<-subset(categorymatrix,variableUID==runaduid & party %in% curparts)
    
    
    #if(docat3==TRUE & (runpar=="AD" | runpar=="EM")){stophere<-TRUE}

    runtotal<-sum(runmatrix[,as.character(years)])
    adtotal<-nrow(admatrix)
    if(adtotal>0){adtotal<-sum(admatrix[,as.character(years)])}
    
    if(runfocus==1){runfocus<-"value"}
    if(runfocus==2){runfocus<-"trend"}
    if(runfocus==4){runfocus<-"countries"}
    
    print(paste("Current task ",numrun,". AD=",runad,". Par=",runpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))

    

    # B3. eealocator: Copy data of relevance to the eealocator data frame ####
    # Get unit for parameter
    if(searchline)print(597)
    if((runpar=="AD") & (runpar %in% curmeasu)){
        eealocator<-actcount
        curunit<-unitad
    } else if((runpar=="EM") & (runpar %in% curmeasu)){
        eealocator<-emicount
        curunit<-unitem
    } else if((runpar=="IEF") & (runpar %in% curmeasu)){
        eealocator<-iefcount
        curunit<-unitief
    } else {
        
        tmp1<-subset(runmatrix,meastype==runpar,select=-meastype)
        curunit<-as.vector(unique(subset(tmp1,select=unit))[,1])
        parparties<-t(as.vector(subset(tmp1,select=party)))
        eealocator<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        row.names(eealocator)<-parparties                
    }
    
    # PART C: processing and plotting ####
    if(nrow(eealocator)==0){stophere<-TRUE}else if(sum(eealocator)==0){stophere<-TRUE}

    if(stophere==FALSE){
        #rm(list=c("tmp0","tmp1","tmp2","tmp3"))
        
        #print("# FOR TEST PURPOSE: ADD RANDOM NUMBER TO INCREASE NUMBER OF COUNTRIES")
        # addrandomcountries in case there is none ####
        addrandomcountries<-0
        if(searchline)print(625)
        if(addrandomcountries==1){
            attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
            allcountries<-row.names(attributes)
            allcountries<-allcountries[allcountries != "Other"]
            restcountries<-allcountries[! allcountries %in% curparts]
            #Add countries
            nnewc<-25
            newcn<-restcountries[sample(1:length(restcountries),nnewc,replace=FALSE)]
            #Multiplicate with random between 0.7 and 1.3 times values
            randommulp<-matrix(runif(nyears*nnewc,min=0.9,max=1.2),ncol=nyears)
            
            pick<-matrix(sample(0:1,nyears*length(curparts),replace=T),ncol=nyears)
            pick[2,]<-1-pick[1,]
            pick<-as.vector(t(colSums(eealocator*pick)))
            newcv<-randommulp*pick
            rownames(newcv)<-newcn
            colnames(newcv)<-years
            eealocator2<-rbind(eealocator,newcv)
            eealocator<-eealocator2
        }
        
        #B.2: Growth rates ####
        
        if (rundata=="adem"){eeatrend<-abstrend}
        if (rundata=="ief") {eeatrend<-growth}
        if (runfocus=="trend" && runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)){
        }
        
        if(searchline)print(698)
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
            ngrowthoutl<-sum(growthoutl!=0,na.rm=TRUE)
            neealocatoroutl<-sum(eealocatoroutl!=0)
            #print("#unlist(growthoutl)")
            
            if(ngrowthoutl>0){
                #listofoutls<-which(unlist(subset(growthoutl,select=-country))!=0)
                listofoutls<-which(unlist(growthoutl)!=0)
                growthoutl$country<-row.names(growthoutl)
                growthoutllist<-melt(growthoutl,id.vars="country",na.rm=T)[listofoutls,]
                #al20150727 - why originally 'nrow(growth)-...'?? 
                #cntyid<-nrow(growth)-(listofoutls %% nrow(growth))
                cntyid<-(listofoutls %% nrow(growth))
                cntyid[cntyid==0]<-nrow(growth)
                #get year-ID
                yearid<-ceiling(listofoutls/nrow(growth))
                growthoutllist<-cbind(cursubm,runcateg,runpar,"growth",
                                      growthoutllist,growthquantiles[cntyid,],
                                      row.names=NULL)
            }
            #paste0(runcateg,runpar)
        }
        
        
        
        # B.3-selectcountries ####    
        if(searchline)print(759)
        
        #print("#Define trend data frame")
        if (runfocus=="trend"){curmatrix<-eeatrend}
        if (runfocus=="value"){curmatrix<-eealocator}
        if (runfocus=="countries"){curmatrix<-eealocator}
        
        # B.3a-selectcountries-adem ####    
        if(rundata=="adem"){
            #print("Calculate EU data")
            # Value-plot
            eu28<-colSums(curmatrix,na.rm=T)
            
            temp<-curmatrix
            temp[temp<=0]<-NA
            eu28pos<-colSums(temp,na.rm=T)
            eu28pos[eu28pos==0]<-NA
            
            temp<-curmatrix
            temp[temp>=0]<-NA
            eu28neg<-colSums(temp,na.rm=T)
            eu28neg[eu28neg==0]<-NA
            
            rel<-t(t(curmatrix)/eu28)
            relav<-rowMeans(rel)
            
            # Trend-plot 
            eu28.trend<-colSums(eeatrend)
            rel.trend<-abs(eeatrend)
            relav.trend<-rowSums(rel.trend)/sum(abs(eu28.trend))
            

            if(runfocus=="value"){
                topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                topother<-tail(relav[order(relav,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
                topn<-length(topneu28)
                topno<-max(0,nrow(curmatrix)-topn)
    
                textorderadem1<-"Countries are sorted by the average contribution to the sum of EU28 value over the the whole time period. "
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
            
            if(runfocus=="trend"){
                topneu28<-head(relav.trend[order(relav.trend,decreasing=T,na.last=T)],topn)
                topother<-tail(relav.trend[order(relav.trend,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
                topn<-length(topneu28)
                topno<-max(0,nrow(curmatrix)-topn)
                
                textorderadem1<-paste0("Countries are sorted by the magnitude of their (absolute) inter-annual changes over the year ",min(years),"-",max(years),". ")
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
        }
        
        # B.3a-selectcountries-ief ####    
        if(searchline)print(818)
        if(rundata=="ief"){

            eu28pos<-max(curmatrix,na.rm=T)
            eu28neg<-min(curmatrix,na.rm=T)
                
            eu28mean<-colMeans(curmatrix,na.rm=T)
            #if((runfocus=="value") & ("AD" %in% curmeasu)){
            if((runfocus=="value") & sum(acteu28>0)){
                #Some parameter are not reported from all countries
                eu28<-colSums(curmatrix[rownames(curmatrix) %in% rownames(actcount),]
                             *actcount[rownames(actcount) %in% rownames(curmatrix),])/
                      colSums(actcount[rownames(actcount) %in% rownames(curmatrix),])
            }else{
                eu28<-eu28mean
            }
            
            #Calculate relative value
            if(runfocus=="value"){
                rel<-t(t(curmatrix)/eu28)
                #Use absolute relative deviation as sorting criterium
                #Store the maximum absolute relative deviation
                relabs<-abs(1-rel)
                relav<-rowMeans(relabs,na.rm=T)
                relav<-apply(relabs,1,max,na.rm=T)

            }
            if(runfocus=="countries"){
                #Relative value against the mean value used by MS over the years
                rel<-curmatrix/apply(curmatrix,1,mean,na.rm=T)
                #Largest range of values; in case of negative values rel calculate distance vs max value
                relabs<-((apply(curmatrix,1,max,na.rm=T)-apply(curmatrix,1,min,na.rm=T))
                         /apply(curmatrix,1,max,na.rm=T)
                         )
                relav<-relabs
            
            }
            if(runfocus=="trend"){
                rel<-eeatrend
                #Use absolute relative deviation as sorting criterium
                #Store the maximum absolute relative deviation
                relabs<-(abs(rel))
                relav<-rowMeans(relabs,na.rm=T)
                relav<-apply(relabs,1,max,na.rm=T)
                relav<-relav[!is.na(relav)]
            }
            
            #Keep only those values to plot which are not overlaying the boxplot
            #Attention: the endrange might make 'disappear' points that would be expected...
            endrange<-0.1
            
            lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
            upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
            lowerok<-(curmatrix<lowerend)
            upperok<-(curmatrix>upperend)
            relna<-lowerok+upperok
            
            
            #20150726 - commented keep for a while...
            #Trend from time series (do not use the mean trend)
            #eu28.trendmean<-colMeans(eeatrend,na.rm=T)
            #eu28.trend<-eu28[2:nyears]/eu28[1:(nyears-1)]
            #rel.trend<-t(t(eeatrend)/eu28.trend)
            #relav.trend<-rowMeans(rel.trend,na.rm=T)
            
            relplot=curmatrix*relna
            
            if(runfocus=="value"){
                relplot[relplot==0]<-NA
                #Criterion for selecting country: largest average deviation from EU average
                # --nr.rm=F keeps only those countries which have large deviations for the whole time series
                #relav<-rowMeans(relplot,na.rm=F)
            }
            
            if(runfocus=="trend"){
                #Criterion for selecting country: largest internnual change in timeseries
                #relav<-apply(relplot,1,max)
                relplot[relplot==0]<-NA
            }  
            
            #---> first determine number of non-NA elements
            topn<-min(10,length(sort(relav,decreasing=F)))
            topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
            topneu28<-sort(topneu28,decreasing=F)
            topother<-tail(relav[order(relav,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
            
            topn<-length(topneu28)
            topno<-max(0,nrow(curmatrix)-topn)

            if(runfocus=="value"){
                textiefval1<-"EU28+IC value is obtained from a weighted average of country-values. "
                textiefval2<-"The relative distance from MS/EU28 value is calculated for each year (e.g. 10% smaller). "
                textiefval3<-"Countries are sorted by average relative distance calculated over the whole time period. "
                textiefval4<-paste0("The top ",topn," countries are displayed. ")
                textiefval5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
            }
            if(runfocus=="countries"){
                textiefcnt3<-"Countries are sorted by their relative range of IEFs over the whole time period. "
                textiefcnt4<-paste0("The top ",topn," countries are displayed. ")
                textiefcnt5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textiefcnt3,textiefcnt4,textiefcnt5)
            }
            if(runfocus=="trend"){
                textorderadem1<-"Countries are sorted by the average growth rate over the whole time period. "
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
            
        }
        
        #print("Determine the the top n countries contributing on average most to EU28 values")
        if(searchline)print(931)
        topnnames<-names(topneu28)
        toponames<-names(topother)
        ncountries<-min(topn,length(topnnames))+min(1,length(toponames))
        
        #print("Extract top n countries from dataset and group other together for plotting")
        eu28main<-curmatrix[topnnames,]
        eu28rest<-curmatrix[toponames,]
        if(rundata=="ief" && runfocus=="trend"){
            eu28main<-relplot[topnnames,]
            eu28rest<-relplot[toponames,]
        }
        if(rundata=="adem"){
            Other<-colSums(eu28rest,na.rm=T)
        }else{
            Other<-colMeans(eu28rest,na.rm=T)
            if((runfocus=="value") & sum(acteu28>0)){
                #Some parameter are not reported from all countries
                Other<-colSums(eu28rest*actcount[rownames(eu28rest),])/
                       colSums(actcount[rownames(eu28rest),])
            }
        }
        
        #print("# ---> combine Main countries with the 'other' countries")
        if(length(toponames)>0){
            eu28fin<-rbind(eu28main,Other)
        }else{
            eu28fin<-eu28main
            #20150727 commented because the '1' for 'other' is already excluded (line 915)
            #ncountries<-ncountries-1
        }
        if(length(toponames)>0){
            finnames<-c(row.names(eu28fin)[1:(ncountries-1)],"Other")
        }else{
            finnames<-row.names(eu28fin)
        }
        eu28fin<-as.matrix(eu28fin)
        
        #rownames(eu28fin)<-finnames
        finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28)*100
        finshares[is.na(finshares)]<-0
        
        if(runfocus=="trend"){
            # Calculate the finshares from trend over total time period
            
        }
        if(searchline)print(977)
        if(!(sum(eu28fin,na.rm=TRUE)==0)) {source("nirplots.r")}
        
        #nirplotsdone<-nirplots(curmatrix,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }else{
        print("sum(eealocator)==0")
    }
}

