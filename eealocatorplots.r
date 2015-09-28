# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
# File eealocatorplots.r
# Main file required for the EU-GIRP
# Purpose: A: connect to eealocator file (run eealocator_csv.bash first)
#             and generate focus
#          B: loop over focus and prepare for current plot 
#             (selection of parameter, trend/value/country-plot, etc.)
#          C. run eugirp_nirplots.r for current plot
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
# Version 1.5 - 07.09.2015
# 
# Initialisation ####


# current inventory year
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
# - First separate agri from other data to speed up processing
# - Clean animal types in source file "eugirp_Acleananimaltypes.r"
# - Recombine with the other sectors
if(stepsdone==1){
    print("Step 2: Generate list of measures")
    source("eugirpA.2_meastype.r")
    stepsdone<-2
    savelist<-c("stepsdone","savelist","alldata","allnotations","allinfos","allmethods")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s2~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>1){
    print("Step 2: List of measures ... already done")
}

# B.1 - Plots 1. Calculate EU-sums and simplify units (remove very large numbers) ####
if(stepsdone==2){
    print("Step 3a: Calculating EU-sums only for summable variables")
    #load(rdatmeasu)
    #source("eugirpD.1_preparetask.r")
    
    calceu<-alldata

    calcmeas<-unique(subset(calceu,select=allfields[!allfields %in% c("notation","party",years)]))
    measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
    lc<-measures2sum[grepl("^3",measures2sum$sector_number),]
    
    eu28sum<-as.data.frame(matrix(rep(0,ncol(calceu)*nrow(measures2sum)),ncol=ncol(calceu),nrow=nrow(measures2sum)))
    names(eu28sum)<-names(calceu)
    eu28sum[,names(measures2sum)]<-measures2sum[,names(measures2sum)]
    eu28sum[,years]<-euvalue("sum",eu28sum,calceu,years,countriesic)
    eu28sum[,"party"]<-rep("EU28",nrow(eu28sum))
    eu28sum$notation[eu28sum$notation==0]<-""
    
    # Check on outliers in AD and EMs: no country should really dominate unless it is the only country reporting
    sharecalc<-function(uid,D,E,x){
        uid<-as.vector(unlist(uid))
        alc<-c(1:length(countriesnoeu))
        coval<-extractuiddata(DF = D,uid = uid,c = countriesnoeu,narm = FALSE)
        ncoval<-coval[apply(coval,1,sum,na.rm=TRUE)!=0,]
        if(is.matrix(ncoval)){ncoval<-nrow(ncoval)}else{ncoval<-0}
        
        euval<-as.numeric(matrix(E[E$variableUID==uid,years]))
        if(ncoval>3) {
            share<-t(apply(coval, 1, "/", euval))
            v<-which(share>0.95,arr.ind=TRUE)
            if(nrow(v)>0){
                nc<-rep(ncoval,nrow(v))
                uids<-rep(uid,nrow(v))
                pa<-countriesnoeu[v[,1]]
                averagepa<-unlist(lapply(c(1:nrow(v)),function(x) mean(coval[v[x,1]],na.rm=TRUE)))
                averagepa<-round(averagepa,3)
                averageot<-mean(coval[alc[!alc%in%pa]],na.rm=TRUE)
                averageot<-rep(round(averageot,3),nrow(v))
                yr<-years[v[,2]]
                v1<-v[,1]
                shares<-as.vector(t(Reduce(rbind,lapply(c(1:nrow(v)),function(x) share[v[x,1],v[x,2]]))))
                shares<-round(shares,3)
                return(list(nc,pa,yr,shares,uids,averagepa,averageot))
            }else{return(NULL)}
        }else{return(NULL)}
    }
    agrisummeas<-measures2sum[grepl("^3",measures2sum$sector_number),]
    calcshare<-as.data.frame(t(Reduce(cbind,lapply(c(1:nrow(agrisummeas)),function(x) 
        Reduce(rbind,sharecalc(uid=agrisummeas$variableUID[x],D=calceu,E=eu28sum,x))))))
    names(calcshare)<-c("ncountries","party","year","share","variableUID","mean","meanother")
    ademoutl<-merge(calcshare,agrisummeas,by="variableUID")
    ademoutl<-simplifytestmatrix(check = ademoutl,group = c("year","share"),compare = list(years,"range"))
    ok1<-ademoutl$party=="IT"&ademoutl$category=="Buffalo"
    ok2<-ademoutl$party=="DK"&ademoutl$source=="Digesters"
    ok3<-ademoutl$party=="NL"&ademoutl$option=="Option B"
    ok4<-ademoutl$party=="RO"&grepl("^3.F.",ademoutl$sector_number)
    ok5<-ademoutl$party=="SE"&grepl("^3.*8$",ademoutl$sector_number)
    ademoutl<-ademoutl[!(ok1 | ok2 | ok3 | ok4 | ok5),]
    ademoutl$correction<-0
    ademoutl<-ademoutl[order(ademoutl$party,ademoutl$sector_number),]
    
    #source("eugirp_simplifyunit.r")
    calceu<-rbind(alldata,eu28sum)
    
    # NEXC must be all changed, even if for one MMS the values are small
    selection<- (grepl("^3",calceu$sector_number) & calceu$unit=="kg N/year")
    calceu$unit[selection]<-"kt N/year"
    calceu[selection,years]<-calceu[selection,years]/1000000
    
    #     selection<- (calceu$unit=="t N/year")
    #     calceu$unit[selection]<-"kt N/year"
    #     calceu[selection,years]<-calceu[selection,years]/1000
    
    #Remove end-blank in sector_number
    selection<-grepl(" $",calceu$sector_number)
    calceu$sector_number[selection]<-gsub(" $","",calceu$sector_number[selection])
    
    #save alldata before unit conversion as backup
    save(alldata,eu28sum,file=gsub("_clean","_nounitconv",rdatallem))
    o<-order(calceu$sector_number,calceu$category,calceu$meastype,calceu$classification,calceu$party)
    alldata<-calceu[o,allfields]
    
    stepsdone<-3
    emplotsdone<-0
    savelist<-c(savelist,"emplotsdone","eu28sum","ademoutl")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>2){
    print("Step 3a: EU sums already calculated")
}

# B.2 - Plots 1. Do emission plots ####
#emplotsdone<-1
if(stepsdone>2){
    if(doemissionplots==TRUE){
        if(emplotsdone==0){
            print("Step 4b: Emission plots")
            adempars<-c("AD","EM")
            source("eugirpD.2_emissionplots.r")
            emplotsdone<-1
            save(list=savelist,file=rdatallem)
            stop("End of general part (Emission plots done!)")
        }else{
            print("Step 3b: Emission plots already done")
        }
    }else{
        if(emplotsdone==0) print("Emission plots not yet done but not requested")
        if(emplotsdone==1) print("Emission plots already done")
    }
}

#++++ END OF GENERAL PART 
#++++ BELOW SECTOR-3 SPECIFIC PART
# A.3 Calculate trend and growth rates ####
if(stepsdone==3){
    print("Step 4: Calculating trends and growth rates")

    nyears<-length(years)
    period1<-as.character(years[1]:years[nyears-1])
    period2<-as.character(years[2]:years[nyears])
    
    #Select only agri-data and remove EU28
    agriselect<-grepl("^3",alldata$sector_number) 
    agriselect<-agriselect | alldata$sector_number=="" & alldata$classification%in%mslivestockclass
    agriselect<-agriselect & alldata$party != "EU28"
    allagri<-alldata[agriselect,]
    o<-order(allagri$sector_number,allagri$category)
    allagri<-allagri[o,]

    # Add other livestock sector_numbers
    source("eugirpB.2_otherlivestock.r")

    print("# Calculate EU28 sum")
    agrimeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
    agri2sum<-agrimeas[agrimeas$meastype %in% meas2sum,]
    eu28sum<-as.data.frame(matrix(rep(0,ncol(allagri)*nrow(agri2sum)),
                                  ncol=ncol(allagri),nrow=nrow(agri2sum)))
    names(eu28sum)<-names(allagri)
    eu28sum[,names(agri2sum)]<-agri2sum[,names(agri2sum)]
    eu28sum[,years]<-euvalue("sum",eu28sum,allagri,years,countriesic)
    eu28sum[,"party"]<-rep("EU28",nrow(eu28sum))
    eu28sum$notation[eu28sum$notation==0]<-""
    allagri<-rbind(allagri,eu28sum)
    
    alltrend<-as.data.frame(matrix(0,rep(0,ncol(allagri)),ncol=ncol(allagri)))
    alltrend<-allagri[allagri$meastype %in% meas2sum,]
    alltrend[,period2]<-allagri[allagri$meastype %in% meas2sum,period2]-allagri[allagri$meastype %in% meas2sum,period1]
    alltrend[,years[1]]<-NA
    
    mgrowth<-c(meas2popweight,meas2clima,meas2mcf)
    allgrowth<-as.data.frame(matrix(0,rep(0,ncol(allagri)),ncol=ncol(allagri)))
    #allgrowth<-allagri[allagri$meastype %in% mgrowth,]
    #allgrowth[,period2]<-allagri[allagri$meastype %in% mgrowth,period2]/allagri[allagri$meastype %in% mgrowth,period1]
    allgrowth<-allagri
    allgrowth[,period2]<-allagri[,period2]/allagri[,period1]
    allgrowth[is.nan(allgrowth)] <- 0
    allgrowth[is.infinite(allgrowth)] <- 1
    allgrowth[,years]<-round(allgrowth[,years],3)
    allgrowth[,years[1]]<-NA

    stepsdone<-4
    savelist<-c(savelist,"allagri","alltrend","allgrowth")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>3){
    print("Step 4: Trends and growth rates already calculated")
}

# A.3 Check for outlier errors ####
correctionsdone<-1
if(!exists("growthcheck")) if(exists("correctionsdone")) rm(correctionsdone)
if(stepsdone==4){
    
    #Remove EU28 data as they have to be re-calculated
    
    
    #Note: set correctionsdone to 1 if the file has been checked and update file name below
    #      if the issues are written into file, correctionsdone is set to 2 and the 
    if(exists("correctionsdone")){
        if(correctionsdone==1){
            print("Step 5: Write out country issues and list corrections needed for IEF calculation ")
            corrections<-read.csv(file=gsub(".csv","_checked.csv",filoutliers),comment.char = "#",header=TRUE)
            corrections<-corrections[!is.na(corrections$cursubm),]
            donotusepar<-subset(corrections,corrections==0,select=c("party","variableUID"))
            
            source("eugirp_writeissues.r")
            firstissue<-7
            lastissue<-nrow(corrections[corrections$correction!=1,])
            lastissue<-7
            writenames<-names(corrections[!names(corrections)%in%c(docfields,resolved)])
            issues<-sum(unlist(lapply(c(firstissue:lastissue),function(x) writeissue(allagri,corrections[x,writenames]))))
            stepsdone<-5
            correctionsdone<-2
            savelist<-c(savelist,"corrections","correctionsdone")
            save(list=savelist,file=rdatallem)
            save(list=savelist,file=gsub(".RData",paste0("_s5~",figdate,".RData"),rdatallem))
            source("curplot.r")
            print("Comment 'correctionsdone<-1'!")
        }else if(correctionsdone==2){
            print("Required corrections identified and outlier issues written to files")
        }
    }else{
        print("Check for outlier errors")
        source("eugirp_obligatoryemissions.r")
        source("eugirpA.3_outlier.r")

        
        # Key source categories ####
        keycats<-read.table("keycategories.txt")
        keycats<-as.vector(keycats$V1)
        
        select<-alldata$variableUID%in%keycats
        keycategories<-alldata[select,]

        select<-allmethods$variableUID%in%keycats
        keymethods<-allmethods[select,]
        agrimethods<-allmethods[grepl("^3",allmethods$sector_number),]
        agrimethods<-agrimethods[agrimethods$notation!="NA",]
        
        select<-keycategories$gas=="CH4"
        keycategories[select,years]<-keycategories[select,years]*gwps[1]
        select<-keycategories$gas=="N2O"
        keycategories[select,years]<-keycategories[select,years]*gwps[3]
        
        select<-alldata$gas=="Aggregate GHGs" & grepl("^[1-6].$",alldata$sector_number)
        select<-select | alldata$gas=="Aggregate GHGs" & grepl("Total",alldata$sector_number)
        totem<-alldata[select,]
        
        View(totem)
        
        if(nrow(ademoutl)>0){write.csv(ademoutl,file=paste0(invloc,"/checks/checks",cursubm,"ADEMoutliers.csv"))}
        savelist<-c(savelist,"growthcheck","paramcheck","autocorrections")
        save(list=savelist,file=rdatallem)
        save(list=savelist,file=gsub(".RData",paste0("_s5~",figdate,".RData"),rdatallem))
        stop("Outliers need to be checked and 'correctionsdone' set MANUALLY to 1")
    }
}else if(stepsdone>4){
    print("Step 5: Check for outlier errors already done")
}

# B.1 Calculate EU sums and weighted averages ####
# 
#stepsdone<-5
if(stepsdone==5){
    print("Step 6: Calculate EU weighted averages")
    
    source("eugirpB.1_euvalues.r")
    
    stepsdone<-6
    save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    savelist<-c(savelist,"allagri","assignad2par")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s6~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>5){
    print("Step 6: EU weighted averages already calculated")
}


# C - Make checks for sector 3 ####
if(stepsdone==6) {
    print("Step 7: Make specific checks for Sector 3")
    #load(rdatmeasu)

    
    source("agrichecks1ADs.r")
    source("agrichecks2Nex.r")
    
    agrichecks<-rbind(checks,check1,check2,check3,check4,check5)
    #stepsdone<-7
#     savelist<-c(savelist,"checkuids","agrichecks","climchecks")
#     save(stepsdone,allagri,allagritab,assignad2par,checkuids,agrichecks,climchecks,file=rdatagri)
#     save(list=savelist,file=gsub(".RData",paste0("_s7~",figdate,".RData"),rdatallem))
#     save(list=savelist,file=rdatallem)
#     source("curplot.r")
}else if(stepsdone>6){
    print("Step 7: Sector 3 checks already done")
}

# C - Make checks for sector 3 ####
if(stepsdone==7) {
    print("Step 7: Make additional plots for sector 3")

    #Attentions: for plots the identified issues need to be corrected again
    #(the data base has not been changed, it was just corrected for EU-weighted parameters)
    # Trend plots for AD-EM
    
    # IEF plots
    source("eugirpD.2_iefplots.r")
    
    # Trend plots for IEF
    
    # Country-plots for IEF
 
    #stepsdone<-8
    source("curplot.r")
}else if(stepsdone>7){
    print("Step 7: All Sector-3 plots already done")
}

