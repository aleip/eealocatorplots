# EU-GIRP
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
eugirp.fullname<-"EU-Greenhouse gas Inventory Reporting and Plots"
eugirp.version<-"2.0"
eugirp.web<-"https://github.com/aleip/eealocatorplots.git"

# current inventory year
locplots<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
setwd(locplots)
options(warn=0)
source("curplot.r")
options(warn=2)

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
    print("Step 2: Generate list of measures and deal with animals")
    source("eugirpA.2_meastype.r")

    # For the moment: exclude Iceland
    #alldata<-alldata[alldata$party!="IS",]
    
    # Deal with other animals and animal types without sector number
    selection<-alldata$sector_number=="" & alldata$classification%in%mslivestockclass
    selection<-selection | grepl("^3",alldata$sector_number)
    allagri<-alldata[selection,allfields]
    alldata<-alldata[!selection,allfields]

    stop("Plause")
    source("eugirp_otherlivestock.r")
    
    alldata<-rbind(alldata,allagri)
    alltotals<-alldata[grepl("^Sector",alldata$sector_number),]
    stepsdone<-2
    savelist<-c(savelist,"alltotals")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s2~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>1){
    print("Step 2: List of measures & animals ... already done")
}

# B.1 - Plots 1. Calculate EU-sums and simplify units (remove very large numbers) ####
if(stepsdone==2){
    print("Step 3a: Calculating EU-sums only for summable variables")
    
    calceu<-alldata

    calcmeas<-unique(subset(calceu,select=allfields[!allfields %in% c("notation","party",years)]))
    measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
    lc<-measures2sum[grepl("^3",measures2sum$sector_number),]
    
    #eu28sum<-as.data.frame(matrix(rep(0,ncol(calceu)*nrow(measures2sum)),ncol=ncol(calceu),nrow=nrow(measures2sum)))
    #names(eu28sum)<-names(calceu)
    #eu28sum[,names(measures2sum)]<-measures2sum[,names(measures2sum)]
    #eu28sum[,years]<-euvalue("sum",eu28sum,calceu,years,countriesic)
    #eu28sum[,"party"]<-rep("EU28",nrow(eu28sum))
    #eu28sum$notation[eu28sum$notation==0]<-""
    #calceu<-rbind(alldata,eu28sum)
    calceu<-eu28sums(calceu)
    print("Calculate EU-sums")
    eu28sum<-calceu[calceu$meastype %in% meas2sum & calceu$party=="EU28",]
    
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
    print("Calculate calcshare")
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
    if(nrow(ademoutl)>0){write.csv(ademoutl,file=paste0(invloc,"/checks/checks",cursubm,"ADEMoutliers.csv"))}
    
    
    #source("eugirp_simplifyunit.r")
    # NEXC must be all changed, even if for one MMS the values are small
    selection<- (grepl("^3",calceu$sector_number) & calceu$unit=="kg N/year")
    calceu$unit[selection]<-"kt N/year"
    calceu[selection,years]<-calceu[selection,years]/1000000
    
    #     selection<- (calceu$unit=="t N/year")
    #     calceu$unit[selection]<-"kt N/year"
    #     calceu[selection,years]<-calceu[selection,years]/1000
    
    #save alldata before unit conversion as backup
    save(alldata,eu28sum,file=gsub("_clean","_nounitconv",rdatallem))
    o<-order(calceu$sector_number,calceu$category,calceu$meastype,calceu$classification,calceu$party)
    alldata<-calceu[o,allfields]

    #stop("pause")
    source("eugirp_allagri.r")
    
    stepsdone<-3
    emplotsdone<-0
    savelist<-c(savelist,"emplotsdone","eu28sum","ademoutl","allagri","agrimethods","agriemissions","agridet","agrimix","agrigen")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>2){
    print("Step 3a: EU sums already calculated")
}


# B.2 - Plots 1. Do emission plots ####
#emplotsdone<-1
doemissionplots<-TRUE
if(stepsdone>2){
    if(doemissionplots==TRUE){
        if(emplotsdone==0){
            print("Step 4: Emission plots")
            adempars<-c("AD","EM")

            rundata<-"adem"
            source("eugirp_prepareplots.r")
            
            #Data not yet checked - no weighted average
            #rundata<-"ief"
            #plotparamcheck<-0
            #source("eugirp_prepareplots.r")
            
            emplotsdone<-1
            
            savelist<-c(savelist,"emplotsdone","plotmeas")
            save(list=savelist,file=rdatallem)
            save(list=savelist,file=gsub(".RData",paste0("_plots","~",figdate,".RData"),rdatallem))
            stop("End of general part (Emission plots done!)")
        }else{
            print("Step 4: Emission plots already done")
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
    
    #agriemissions$option[agriemissions$option==0]<-""
    agriemissions$option<-""
    agriemissions<-unique(agriemissions)
    
    # Agrishares as agriemissions relative to total emissions ####
    totaluid<-unique(alltotals$variableUID[alltotals$classification==signclass])
    totalval<-alltotals[alltotals$classification=="Total (with LULUCF  with indirect)",]
    totalval<-extractuiddata(DF = alltotals,uid = totaluid,c = allcountries,narm=FALSE)
    agrishares<-unique(agriemissions[,allfields[!allfields%in%c("party",years)]])
    shareuids<-unique(agrishares$variableUID)
    if(nrow(agrishares)!=length(shareuids)){stop("number of uids inconsistent with data frame agrishares")}
    tmp<-Reduce(rbind,lapply(c(1:length(shareuids)),function(x) 
        calculateshares(shareuids[x],agriemissions,totalval)))
    
    selection<-is.na(apply(tmp[,years],1,sum,rm.na=TRUE)) | apply(tmp[,years],1,sum,rm.na=TRUE)==0
    tmp<-tmp[!selection,]
    agrishares<-merge(agrishares,tmp,by=c("variableUID"))
    agrishares<-agrishares[order(agrishares$sector_number,agrishares$category,agrishares$party),allfields]
    
    # Signnificant categories on the basis of the share threshold criterium only ####
    otheryears<-years[!years%in%signyear]

    signcategories<-agrishares[allfields[!allfields%in%otheryears]]
    signcategories[,"maxshare"]<-apply(agrishares[,years],1,max)
    signshares<-c(signyear,"maxshare")
    signcategories[,"potsig"]<-whichmatrix(D = signcategories[,as.character(signyear)],v = which(signcategories[,as.character(signyear)]>signthreshold,arr.ind = TRUE))    
    signcategories$potsig[signcategories$potsig>0]<-1
    shiftfields<-c(metafields,"meastype","unit","measure","notation")
    sigfield<-c(allfields[!allfields%in%c(years,"variableUID",shiftfields)],"potsig",signyear,"maxshare","variableUID",shiftfields)
    signcategories<-signcategories[order(signcategories$sector_number,signcategories$category),sigfield]
    fileunder<-paste0(invloc,"/checks/significant/signcategories~",figdate,".csv")
    
    con<-file(fileunder,open = "wt")
    writeLines(signcatexp,con)
    write.csv(signcategories,con)
    close(con)
    
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
    savelist<-c(savelist,"alltrend","allgrowth","agrishares","signcategories","signthreshold")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>3){
    print("Step 4: Trends and growth rates already calculated")
}

# A.3 Check for outlier errors and calculate EU weighted averages ####
checksteps<-4
if(stepsdone==checksteps){

    allagri<-eu28sums(allagri)
    allagri<-allagri[order(allagri$sector_number,allagri$category,allagri$meastype),]
    #remove option from Cattle, Dairy Cattle, Non-Dairy Cattle
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option[allagri$category%in%allcattle]<-""
    allagri<-unique(allagri)
    curnames<-c("sector_number","category","meastype","option","party","variableUID",years)
    
    print(paste0("Step ",checksteps+1,"a1: Check NEs"))
    source("eugirp_checknes.r")
    
    print(paste0("Step ",checksteps+1,"a2: Check units"))
    source("eugirp_checkunits.r")
    
    print(paste0("Step ",checksteps+1,"a3: Check for outlier errors"))
    source("eugirp_checkoutliers.r")
    tmppar<-paramcheck
    levels(paramcheck$gas)<-factor(signcategories$gas)
    paramcheck[,testfields]<-Reduce(rbind,lapply(c(1:nrow(paramcheck)),function(x) Reduce(cbind,ispotentialissue(paramcheck[x,],signcategories,as.character(signyear),signthreshold))))
        
    paramchecked<-0
    savelist<-c(savelist,"growthcheck","paramcheck","autocorrections","paramchecked")
    
    print(paste0("Step ",checksteps+1,"b: Calculate EU weighted averages"))
    save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    savelist<-c(savelist,"assignad2par")
    
    stop("now write issues")
    
    ## Current manual changes to paramcheck
    paramcheck$correction[paramcheck$party=="BG"&paramcheck$category=="Goats"]<-0
    paramcheck$correction[paramcheck$party=="SE"&paramcheck$meastype=="VSEXC"]<-0
    source("eugirp_euweightedaverages.r")
    
    
    print(paste0("Step ",checksteps+1,"c: Write country outlier list and make corresponding plots"))
    source("eugirp_writeoutlierlist.r")

    rundata<-"ief"
    plotparamcheck<-0
    source("eugirp_prepareplots.r")
    
    # Key source categories ####
    print(paste0("Step ",checksteps+1,"d: Key source category analysis (unfinished)"))
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
    
    print(paste0("Step ",checksteps+1,"e: Save results"))
    stepsdone<-checksteps+1
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    
}else if(stepsdone>checksteps){
    print(paste0("Step ",checksteps+1,": Check for outlier errors already done"))
}

if(exists("paramchecked")){
    
    #paramchecked<-1
    
    #Note: set paramchecked to 1 if the file has been checked and update file name below
    #      if the issues are written into file, paramchecked is set to 2 and the 
    if(paramchecked==1){
        print(paste0("Step ",checksteps+1,": Write out country issues and list corrections needed for IEF calculation "))
        listoftext<-paste0(filoutliers,"_listoftext~",format(Sys.time(),format="%Y%m%d-%H%M"),".csv")
        corfile<-paste0(filoutliers,"list_checked.csv")
        con1 <- file(listoftext, open="wt")
        writeLines(paste0("# ",date()),con1)
        writeLines(paste0("# List if issues that need to be added to the EEA review tool.",
                          "\n# Issue-number and other info to be inserted into the file:",
                          "\n","=HYPERLINK(\"",cursubm,"co_checked.csv","\")"),con1)
        writeLines(paste0("# List of country outlier issues to be included in the tool"),con1)
        writeLines(paste0("id",",","file",",","observation",",","question"),con1)
        close(con1)
        corrections<-read.csv(file=corfile,comment.char = "#",header=TRUE)
        corrections<-corrections[!is.na(corrections$cursubm),]
        corrections[is.na(corrections[,"share"])]<-""
        corrections[is.na(corrections[,"effect"])]<-""
        if(!"issueID"%in%names(corrections)) corrections[,"issueID"]<-""
        corrections[,"share"]<-as.character(corrections[,"share"])
        corrections[,"effect"]<-as.character(corrections[,"effect"])
        corrections[,"issueID"]<-as.character(corrections[,"issueID"])
        xyears<-grepl("X....",names(corrections))
        yyears<-gsub("X","",names(corrections)[xyears])
        names(corrections)[xyears]<-yyears
        donotusepar<-subset(corrections,corrections==0,select=c("party","variableUID"))
        
        #correct unit which was wrong (why??)
        #levels(allagri$gas)<-factor(unique(allagri$gas))
        #levels(corrections$gas)<-gases
        allagri$variableUID<-as.character(allagri$variableUID)
        correctgas<-function(uid){
            gas<-unique(allagri$gas[allagri$variableUID==uid])
        }
        corrections$gas<-unlist(lapply(c(1:nrow(corrections)),function(x) 
            correctgas(corrections$variableUID[x])))
        
        
        #resolved=0: unresolved
        #resolved=4: follow-up unresolved issue
        resolved<-corrections[corrections$resolved!=0 & corrections$resolved!=4,]
        notresolved<-corrections[corrections$resolved==0 | corrections$resolved==4,]
        firstissue<-60
        firstissue<-1
        lastissue<-2
        lastissue<-nrow(notresolved)
        notresolved[c(firstissue:lastissue),"issueID"]<-unlist(lapply(c(firstissue:lastissue),function(x) writeissue(allagri,notresolved[x,],listoftext)))
        
        
        allissues<-rbind(notresolved,resolved)
        
        selection<-grepl(cursubm,allissues$issueID)
        allissues$issueID[selection]<-paste0(allissues$issueID[selection],".csv")
        allissues$issueID[selection]<-unlist(lapply(c(1:sum(selection)), function(x) linkto(allissues$issueID[x])))
        selection<-grepl("png",allissues$plot)
        allissues$plot<-as.character(allissues$plot)
        tmp<-allissues[selection,]
        allissues$plot[selection]<-unlist(lapply(c(1:sum(selection)), function(x) linkto(tmp$plot[x])))
        allissues$plot[!selection]<-""
        
        writeissuelist(allissues,paste0(filoutliers,"_written.csv"))
        
        
        paramchecked<-2
        savelist<-c(savelist,"corrections")
        save(list=savelist,file=rdatallem)
        save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
        source("curplot.r")
        print("Comment 'paramchecked<-1'!")
        
    }else if(paramchecked==2){
        print(paste0("Required corrections identified and outlier issues written to files"))
    }else if(paramchecked==0){
        print("Outliers need to be checked and 'paramchecked' set MANUALLY to 1")
    }
}   


# C - Make checks for sector 3 ####
checksteps<-5
if(stepsdone==checksteps) {
    print(paste0("Step ",checksteps+1,": Make specific checks for Sector 3"))
    #load(rdatmeasu)
    
    # source("tmp_otherlivestockearlier.r")
    # Again manual correction of BE-swine/sheep problem
    #     allagri<-allagri[!allagri$variableUID=="8A72A9BE-DB94-4277-ADFC-8AE85BE6B999",]
    #     uidrem<-"76660D32-4F2A-4C65-81C3-71FA2C340542"
    #     uidnew<-"AB1CC8F6-D71C-46A1-A846-B5E76E2DE3A2"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BE"]<-uidnew
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    #     uidrem<-"1FB8F04A-E6A1-47B7-A6F0-407836FDF3EF"
    #     uidnew<-"3FB4B8CB-B0A8-4BA1-8D81-CD412F301D1B"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BE"]<-uidnew
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option<-""
    allagri<-unique(allagri)
    source("agrichecks1ADs.r")
    source("agrichecks2Nex.r")
    
    agrichecks<-rbind(checks[names(check1)],check1,check2,check3,check4,check5)
    stepsdone<-checksteps+1
    savelist<-c(savelist,"checkuids","agrichecks")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>checksteps){
    print(paste0("Step ",checksteps+1,": Sector 3 checks already done"))
}

checksteps<-6
if(stepsdone==checksteps) {
    print(paste0("Step ",checksteps+1,": Make additional plots for sector 3"))

    #Attentions: for plots the identified issues need to be corrected again
    #(the data base has not been changed, it was just corrected for EU-weighted parameters)
    # Trend plots for AD-EM
    
    # IEF plots done above
    #source("eugirpD.2_iefplots.r")
    
    # Trend plots for IEF
    
    # Country-plots for IEF
 
    #stepsdone<-checksteps+1
    #source("curplot.r")
}else if(stepsdone>checksteps){
    print(paste0("Step ",checksteps+1,": All Sector-3 plots already done"))
}

