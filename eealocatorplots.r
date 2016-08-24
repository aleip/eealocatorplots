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

# current inventory year
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
    adrian<-"c:/Adrian/"
}else if(Sys.info()[4]=="D01RI1600881"){
    adrian<-"x:/Adrian/"
}else{
    adrian<-"C:/Adrian/"
}
locplots<-paste0(adrian,"/data/inventories/ghg/unfccc/eealocatorplots")           #!!!
setwd(locplots)
options(warn=0)
source("curplot.r")
options(warn=2) #warn=2 turns warnings into errors; set to 0 if this should be avoided
options(error=NULL) #error=recover goes into debug mode
options(error=recover) #error=recover goes into debug mode

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
    # Remove UK (use GB) and remove Island ####
    
    alldata$datasource<-"nir"
    source("eugirpA.2_meastype.r")
    
    # Deal with other animals and animal types without sector number
    selection<-alldata$sector_number=="" & alldata$classification%in%mslivestockclass
    selection<-selection | grepl("^3",alldata$sector_number)
    allagri<-alldata[selection,allfields]
    alldata<-alldata[!selection,allfields]
    
    #stop("Plause")
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

#stop("second step done")
# B.1 - Plots 1. Calculate EU-sums and simplify units (remove very large numbers) ####
if(stepsdone==2){
    print("Step 3a: Calculating EU-sums only for summable variables")

    alldata<-alldata[alldata$party!="EU28",]
    alldata$datasource<-"nir"
    calceu<-alldata[grepl("^3",alldata$sector_number),]
    alldata<-alldata[!grepl("^3",alldata$sector_number),]
    
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
    print("Calculate EU-sums")
    calceu<-eu28sums(calceu,"EUC")
    eu28sum<-calceu[calceu$meastype %in% meas2sum & calceu$party=="EUC",]
    agrisummeas<-measures2sum[grepl("^3",measures2sum$sector_number),]
    
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
    alldata<-rbind(alldata,calceu)
    o<-order(alldata$sector_number,alldata$category,alldata$meastype,alldata$classification,alldata$party)
    alldata<-alldata[o,allfields]
    
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

#stop("Third step done")
# B.2 - Plots 1. Do emission plots ####
#emplotsdone<-1
#doemissionplots<-TRUE
if(stepsdone>2){
    if(doemissionplots==TRUE){
        if(emplotsdone==0){
            print("Step 4: Emission plots")
            adempars<-c("AD","EM")
            
            rundata<-"adem"
            runfocus<-"value"
            datasource<-"nir"
            alldata$autocorr<-NA
            
            #Temporary correction!!!!
            #sel<-alldata$sector_number=="3.B.2.1"&alldata$category=="Dairy Cattle"&alldata$party=="CZ"&alldata$unit=="kt N/year"
            #alldata[sel,"2000"]<-alldata[sel,"2000"]/2000
            #sel<-allagri$sector_number=="3.B.2.1"&allagri$category=="Dairy Cattle"&allagri$party=="CZ"&allagri$unit=="kt N/year"
            #allagri[sel,"2000"]<-allagri[sel,"2000"]/2000
            
            temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
            plotdata<-temp[[1]]
            plotmeas<-temp[[2]]
            adddefault<-temp[[3]]
            sharesexist<-temp[[4]]
            
            x1<-1;x2<-nrow(plotmeas)
            x1<-368;x2<-nrow(plotmeas)
            x1<-1;x2<-2
            for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")}
            plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
            write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")
            
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

#stop("plots done")
#++++ END OF GENERAL PART 
#++++ BELOW SECTOR-3 SPECIFIC PART
# A.3 Calculate trend and growth rates ####
if(stepsdone==3){
    print("Step 4: Calculating trends and growth rates")
    
    nyears<-length(years)
    period1<-as.character(years[1]:years[nyears-1])
    period2<-as.character(years[2]:years[nyears])
    
    #agriemissions$option[agriemissions$option==0]<-""
    agriemissions$datasource<-"nir"
    agriemissions$option<-""
    agriemissions<-unique(agriemissions)
    
    # Agrishares as agriemissions relative to total emissions ####
    totaluid<-unique(alltotals$variableUID[alltotals$classification==signclass&alltotals$gas=="Aggregate GHGs"])
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
    checksteps<-4
    savelist<-c(savelist,"alltrend","allgrowth","agrishares","signcategories","signthreshold")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    save(checksteps,file="checksteps.RData")
    source("curplot.r")
}else if(stepsdone>3){
    print("Step 4: Trends and growth rates already calculated")
}

#stop("step 4 done")
# A.4 NE-check and check on unit errors. Prepare for outlier check ####
if(stepsdone==4){
    print("# A.4 NE-check and check on unit errors. Prepare for outlier check ####")
    #load("checksteps.Rdata")
    #print(checksteps)
    
    #if(checksteps == "4"){
    print(paste0("Step ",stepsdone+1,"a: Caluclate allagri for EU28"))
    allagri$datasource<-"nir"
    allagri<-allagri[allagri$party!="EU28",]
    allagri<-eu28sums(allagri,aeu = c("EUC","EUA"))
    allagri<-allagri[order(allagri$sector_number,allagri$category,allagri$meastype),]
    #remove option from Cattle, Dairy Cattle, Non-Dairy Cattle
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option[allagri$category%in%allcattle]<-""
    allagri<-unique(allagri)
    curnames<-c("sector_number","category","meastype","option","party","variableUID",years)
    #checksteps<-"4a"
    #save(checksteps,file="checksteps.RData")
    #}   
    
    # Create table with statistical info for each variable
    agrimeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
    #parammeas<-unique(subset(param,select=names(param)[!names(param) %in% c("notation","party",years)]))
    #agrimeas<-merge(agrimeas,parammeas,by="variableUID",all.x = TRUE)
    agrimeas<-agrimeas[order(agrimeas$sector_number,agrimeas$category),]
    
    #if(checksteps == "4a"){
    print(paste0("Step ",stepsdone+1,"b: Check NEs"))
    if (! file.exists("../2016/checks/nechecks")){dir.create(file.path("../2016/checks/necheck/"))}
    source("eugirp_checknes.r")
    #checksteps<-"4b"
    #save(checksteps,file="checksteps.RData")
    #}
    #if(checksteps == "4b"| checksteps=="4c"){
    print(paste0("Step ",stepsdone+1,"c: Check units"))
    if (! file.exists("../2016/checks/autocorrections")){dir.create(file.path("../2016/checks/autocorrections/"))}
    source("eugirp_checkunits.r")
    
    print(paste0("Step ",stepsdone+1,"d: Calculation statistics distribution for parameters and growth"))
    newcols<-c("min","p25","median","p75","max")
    source("eugirp_paramdata.r")
    #checksteps<-"4c"
    #save(checksteps,file="checksteps.RData")
    
    stepsdone<-5
    savelist<-c(savelist,"agrimeas","param","growth","autocorrections")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>4){
    print("Step 5: NEchecks and Unitchecks done; param and growth prepared for outlier checks.")
}


#stop("step 5 done")
# A.3 Check for outlier errors and calculate EU weighted averages ####
if(stepsdone==5){
    
    print("# A.3 Check for outlier errors and calculate EU weighted averages ####")
    print(paste0("Step ",stepsdone+1,"a: Check for outlier errors on parameters"))
    outcheck<-"param"
    source("eugirp_checkoutliers.r")
    source("eugirp_ipccdefaults.r")
    print(paste0("Step ",stepsdone+1,"b: Check for outlier errors on growth"))
    outcheck<-"growth"
    nyears<-length(years)
    
    #allcurve not very relevant
    #period1<-as.character(yearsnum[1]:yearsnum[nyears-1])
    #period2<-as.character(yearsnum[2]:yearsnum[nyears])
    #allcurve<-as.data.frame(matrix(0,rep(0,ncol(alltrend)),ncol=ncol(alltrend)))
    #allcurve<-alltrend[alltrend$meastype %in% meas2sum,]
    #allcurve[,period2]<-alltrend[alltrend$meastype %in% meas2sum,period2]-alltrend[alltrend$meastype %in% meas2sum,period1]
    #allcurve[,years[1]]<-NA
    #allgcurve<-allgrowth
    #allgcurve[,period2]<-allgrowth[,period2]/allgrowth[,period1]
    source("eugirp_checkoutliers.r")
    # Add the gas to each measure so see what the corresponding emissions are
    levels(paramcheck$gas)<-factor(signcategories$gas)

    # Ispotentialissue:
    # Method: (a) Significant issue only if the last year is included in the list of outliers
    #         (b) Checks if the difference to using the median reported value is significant
    #             acc. to the significance threshold. Uses data frame significantcategories
    #             calculated earlier
    # Flags: over --> issue is a potential overestimation
    #        overnotlast ---> issue is a potential overestimation but last year is not identified
    #        oversource ---> issue interests a significant source category, but overestimation might be below threshold
    #        ... in analogy for 'under'
    paramcheck[,testfields]<-Reduce(rbind,lapply(c(1:nrow(paramcheck)),function(x) Reduce(cbind,ispotentialissue(paramcheck[x,],signcategories,as.character(signyear),signthreshold))))
    paramchecked<-0
    
    # Load IPCC default values
    paramcheck<-loadipccdefaults(paramcheck,1,nrow(paramcheck),insert="value")
    write.csv(paramcheck,file="paramcheck_ipcc.csv")
    
    print(paste0("Step ",stepsdone+1,"c: Write country outlier list"))
    rundata<-"ief"
    runfocus<-"value"
    #plotparamcheck: plotmeas is overwritten by paramcheck
    plotparamcheck<-0
    # Create link to plots
    paramcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(invyear,"..",unlist(lapply(c(1:nrow(paramcheck)),function(x) 
                        plotname("",plotsdir,issuedir,"*",paramcheck[x,sectfields],paramcheck[x,metafields],paramcheck[x,measfields],
                                 runfoc="1VAL",figdate,plotformat,rundata,cursubm,plotparamcheck=0))))),
               "\",\"Link to plot\")")
    t<-growthcheck
    co<-c("plot",names(growthcheck))
    growthcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(invyear,"..",unlist(lapply(c(1:nrow(growthcheck)),function(x) 
                        plotname("",plotsdir,issuedir,"*",growthcheck[x,sectfields],growthcheck[x,metafields],growthcheck[x,measfields],
                                 runfoc="1VAL",figdate,plotformat,rundata,cursubm,plotparamcheck=0))))),
               "\",\"Link to plot\")")
    
    # Make growth plots to check ... improve loop!!
    mainanimals<-c("Dairy Cattle","Non-Dairy Cattor","Sheep","Swine","Poultry")
    mainmeasures<-c("AD","IEF","POP","AREA","NRATE","FracGASF","FracGASM","FracLEACH")
    #temporarycommented for(mm in mainmeasures) makegrowthplot(sec="3.",meastype=mm)
    
    
    #corfile<-paste0(filoutliers,"list_checked_GC2.csv")
    #corrections<-read.csv(file=corfile,comment.char = "#",header=TRUE)
    #toflag<-corrections[corrections$correction=="flag",]
    #names(toflag)<-yearheaders(names(toflag))
    
    keycategories<-keysources()
    test0<-matrix(rep(0,nrow(growthcheck)*length(flag4issues)),ncol=length(flag4issues),nrow=nrow(growthcheck))
    test0<-as.data.frame(test0)
    names(test0)<-flag4issues
    growthcheck<-growthcheck[,-which(names(growthcheck)=="gas")]
    growthcheck<-cbind(growthcheck,test0)
    co<-names(growthcheck)
    # Integrate outcome into paramcheck and to writeoutlierlist!!
    x1<-1;x2<-10
    x1<-nrow(paramcheck);x2<-nrow(growthcheck)
    growthcheck<-growthcheck[growthcheck$sector_number!="3.i",]
    x1<-1;x2<-nrow(growthcheck)
    test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(growthcheck[x,],"growth",x)))
    test<-Reduce(rbind,test)
    growthcheck[x1:x2,flag4issues]<-test
    #write.csv(test,file=paste0(filoutliers,"list_checked4emrt.csv"))
    
    
    test<-as.data.frame(test)
    names(test)<-flag4issues
    write.csv(test,file=paste0(filoutliers,"list_checked4emrt.csv"))
    source("eugirp_writeoutlierlist.r")
    
    print(paste0("Step ",stepsdone+1,"d: Calculate EU weighted averages"))
    #stop("now write issues")
    #paramcheck$correction[paramcheck$party=="SE"&paramcheck$meastype=="VSEXC"]<-0
    source("eugirp_euweightedaverages.r")
    export4uba(allagri = allagri)
    
    print(paste0("Step ",stepsdone+1,"e: Make plots"))
    datasource<-"nir"
    runfocus<-"value"
    rundata<-"ief"
    #source("eugirp_prepareplots.r")
    temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
    plotdata<-temp[[1]]
    plotmeas<-temp[[2]]
    adddefault<-temp[[3]]
    sharesexist<-temp[[4]]
    
    x1<-1;x2<-nrow(plotmeas)
    x1<-97;x2<-nrow(plotmeas)
    x1<-7;x2<-7
    for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")}
    for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = "range",eusubm = "EUC")}
    plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
    write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")
    
    
    stepsdone<-6
    savelist<-c(savelist,"growthcheck","paramcheck","autocorrections","paramchecked","keycategories")
    save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    savelist<-c(savelist,"assignad2par")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
    
}else if(stepsdone>5){
    print(paste0("Step 5: Check for outlier errors already done"))
}

stop("step 6 done")
# C - Make checks for sector 3 ####
checksteps<-6
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
    
    keycategories<-keysources()
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option<-""
    allagri<-unique(allagri)
    
    source("agrichecks1ADs.r")
    source("agrichecks2Nex.r")
    
    #agrichecks<-rbind(checks[names(check1)],check1,check2,check3,check4,check5)
    agrichecks<-checks
    #agrinames<-c("test","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved,docfields)
    agrinames<-c("test","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved)
    names(agrichecks)<-agrinames
    agrichecks<-agrichecks[agrichecks$party!="all",]
    agrichecks<-filldf(agrichecks,allcheckfields)
    climcheck<-filldf(climcheck,names(agrichecks))
    climcheck<-climcheck[,names(agrichecks)]
    agrichecks<-filldf(agrichecks,climcheck)
    
    print("Bind climacheck and agricheck")
    agrichecks<-rbind(agrichecks,climcheck)
    
    # Integrate outcome into paramcheck and to writeoutlierlist!!
    x1<-55;x2<-56
    x1<-1;x2<-nrow(agrichecks)
    test<-Reduce(rbind,lapply(c(x1:x2),function(x) unlist(flags4newissue(agrichecks[x,],"agri"))))
    agrichecks[x1:x2,flag4issues]<-test
    write.csv(agrichecks[allcheckfields4emrt],file=paste0(issuedir,"agrichecks.csv"))
    
    
    
    stepsdone<-checksteps+1
    savelist<-c(savelist,"checkuids","agrichecks")
    #save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>checksteps){
    print(paste0("Step ",checksteps+1,": Sector 3 checks already done"))
}
stop("Step 7 done")



# A.3 Determine Key categories ####
if(stepsdone==7){
    
    # Key source categories ####
    
    print(paste0("Step ",stepsdone+1,": Key source category analysis (unfinished)"))
    # Make sure that the categories listed are complete but not redundant!!
    # if necessary, do some tests!
    # Check at the end if the totals are OK
    keycats<-read.table("keycategories.txt")
    keycats<-as.vector(keycats$V1)
    
    select<-alldata$variableUID%in%keycats
    keycategories<-alldata[select,]
    
    
    alldata$digit<-nchar(alldata$sector_number)-nchar(gsub("\\.","",alldata$sector_number))
    allsectors<-unique(subset(alldata,select=c("sector_number","digit")))
    allemissions<-alldata[alldata$meastype=="EM"&alldata$digit!=0,]
    allemissions<-allemissions[!grepl("Aggregate",allemissions$gas),]
    
    allsectors$digit<-nchar(allsectors$sector_number)-nchar(gsub("\\.","",allsectors$sector_number))
    allsectors$digit<-nchar(allsectors$sector_number)
    select<-grepl(" $",allagri$sector_number)
    allagri$sector_number[select]<-(gsub("\\.$","",allagri$sector_number[select]))
    
    
    #select<-allmethods$variableUID%in%keycats
    #keymethods<-allmethods[select,]
    #agrimethods<-allmethods[grepl("^3",allmethods$sector_number),]
    #agrimethods<-agrimethods[agrimethods$notation!="NA",]
    
    # Convert all emissions into CO2eq
    select<-keycategories$gas=="CH4"
    keycategories[select,years]<-keycategories[select,years]*gwps[1]
    select<-keycategories$gas=="N2O"
    keycategories[select,years]<-keycategories[select,years]*gwps[3]
    
    # Define reference total 
    select<-alldata$gas=="Aggregate GHGs" & grepl("^[1-6].$",alldata$sector_number)
    select<-select | alldata$gas=="Aggregate GHGs" & grepl("Total",alldata$sector_number)
    totem<-alldata[select,]
    
    # Rank all emissions according to their contribution to total
    # (convert emissions into share of total)
    
    # Calculate cumulative share
    
    
    # Assign yes/no if cumulative share is above/below threshold
    
    
    print(paste0("Step ",checksteps,": Save results"))
    stepsdone<-7
    savelist<-c(savelist,"keycategories","totem")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    source("curplot.r")
}else if(stepsdone>5){
    print(paste0("Step 7: Key category analysis errors already done"))
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
        notresolved[c(firstissue:lastissue),"issueID"]<-unlist(lapply(c(firstissue:lastissue),function(x) writeoutlierissue(allagri,notresolved[x,],listoftext)))
        
        
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

