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
  adrian<-"D:/Users/leipadr/adrian/"
}else if(Sys.info()[4]=="D01RI1600881"){
  adrian<-"x:/Adrian/"
  adrian <- "\\\\s-jrciprap246p.jrc.it/dev/ghginventory/"
}else if(Sys.info()[4]=="D01RI1701864"){
  adrian<-"E:/ghginventory/"
}else if(Sys.info()[4]=="BARAGOON"){
  adrian<-"C:/dev/ghginventory/"
}else if(Sys.info()[4]=="MARSBL1BHL"){
  adrian<-"X:\\Agrienv\\ghginventory\\"
}else if(Sys.info()[4]=="D01RI1600850"){# Gema PC
  adrian<-"D:\\Users\\carmoge\\Documents\\GitHub\\"
}else if(Sys.info()[4]=="S-JRCIPRAP246P"){# New server
  adrian<-"D:\\dev\\ghginventory"
}else{
  adrian<-"C:/Adrian/"
}

locplots<-paste0(adrian,"/data/inventories/ghg/unfccc/eealocatorplots")           #!!!
if(Sys.info()[4]=="MARSBL1BHL")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="BARAGOON")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1701864")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1600881")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1600850")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="S-JRCIPRAP246P")locplots<-paste0(adrian,"/eealocatorplots")
setwd(locplots)
#setwd("\\\\s-jrciprap246p.jrc.it\\dev\\ghginventory\\eealocatorplots")   #Activate this to work with the data from the server remotely
                                                                          #Also activate remote2server (remote2server <- 1) in 'curplot.r'  
options(warn=0)
source("curplot.r")
options(warn=0) #warn=2 turns warnings into errors; set to 0 if this should be avoided
options(error=recover) #error=recover goes into debug mode
options(error=NULL) #error=recover goes into debug mode

# PART A: Link with EEA locator tool ----
# A.1 Load eea-locator data (either from text file or from pre-processed Rdata file) ####
# Return:
# - alldata: data frame containing (almost) all EEA-locator data. Years are transposed to columns.
#            Some cleaning on uids has been done, data rows with irrelevant gases, GBE removed (GBK
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

#  source("curplot.r")
if(stepsdone==1){
  print("Step 2: Generate list of measures and deal with animals")
  # Remove GBE (use GBK) and remove Island ####
  
  alldata$datasource<-"nir"
  alldata$notation<-""
  source("eugirpA.2_meastype.r")
  
  # Deal with other animals and animal types without sector number
  selection<-alldata$sector_number=="" & alldata$classification%in%mslivestockclass
  selection<-selection | grepl("^3",alldata$sector_number)
  allagri<-alldata[selection,allfields, with=FALSE]
  alldata<-alldata[!selection,allfields, with=FALSE]
  save(allagri,file="tmpallagri60.rdata")
  #load("tmpallagri60.rdata", verbose = TRUE)
  #stop("Plause")
  source("eugirp_otherlivestock.r")
  # Calculate parameter for parent category 'swine' and 'sheep ####
  allagri$method<-""
  allagri65<-allagri #keep 65 here!
  sheepswine<-c("Sheep","Swine")
  addparentanimal<-allagri #needs addparentanimal
  source("eugirp_aggparentanimal.r")
  
  allagri70<-addparentanimal 
  curagri<-addparentanimal  #needs curagri
  source("eugirp_cattle.r")
  allagri177<-curagri
  allagri<-curagri
  allagri$option[allagri$category%in%allcattle]<-""
  
  
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
  
  if(!is.null(keepNORout)){ 
    print("Keeping Norway out")
    alldata_NOR<-alldata[alldata$party == "NOR",]
    alldata<-alldata[alldata$party != "NOR",]
  }
  
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
  #xavi20180504: calceu[,years]<-apply(calceu[,years],2,function(x) as.numeric(x))
  #xavi20180504: calceu<-eu28sums(calceu,"EUC",years = years)
  #xavi20180504: eu28sum<-calceu[calceu$meastype %in% meas2sum & calceu$party=="EUC",]
  #xavi20180504: calceu <- calceu[!calceu$party=="EUC",] #xavi20180219:removing EUC sums, both well and wrongly calculated (i.e. meas2popweight, CLIMA and MCF, which need to be averaged)
  calceu <- eu28sums(A = calceu,years = years) #xavi20180504: to calculate sums for EUA and EUC
  eu28sum <- calceu[calceu$meastype %in% meas2sum & calceu$party %in% eu,]
  calceu <- calceu[!calceu$party %in% eu,] #xavi20180219:removing EUC and EUA sums, both well and wrongly calculated (i.e. meas2popweight, CLIMA and MCF, which need to be averaged)
  calceu <- rbind(calceu, eu28sum) #xavi20180219: so that calceu keeps only EUC and EUA sums for those that can be summed (i.e. meas2sum), but all the sector n.3 (agri)
  agrisummeas<-measures2sum[grepl("^3",measures2sum$sector_number),]
  
  # Check on outliers in AD and EMs: no country should really dominate unless it is the only country reporting
  
  sharecalc<-function(uid,D,E,x, cursubm){
    #print(x)
    uid<-as.vector(unlist(uid))
    #xavi20180504: alc<-c(1:length(countriesnoeu))
    countriesnoeu1 <- countriesnoeu[countriesnoeu %in% as.character(country4sub[country4sub[,"EUC"]==1,"code3"])]
    if(!is.null(keepNORout)){
      countriesnoeu1 <- countriesnoeu1[!countriesnoeu1 %in% c("NOR")]
      D <- D[!D$party %in% c("NOR"), ]
    } 
    alc<-c(1:length(countriesnoeu1))
    coval<-extractuiddata(DF = D,uid = uid,c = countriesnoeu1,narm = FALSE, cursubm = cursubm)
    ncoval<-coval[apply(coval,1,sum,na.rm=TRUE)!=0,]
    if(is.matrix(ncoval)){ncoval<-nrow(ncoval)}else{ncoval<-0}
    
    #xavi20180504: euval<-E[E$variableUID==uid,years]
    euval<-E[E$variableUID==uid,]
    euval<-euval[euval$party=="EUC",years]
    #euval<-euval[E$category[E$variableUID%in%uid]%in%c("Other Cattle.Non-dairy cattle","Other Cattle.Dairy cattle"),]
    euval<-as.numeric(matrix(euval))
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
  cat("\n")
  print("Calculate calcshare")
  calcshare<-lapply(c(1:nrow(agrisummeas)),function(x) 
    Reduce(rbind,sharecalc(uid=agrisummeas$variableUID[x],D=calceu,E=eu28sum,x, cursubm = cursubm)))
  calcshare<-as.data.frame(t(Reduce(cbind,calcshare)))
  names(calcshare)<-c("ncountries","party","year","share","variableUID","mean","meanother")
  ademoutl<-merge(calcshare,agrisummeas,by="variableUID")
  ademoutl<-simplifytestmatrix(check = ademoutl,group = c("year","share"),compare = list(years,"range"))
  ok1<-ademoutl$party=="ITA"&ademoutl$category=="Buffalo"
  ok2<-ademoutl$party=="DNM"&ademoutl$source=="Digesters"
  ok3<-ademoutl$party=="NLD"&ademoutl$option=="Option B"
  ok4<-ademoutl$party=="ROU"&grepl("^3.F.",ademoutl$sector_number)
  ok6<-ademoutl$party=="ESP"&grepl("^3.F.",ademoutl$sector_number) #20170127 burning of agricultural residues, no area burnt is reported (NA) but some biomass available (table 3.F, column C) from soybean and other non-specified. Reported emissions are NO.
  ok5<-ademoutl$party=="SWE"&grepl("^3.*8$",ademoutl$sector_number)
  ademoutl<-ademoutl[!(ok1 | ok2 | ok3 | ok4 | ok5 | ok6),]
  ademoutl$correction<-0
  ademoutl<-ademoutl[order(ademoutl$party,ademoutl$sector_number),]
  if(nrow(ademoutl)>0){write.csv(ademoutl,file=paste0(invloc,"/checks/checks",cursubm,"ADEMoutliers.csv"))}
  
  
  #source("eugirp_simplifyunit.r")
  #     selection<- (calceu$unit=="t N/year")
  #     calceu$unit[selection]<-"kt N/year"
  #     calceu[selection,years]<-calceu[selection,years]/1000
  
  #save alldata before unit conversion as backup
  #xavi20180219: save(alldata,eu28sum,file=gsub("_clean","_nounitconv",rdatallem))
  save(alldata,eu28sum, calceu,file=gsub("_clean","_nounitconv",rdatallem))
  
  # NEXC must be all changed, even if for one MMS the values are small
  
  if(!is.null(keepNORout)){
    alldata_NOR_agri <- alldata_NOR[grepl("^3", alldata_NOR$sector_number), ]
    alldata_NOR_notAgri <- alldata_NOR[!grepl("^3", alldata_NOR$sector_number), ]
    calceu <- rbind(calceu, alldata_NOR_agri)
  }
  selection<- (grepl("^3",calceu$sector_number) & calceu$unit=="kg N/year")
  calceu$unit[selection]<-"kt N/year"
  calceu[selection,years]<-calceu[selection,years]/1000000
  
  if(!is.null(keepNORout)){
    alldata<-rbind(alldata, alldata_NOR_notAgri, calceu)
  }else{
    alldata<-rbind(alldata, calceu)
  }
  
  o<-order(alldata$sector_number,alldata$category,alldata$meastype,alldata$classification,alldata$party)
  alldata<-alldata[o,allfields]
  
  #stop("pause")
  source("eugirp_allagri.r")  #20190125: allagri data WITH Norway (alldata WITHOUT Norway)
  
  stepsdone<-3
  emplotsdone<-0
  savelist<-c(savelist,"emplotsdone","eu28sum","ademoutl","allagri","agrimethods","agriemissions", "agriemissions_GBE","agridet","agrimix","agrigen", "alldata_NOR", "acountry")
  save(list=savelist,file=rdatallem)
  save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
  #drive_update(file = paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
  #             media = rdatallem, 
  #             verbose = FALSE)
  #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
  #             #path = NULL, 
  #             #name = NULL, type = NULL, 
  #             verbose = FALSE)
  source("curplot.r")
}else if(stepsdone>2){
  print("Step 3a: EU sums already calculated")
}

#stop("Third step done")
# B.2 - Plots 1. Do emission plots ####
#emplotsdone<-0
doemissionplots <- FALSE
doemissionplots<-TRUE
if(stepsdone>2){
    if(doemissionplots==TRUE){
        if(emplotsdone==0){
            print("Step 4: Emission plots")
            if(!is.null(keepNORout)) print("Keeping Norway out of these plots")
          
            adempars<-c("AD","EM")
            
            rundata<-"adem"
            runfocus<-"value"
            datasource<-"nir"
            
            #Do temporary corrections for observations
            
            #if(cursubm == "20190508" & file.info(paste0(csvfil,".txt"))$ctime < "2019-05-11 15:11:54 CEST"){
            #  alldata[alldata$variableUID == "E92759E4-BD91-46D2-BEE4-B21C3C0D3207" & alldata$party == "POL", ]$`1995` <- 146800
            #  alldata[alldata$variableUID == "131C4470-1F4D-4A5C-93F9-094BC7BA86F9" & alldata$party == "POL", ]$`1995` <- 52000
            #}
            
            temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
            plotdata<-temp[[1]]
            plotdata$autocorr<-NA
            plotdata$correction<-1
            plotmeas<-temp[[2]]
            adddefault<-temp[[3]]
            sharesexist<-temp[[4]]

            x1<-368;x2<-nrow(plotmeas)
            x1<-155;x2<-156
            x1<-167; x2<-nrow(plotmeas)
            x1<-1; x2<-nrow(plotmeas)
            #imeas <- 171
            #loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")
            #View(alldata[alldata$variableUID == "91817067-8DB6-41D6-A348-57E2C17B655D", ])
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
            #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"), 
            #             media = rdatallem, 
            #             verbose = FALSE)
            #drive_upload(media = gsub(".RData",paste0("_plots","~",figdate,".RData"),rdatallem), 
            #             #path = NULL, 
            #             #name = NULL, type = NULL, 
            #             verbose = FALSE)
            #stop("End of general part (Emission plots done!)")
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
# 2019: Norway is included in all checks
# A.3 Calculate trend and growth rates ####
if(stepsdone==3){
    print("Step 4: Calculating trends and growth rates")
    if(!is.null(keepNORout)) print("Including (again) Norway")
    
    nyears<-length(years)
    period1<-as.character(years[1]:years[nyears-1])
    period2<-as.character(years[2]:years[nyears])
    
    #agriemissions$option[agriemissions$option==0]<-""
    agriemissions$datasource<-"nir"
    agriemissions$option<-""
    agriemissions<-unique(agriemissions)
    
    # Agrishares as agriemissions relative to total emissions ####
    # 2019: both 'alltotals' and 'agriemissions' include Norway
    totaluid<-as.vector(unique(alltotals$variableUID[alltotals$classification==signclass&alltotals$type==signtype&alltotals$gas=="Aggregate GHGs"]))
    totalval<-alltotals[alltotals$classification==signclass&alltotals$gas=="Aggregate GHGs",]
    totalval<-extractuiddata(DF = alltotals,uid = totaluid,c = allcountries,narm=FALSE, cursubm = cursubm)
    totalval<-apply(totalval,2,function(x) as.numeric(x))
    agrishares<-unique(agriemissions[,allfields[!allfields%in%c("party",years,"notation")]])
    shareuids<-unique(agrishares$variableUID)
    if(nrow(agrishares)!=length(shareuids)){stop("number of uids inconsistent with data frame agrishares")}
    tmp<-Reduce(rbind,lapply(c(1:length(shareuids)),function(x) 
        calculateshares(x,shareuids[x],agriemissions,totalval)))
    
    selection<-is.na(apply(tmp[,years],1,sum,rm.na=TRUE)) | apply(tmp[,years],1,sum,rm.na=TRUE)==0
    tmp<-tmp[!selection,]
    agrishares<-merge(agrishares,tmp,by=c("variableUID"))
    agrishares$notation<-""
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
    
    if (! file.exists(paste0(invloc,"/checks/significant/"))){dir.create(file.path(paste0(invloc,"/checks/significant")),showWarnings = FALSE )}
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
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>3){
    print("Step 4: Trends and growth rates already calculated")
}

#stop("step 4 done")
# A.4 NE-check and check on unit errors. Prepare for outlier check ####
if(stepsdone==4){
    print("# A.4 NE-check and check on unit errors. Prepare for outlier check ####")
    if(!is.null(keepNORout)){
      print("Norway data is included for these checkings")
      alldata <- rbind(alldata, alldata_NOR)
    } 
    #load("checksteps.Rdata")
    #print(checksteps)
    
    #if(checksteps == "4"){
    print(paste0("Step ",stepsdone+1,"a: Caluclate allagri for EU28"))
    allagri$datasource<-"nir"
    allagri<-allagri[allagri$party!="EU28",]
    #xavi20180220: allagri<-eu28sums(allagri,aeu = c("EUC","EUA"),years = years)    #xavi20180220: this is already done for EUC (I think not necessary for EUA?)
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
    if (! file.exists(paste0(invloc,"/checks/nechecks"))){
        dir.create(file.path(paste0(invloc,"/checks/necheck/")))}
    source("eugirp_checknes.r")
    #checksteps<-"4b"
    #save(checksteps,file="checksteps.RData")
    #}
    #if(checksteps == "4b"| checksteps=="4c"){
    print(paste0("Step ",stepsdone+1,"c: Check units"))
    if (! file.exists(paste0(invloc,"/checks/autocorrections"))){dir.create(file.path(paste0(invloc,"/checks/autocorrections/")))}
    source("eugirp_checkunits.r")
    
    print(paste0("Step ",stepsdone+1,"d: Calculation statistics distribution for parameters and growth"))
    newcols<-c("min","p25","median","p75","max")
    source("eugirp_paramdata.r")
    #checksteps<-"4c"
    #save(checksteps,file="checksteps.RData")
    
    # removing again NOR data
    #if(!is.null(keepNORout)) alldata <- alldata[alldata$party != "NOR",] 
    
    stepsdone<-5
    savelist<-c(savelist,"agrimeas","agrinotations","param","growth","autocorrections")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>4){
    print("Step 5: NEchecks and Unitchecks done; param and growth prepared for outlier checks.")
}


#stop("step 5 done")
# A.3 Check for outlier errors and write outlier lists ####
if(stepsdone==5){
    
    print("# A.3 Check for outlier errors and calculate EU weighted averages ####")
    #if(!is.null(keepNORout)){
    #  print("Norway data is included for these checkings (not for calculating EU weighted averages!!)")
    #  alldata <- rbind(alldata, alldata_NOR)
    #} 
  
    print(paste0("Step ",stepsdone+1,"a: Check for outlier errors on parameters @ ",curtime(1)))
    outcheck<-"param"
    source("eugirp_checkoutliers.r")
    #source("eugirp_ipccdefaults.r")
    print(paste0("Step ",stepsdone+1,"b: Check for outlier errors on growth @ ",curtime(1)))
    
    outcheck<-"growth"
    nyears<-length(years)
    source("eugirp_checkoutliers.r")
    # Add the gas to each measure so see what the corresponding emissions are
    #al20200127 ... gas already availabel in paramcheck - not clear what the formula below was doing
    #levels(paramcheck$gas)<-factor(signcategories$gas)
    paramcheck<-paramcheck[!paramcheck$party%in%eu,]
    paramcheck<-paramcheck[remagglevel(paramcheck,mt = 1),]
    
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

    print(paste0("Step ",stepsdone+1,"c: Prepare for plot-names @ ",curtime(1)))
    rundata<-"ief"
    runfocus<-"value"
    #plotparamcheck: plotmeas is overwritten by paramcheck
    plotparamcheck<-0
    # Create link to plots
    paramcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(plotsdir,"../../plots/",unlist(lapply(c(1:nrow(paramcheck)),function(x) 
                        plotname("",plotsdir,issuedir,"*",paramcheck[x,sectfields],paramcheck[x,metafields],paramcheck[x,measfields],
                                 paste0("nir",runfocus),figdate,plotformat,rundata,cursubm,plotparamcheck=0))))),
               "\",\"Link to plot\")")
    t<-growthcheck
    co<-c("plot",names(growthcheck))
    growthcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(plotsdir,"../../plots/",unlist(lapply(c(1:nrow(growthcheck)),function(x) 
                        plotname("",plotsdir,issuedir,"*",growthcheck[x,sectfields],growthcheck[x,metafields],growthcheck[x,measfields],
                                 paste0("nir",runfocus),figdate,plotformat,rundata,cursubm,plotparamcheck=0))))),
               "\",\"Link to plot\")")
    
    
    print(paste0("Step ",stepsdone+1,"d: Calculate key categories @ ",curtime(1)))
    source("eugirp_functions.r")
    keycategories<-keycategories()
    keyeuagri<-keycateuc()
    
    print(paste0("Step ",stepsdone+1,"e: Prepare flags @ ",curtime()))
    test0<-matrix(rep(0,nrow(growthcheck)*length(flag4issues)),ncol=length(flag4issues),nrow=nrow(growthcheck))
    test0<-as.data.frame(test0)
    names(test0)<-flag4issues
    growthcheck<-growthcheck[,-which(names(growthcheck)=="gas")]
    growthcheck<-cbind(growthcheck,test0)
    growthcheck$party<-as.character(growthcheck$party)
    print(paste0("Step ",stepsdone+1,"f: Integrate outcome into growthcheck and to writeoutlierlist @ ",curtime()))
    x1<-1;x2<-nrow(growthcheck)
    test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(growthcheck[x,],"growth",x)))
    test<-Reduce(rbind,test)
    growthcheck[x1:x2,flag4issues]<-test
    
    print(paste0("Step ",stepsdone+1,"g: Load now solved issues @ ",curtime()))
    growthcheck<-addsolved2check(growthcheck,c("recalc"))
    cog<-names(growthcheck)
    
    test0<-matrix(rep(0,nrow(paramcheck)*length(flag4issues)),ncol=length(flag4issues),nrow=nrow(paramcheck))
    test0<-as.data.frame(test0)
    names(test0)<-flag4issues
    paramcheck<-paramcheck[,-which(names(paramcheck)=="gas")]
    paramcheck<-cbind(paramcheck,test0)
    paramcheck$party<-as.character(paramcheck$party)
    # !!
    print(paste0("Step ",stepsdone+1,"h: Integrate outcome into paramcheck and to writeoutlierlist @ ",curtime()))
    x1<-1;x2<-nrow(paramcheck)
    source("eugirp_functions.r")
    keycategories <- keycategories()  #xavi20180131
    test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(paramcheck[x,],"outlier",x)))
    test<-Reduce(rbind,test)
    paramcheck[x1:x2,flag4issues]<-test

    #Load now solved issues!
    # --> function addsolved2check in file eugirp_functions.r (ca line 1160)
    paramcheck<-addsolved2check(paramcheck,c("recalc"))
    cof<-names(paramcheck)
    
    print(paste0("Step ",stepsdone+1,"i: Write country outlier list @ ",curtime()))
    test<-as.data.frame(test)
    names(test)<-flag4issues
    write.csv(test,file=paste0(filoutliers,"list_checked4emrt.csv"))
    
    print(paste0("Step ",stepsdone+1,"j: Write outlier list @ ",curtime(1)))
    source("eugirp_writeoutlierlist.r")
    
    # removing again NOR data
    #if(!is.null(keepNORout)) alldata <- alldata[alldata$party != "NOR",] 
    
    stepsdone<-6
    savelist<-c(savelist,"growthcheck","paramcheck","paramchecked","keycategories")
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>5){
    print(paste0("Step 6: Check for outliers errors already done"))
}

    
#stop("step 6 done")
# Calculate EU weighted averages and make adem and ief plots####
if(stepsdone==6){
    # Make growth plots to check ... improve loop!!
    #temporarycommented 
    print(paste0("Step ",stepsdone+1,"a: Making Growth plots @ ",curtime()))
    mainanimals<-c("Dairy Cattle","Non-Dairy Cattor","Sheep","Swine","Poultry")
    mainmeasures<-c("AD","IEF","POP","AREA","NRATE","FracGASF","FracGASM","FracLEACH")
    for(mm in mainmeasures) {makegrowthplot(secs="3.",meastype=mm)}
    print(paste0("Step ",stepsdone+1,"d: Calculate EU weighted averages"))
    #stop("now write issues")
    #paramcheck$correction[paramcheck$party=="SWE"&paramcheck$meastype=="VSEXC"]<-0
    
    #removing NOR data before to calculate averages
    if(!is.null(keepNORout)){ 
      print("Averages computed keeping OUT Norway")
      allagri_NOR<-allagri[allagri$party == "NOR",]
      allagri<-allagri[allagri$party != "NOR",]
    }
    source("eugirp_euweightedaverages.r")
    export4uba(allagri = allagri)
    
    # Including again Norway data
    if(!is.null(keepNORout)){
      allagri_NOR$autocorr <- ""
      allagri_NOR$correction <- ""
      allagri <- rbind(allagri, allagri_NOR)
      write.table(allagri,file=paste0(csvfil,"_agri.csv"),sep=",")
    }  
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
    
    x1<-942;x2<-nrow(plotmeas)
    x1<-31;x2<-33
    x1<-1;x2<-nrow(plotmeas)
    for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")}
    for(imeas in x1:x2){loopoverplots(imeas = imeas,runfocus = "range",eusubm = "EUC")}
    plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
    write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")
    
    
    stepsdone<-stepsdone+1
    save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
    
}else if(stepsdone>6){
    print(paste0("Step 7: Check for outlier errors already done"))
}

#stop("step 7 done")
# C - Make checks for sector 3 ####
#checksteps<-7
if(stepsdone==7) {
    print(paste0("Step ",stepsdone+1,": Make specific checks for Sector 3 - Set 1"))
    #load(rdatmeasu)
    
    # source("tmp_otherlivestockearlier.r")
    # Again manual correction of BEL-swine/sheep problem
    #     allagri<-allagri[!allagri$variableUID=="8A72A9BE-DB94-4277-ADFC-8AE85BE6B999",]
    #     uidrem<-"76660D32-4F2A-4C65-81C3-71FA2C340542"
    #     uidnew<-"AB1CC8F6-D71C-46A1-A846-B5E76E2DE3A2"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BEL"]<-uidnew
    
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    #     uidrem<-"1FB8F04A-E6A1-47B7-A6F0-407836FDF3EF"
    #     uidnew<-"3FB4B8CB-B0A8-4BA1-8D81-CD412F301D1B"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BEL"]<-uidnew
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    
    #keycategories<-keysources()
    allagri<-allagri[!is.na(allagri$sector_number),]
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option<-""
    allagri<-unique(allagri)
    
    source("agrichecks1ADs.r")
    source("agrichecks2Nex.r")
    source("agrichecks3.r")
    source("eugirp_checklulucf.r")
    
    # Write out list of issues ####
    checks$correction<-1
    checks[,resolved]<-""
    sel<-grepl("agrichecks",checks$val)
    checks$val<-paste0("=HYPERLINK(\"",checks$val,"\")")
     
    #agrichecks<-rbind(checks[names(check1)],check1,check2,check3,check4,check5)
    agrichecks<-checks
    #Load now solved issues!
    agrinames<-c("check","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved)
    
    print("Bind climacheck and agricheck")
    agrichecks<-agrichecks[agrichecks$ms!="all",]
    names(agrichecks)<-agrinames
    #climcheck<-filldf(climcheck,names(agrichecks))
    #climcheck<-climcheck[,names(agrichecks)]
    #agrichecks<-filldf(agrichecks,climcheck)
    #agrichecks<-rbind(agrichecks,climcheck)
    agrichecks$party<-as.character(agrichecks$party)
    #agrinames<-c("test","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved,docfields)
    # Integrate outcome into paramcheck and to writeoutlierlist!!
    x1<-55;x2<-56
    x1<-1;x2<-nrow(agrichecks)
    test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(agrichecks[x,],"agri",x)))
    test<-Reduce(rbind,test)
    agrichecks<-filldf(agrichecks,flag4issues,fillwith = "")
    agrichecks<-convert2char(agrichecks)
    agrichecks[x1:x2,flag4issues]<-test
    #write.csv(agrichecks[allcheckfields4emrt],file=paste0(issuedir,"agrichecks.csv"))
    
    #agrichecks<-addsolved2check(agrichecks,c("recalc","outlier","timeseries","ne_empty","notakey"))
    agrichecks<-addsolved2check(agrichecks,c("recalc","recalculation","outlier","timeseries","ne_empty","notakey"))
    
    #agrichecks<-filldf(agrichecks,allcheckfields)
    
    write.csv(agrichecks,file=paste0(issuedir,"agrichecks",curdate(),".csv"))
    
    stepsdone<-7+1
    savelist<-c(savelist,"checkuids","agrichecks")
    #save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    save(list=savelist,file=rdatallem)
    if(nrow(drive_find(paste0("eealocatorplots"))) == 0) drive_mkdir("eealocatorplots")
    if(!cursubm %in% drive_ls("eealocatorplots/")$name) drive_mkdir(cursubm, "eealocatorplots")
    source("eugirp_files2googleddrive.r")
    if(nrow(drive_find(paste0("eealocator_", cursubm, "_clean.RData"))) == 0){
      for(f2d in files2upload){
        drive_upload(media = f2d, 
                     path = as_dribble(paste0("eealocatorplots/", cursubm, "/")), 
                     #name = NULL, type = NULL, 
                     verbose = FALSE)
      }
    }else{
      for(f2d in files2upload){
        drive_update(file = paste0("eealocatorplots/", cursubm, "/", sub('.*\\/', '', f2d)), 
                     media = f2d, 
                     verbose = TRUE)
      }
    }
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>7){
    print(paste0("Step 7: Sector 3 checks 1 already done"))
}

#stop("Step 8 done")
if(stepsdone==8) {
  #nor dentro
    print(paste0("Step ",stepsdone+1,": Comparison with FAO"))
    source("eugirp_faocomparison.r")
    source("eugirp_exportUIDs4capri.r")
    stepsdone<-9
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    save(list=savelist,file=rdatallem)
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>8){
    print(paste0("Step 7: Comparison with FAO already done"))
}
stop("Step 9 done")




# UNCERTAINTY ####
source("eugirp_uncertainty.r")

# A.3 Determine Key categories ####
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


