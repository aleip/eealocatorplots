
# Save quantiles for growth rate
# Store 'bias' in distibution of parameters: ratio of max/min (zeros excluded)
#        -- If there is systematic mistakes in errors the bias is high

identifyoutliers<-function(outlmethod,D,type){
    if(outlmethod==1){
        # Method 1 in outl tool: Deviation from median
        # Median absolute deviation of the absolute deviations from the median
        Dmeddevtest<-abs(trend-trenD[2,])>2*trendmadn
        
        n1<-c("party","sector_number","meastype","category","mad","value","variable")
        n<-c("party","sector_number","category","meastype","unit","value","correction","variable",years,"gas",
             "mad","method","classification","source","target","type","option","measure","variableUID")
    } else if (outlmethod==2){
        # Method 2 in outl tool
        
        D$range<-D[,"max"]/D[,"min"]
        D$range[is.nan(D$range)]<-0
        D$ulim<-D[,"median"]+(1 + bxplf)*(D[,"p75"]-D[,"median"])
        D$llim<-D[,"median"]+(1 + bxplf)*(D[,"p25"]-D[,"median"])
        #D<-merge(D,D,by="variableUID")
        
        #               Keep values which are outside of the 95% confidence intervall
        #               Do NOT keep values which are very close to the median (rounding errors)
        Dmeddevtest<-(((D[years]>D$ulim) | (D[years]<D$llim)) & (abs(1-D[years]/D[,"median"])>0.1)) 
        
    } else if (outlmethod==3){
        # Method 3 in outl tool
        # Obtaining 95% confidence intervall 
        D$range<-D[,"max"]/D[,"min"]
        D$range[is.nan(D$range)]<-0
        D$llim<-D[,"mean"]-bxplf*D[,"std"]
        D$ulim<-D[,"mean"]+bxplf*D[,"std"]
        
        # Keep values which are outside of the 95% confidence intervall
        # Do NOT keep values which are very close to the median (rounding errors)
        Dmeddevtest<-(((D[years]>D$ulim) | (D[years]<D$llim)) & ((abs(1-D[years]/D[,"median"])>0.1)))
        
    }
    #if(type=="param") n1<-c("party","sector_number","category","meastype","unit","value","rellim","range","correction",resolved,docfields,"variable",
    if(type=="param") n1<-c("party","sector_number","category","meastype","unit","value","rellim","range","correction",resolved,"variable",
                            "llim","ulim","min","mean","p25","median","p75","max")
    if(type=="growth") n1<-c("party","sector_number","category","meastype","unit","value","range","correction","variable",
                             "llim","ulim","min","mean","p25","median","p75","max")
    Doutl<-Dmeddevtest[,years]*D[,years]
    nDoutl<-sum(Doutl!=0,na.rm=TRUE)
    listofoutls<-which(unlist(Doutl)!=0)
    Doutl$party<-D$party
    Doutl$variableUID<-D$variableUID
    
    Doutllist<-melt(Doutl,id.vars=c("party","variableUID"),na.rm=F)
    select<-! is.na(Doutllist$value)
    Doutllist<-Doutllist[! is.na(Doutllist$value),]
    Doutllist<-Doutllist[Doutllist$value !=0,]
    
    if(type=="growth") {
        ming<-0.03
        select<-(Doutllist$value<(1-ming) | Doutllist$value>(1+ming))
        Doutllist<-Doutllist[select,]
    }
    
    Doutlvalues<-subset(Doutllist,select=c(value,party,variableUID))
    Doutllist<-subset(Doutllist,select=-value)
    
    Dcheck<-simplifytestmatrix(check = Doutllist,group = "variable",compare = years)
    Dcheckv<-aggregate(value ~ party + variableUID,data =Doutlvalues,mean)
    if(type=="growth"){
        x<-simplifytestmatrix(check = Doutlvalues, group = "value")
        Dcheck<-merge(Dcheck,x,by=c("variableUID","party"))
        Dcheck<-merge(Dcheck,allagri[c(sectfields,measfields,metafields,"variableUID","party",years)],by=c("variableUID","party"),all=FALSE)
    } else if (type=="param"){
        Dcheck<-merge(Dcheck,Dcheckv,by=c("variableUID","party"))
        Dcheck<-merge(Dcheck,allagri[c(sectfields,measfields,metafields,"variableUID","party")],by=c("variableUID","party"),all=FALSE)
        
    }
    Dcheck<-merge(Dcheck,D,by=c("variableUID","party"))
    Dcheck<-cbind(cursubm,Dcheck,row.names=NULL)                 
    Dcheck$correction<-""
    if(type=="param"){
        Dcheck[,resolved]<-""
        #Dcheck[,docfields]<-""
        Dcheck$rellim<-unlist(lapply(c(1:nrow(Dcheck)),function(x)
            if(Dcheck$value[x]>Dcheck$ulim[x]){
                round((Dcheck$value[x]-Dcheck$median[x])/(Dcheck$ulim[x]-Dcheck$median[x]),2)
                }else if(Dcheck$value[x]<Dcheck$llim[x]){
                    round((Dcheck$value[x]-Dcheck$median[x])/(Dcheck$llim[x]-Dcheck$median[x]),2)
                    }else{1}))
    }
    n3<-c("gas","method","classification","source","target","type","option","measure","variableUID","cursubm")
    
    
    if(type=="growth"){
        n<-c(n1,paste0(years,".x"),paste0(years,".y"),n3)
        o<-order(Dcheck$party,Dcheck$sector_number,Dcheck$category)
    } else if (type=="param"){
        n<-c(n1,years,n3)
        o<-order(Dcheck$meastype,Dcheck$sector_number,Dcheck$category,Dcheck$party)
    }
    Dcheck<-Dcheck[o,n]
    
    n[which(n=="variable")]<-"years"
    names(Dcheck)<-n
    
    return(Dcheck)
    
}
serious<-function(P,uid,c){
    if(uid %in% P$variableUID){if(c %in% P$party[P$variableUID==uid]){s<-"0"}else{s<-""}
    }else{s<-""}
    return(s)
}

outlierintimeseries<-function(paramcheckfew,x,conv=0){
    
    #convert if this is 'except' years
    if(conv==1){
        cury<-gsub("all except: ","",paramcheckfew$years[x])
        cury<-as.numeric(unlist(strsplit(cury," ")))
        cury<-yearsnum[!yearsnum %in% cury]
    }else{     
        cury<-as.numeric(unlist(strsplit(paramcheckfew$years[x]," ")))
    }
    curymin<-min(cury)
    curymax<-max(cury)
    curt1<-NA;curt2<-NA
    if(curymin>1990){curt1<-abs(1-paramcheckfew[x,as.character(curymin)]/paramcheckfew[x,as.character(curymin-1)])}
    if(curymax<(invyear-2)){curt2<-abs(1-paramcheckfew[x,as.character(curymax)]/paramcheckfew[x,as.character(curymax+1)])}
    curok<-mean(c(curt1,curt2),na.rm=TRUE)<0.15
    curok[is.na(curok)]<-FALSE
    return(curok)
}



# Calculate statistical moments (call functions) ####
newcols<-c("min","p25","median","p75","max")
if (! file.exists("../2016/checks/timeseries")){dir.create(file.path("../2016/checks/timeseries/"))}
if (! file.exists("../2016/checks/timeseries/checks")){dir.create(file.path("../2016/checks/timeseries/checks/"))}
if (! file.exists("../2016/checks/countryoutliers")){dir.create(file.path("../2016/checks/countryoutliers/"))}
if (! file.exists("../2016/checks/countryoutliers/checks")){dir.create(file.path("../2016/checks/countryoutliers/checks/"))}


# Calculate outliers (method) ####
if(outcheck=="param"){
    # 'SERIOUS' ERRORS, LIKLY DUE TO COMMA-PROBLEM
    paramV<-param[unique(which(param[years]>1000,arr.ind = TRUE)[,1]),]
    
    paramcheck<-identifyoutliers(trendoutlmethod,param,"param")
    #View(paramcheck)
    paramcheck$correction<-unlist(lapply(c(1:nrow(paramcheck)),function(x)
        serious(paramV,paramcheck$variableUID[x],paramcheck$party[x])))
    ## Current manual changes to paramcheck
    paramcheck$correction[paramcheck$party=="BG"&paramcheck$category=="Goats"]<-0
    
    # Selection of issues if there are too many!!
    # Manure management IEFs vary largely because of different MMS --> a highly skewed distribution is OK
    ok1<-grepl("^3.B.",paramcheck$sector_number) & (paramcheck$rellim>0.5 & paramcheck$rellim<2.0)
    #Relax upper and lower limits by 205
    ok1<-grepl("^3.B.",paramcheck$sector_number) & (paramcheck$rellim>0.5 & paramcheck$rellim<1.2)
    paramcheck$correction[ok1]<-"rellim"
    
    #Allow a margin of 10% for the other categories
    ok1<-(paramcheck$rellim>0.9 & paramcheck$rellim<1.1)
    paramcheck$correction[ok1]<-"rellim"
    paramR<-paramcheck[paramcheck$range>maxr,]

    # Cattle is an aggregate ... don't consider as outlier
    paramcheck$correction[paramcheck$category=="Cattle"]<-"cattle"
    
    # Outliers for only a few years ... if they are part of a trend do not consider them as
    # 'country-outlier' ... they might be identified as trend-outliers
    # Assume that those which are within 15% of their 'neighbour' point are OK
    
    paramcheckfew<-paramcheck[! grepl("all",paramcheck$years),]
    paramcheckfewok<-unlist(lapply(c(1:nrow(paramcheckfew)),function(x) outlierintimeseries(paramcheckfew,x)))    
    paramcheckfew$correction[paramcheckfewok]<-"trend"
    
    paramcheck<-paramcheck[grepl("all",paramcheck$years),]
    paramcheck<-rbind(paramcheck,paramcheckfew)
    
    paramcheckfew<-paramcheck[grepl("except",paramcheck$years),]
    paramcheckfewok<-unlist(lapply(c(1:nrow(paramcheckfew)),function(x) outlierintimeseries(paramcheckfew,x,1)))    
    paramcheckfew$correction[paramcheckfewok]<-"trend"
    
    paramcheck<-paramcheck[!grepl("except",paramcheck$years),]
    paramcheck<-rbind(paramcheck,paramcheckfew)
    

}
if(outcheck=="growth"){
    ming<-0.03
    growthcheck<-identifyoutliers(trendoutlmethod,growth,"growth")
    
    
    
}
