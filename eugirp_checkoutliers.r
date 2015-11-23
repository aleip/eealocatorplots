# EXCLUDE OTHER LIVESTOCK FROM CHECKS - INHOMOGENEOUS COMPOSITION
paramdata<-paramdata[! paramdata$category=="Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Other Livestock",]
#  ---> eliminate population, not needed any more
paramdata<-paramdata[! paramdata$meastype=="POP",]

# 'SERIOUS' ERRORS, LIKLY DUE TO COMMA-PROBLEM
growth<-growthdata[,c(years,"party","variableUID")]
param<-paramdata[,c(years,"party","variableUID")]
paramV<-paramdata[unique(which(paramdata[years]>1000,arr.ind = TRUE)[,1]),]

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
        bxplf <- 0.953
        
        D$range<-D[,"max"]/D[,"min"]
        D$range[is.nan(D$range)]<-0
        D$uwhisk<-D[,"p50"]+(1 + bxplf)*(D[,"p75"]-D[,"p50"])
        D$lwhisk<-D[,"p50"]+(1 + bxplf)*(D[,"p25"]-D[,"p50"])
        D<-merge(D,D,by="variableUID")
        
        #               Keep values which are outside of the 95% confidence intervall
        #               Do NOT keep values which are very close to the median (rounding errors)
        Dmeddevtest<-(((D[years]>D$uwhisk) | (D[years]<D$lwhisk)) & (abs(1-D[years]/D[,"p50"])>0.1)) 
        n1<-c("party","sector_number","meastype","category","min","lwhisk","p25","p50","p75",
              "uwhisk","max","range","value","variable")
        
    } else if (outlmethod==3){
        # Method 3 in outl tool
        # Obtaining 95% confidence intervall 
        bxplf <- 1.5
        D$range<-D[,"max"]/D[,"min"]
        D$range[is.nan(D$range)]<-0
        D$llim<-D[,"mean"]-bxplf*D[,"std"]
        D$ulim<-D[,"mean"]+bxplf*D[,"std"]
        
        #               Keep values which are outside of the 95% confidence intervall
        #               Do NOT keep values which are very close to the median (rounding errors)
        Dmeddevtest<-(((D[years]>D$ulim) | (D[years]<D$llim)) & ((abs(1-D[years]/D[,"median"])>0.1)))
        
        if(type=="param") n1<-c("party","sector_number","category","meastype","unit","value","rellim","range","correction",resolved,docfields,"variable",
              "llim","ulim","min","mean","p25","median","p75","max")
        if(type=="growth") n1<-c("party","sector_number","category","meastype","unit","value","range","correction","variable",
              "llim","ulim","min","mean","p25","median","p75","max")
    }
    
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
        Dcheck[,docfields]<-""
        Dcheck$rellim<-unlist(lapply(c(1:nrow(Dcheck)),function(x)
            if(Dcheck$value[x]>Dcheck$ulim[x]){round(Dcheck$value[x]/Dcheck$ulim[x],2)}else
                if(Dcheck$value[x]<Dcheck$llim[x]){round(Dcheck$value[x]/Dcheck$llim[x],2)}else
                {1}))
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

# Calculate statistical moments (call functions) ####
newcols<-c("min","p25","median","p75","max")
growthquantiles<-subset(growth,select=c("variableUID","party"))
paramq<-as.data.frame(unique(param$variableUID))
names(paramq)<-"variableUID"
growth[,c("mean","std")]<-t(Reduce(cbind,lapply(c(1:nrow(growth)),function(x) selmeansd(growth[x,years]))))
growth[,newcols]<-t(Reduce(cbind,lapply(c(1:nrow(growth)),function(x) selquantiles(growth[x,years]))))
paramq$mad<-unlist(lapply(paramq$variableUID,function(x) mad(param[param$variableUID==x,years],na.rm=TRUE)))
paramq[,c("mean","std")]<-t(Reduce(cbind,lapply(paramq$variableUID,function(x) selmeansd(param[param$variableUID==x,years]))))
paramq[,newcols]<-t(Reduce(cbind,lapply(paramq$variableUID,function(x) selquantiles(param[param$variableUID==x,years]))))
param<-merge(param,paramq,by="variableUID")

# Calculate outliers (method) ####
paramcheck<-identifyoutliers(trendoutlmethod,param,"param")
paramcheck$correction<-unlist(lapply(c(1:nrow(paramcheck)),function(x)
    serious(paramV,paramcheck$variableUID[x],paramcheck$party[x])))

# Selection of issues if there are too many!!
# Manure management IEFs vary largely because of different MMS --> a highly skewed distribution is OK
ok1<-grepl("^3.B.",paramcheck$sector_number) & (paramcheck$rellim>0.5 & paramcheck$rellim<2.0)
paramcheck<-paramcheck[!ok1,]

#Allow a margin of 10% for the other categories
ok1<-(paramcheck$rellim>0.9 & paramcheck$rellim<1.1)
paramcheck<-paramcheck[!ok1,]
paramR<-paramcheck[paramcheck$range>maxr,]

ming<-0.03
growthcheck<-identifyoutliers(trendoutlmethod,growth,"growth")


