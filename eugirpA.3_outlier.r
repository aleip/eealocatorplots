library(reshape2) #required for melt function - for outls
source("eugirp_writeissues.r")
restrictdata<-function(growthdata,measures){
    growthmeas<-unique(subset(growthdata,select=allfields[!allfields %in% c("notation","party",years)]))
    growthmeas<-growthmeas[growthmeas$meastype %in% measures,]
    
    # Criterion 1: Do not growth without sector_number
    growthselect <- growthmeas$sector_number!=""
    growthmeas<-growthmeas[growthselect,]
    
    # Criterion 2: Do not growth sector_numbers 
    #      Exceptions: do all growths in Sector 3
    growthselect<-unlist(lapply(c(1:nrow(growthmeas)),function(x) 
        if(grepl("^3",growthmeas$sector_number[x])){TRUE}else{
            gcCount(growthmeas$sector_number[x],".")<3}))
    growthmeas<-growthmeas[growthselect,]
    
    # Criterion 3: growth only sector 3
    growthselect <- grepl("^3",growthmeas$sector_number)
    growthmeas<-growthmeas[growthselect,]
    
    
    if(restrictsector!=""){
        select <- grepl(restrictsector,growthmeas$sector_number)
        growthmeas<-growthmeas[select,]
    }
    if(restrictcategory!=""){
        select <- grepl(restrictcategory,growthmeas$category)
        growthmeas<-growthmeas[select,]
    }
    return(growthmeas)
}
selquantiles<-function(D){
    #View(D)
    quantls<-c(0.25, 0.5, 0.75)
    Dnozero<-matbyind(D = D,v = which(D!=0,arr.ind = TRUE))
    mi<-min(Dnozero)
    mx<-max(Dnozero)
    q<-(as.vector(quantile(Dnozero,probs=quantls,na.rm=TRUE)))
    z<-c(mi,q,mx)
    return(z)
}
selmeansd<-function(D){
    #View(D)
    quantls<-c(0.5)
    Dnozero<-matbyind(D = D,v = which(D!=0,arr.ind = TRUE))
    mi<-min(Dnozero)
    mx<-max(Dnozero)
    mean<-mean(Dnozero)
    std<-sd(Dnozero)
    q<-(as.vector(quantile(Dnozero,probs=quantls,na.rm=TRUE)))
    z<-c(mean,std)
    return(z)
}

# Growthdata and paramdata initialization ####
noemissions<-c(meas2popweight,meas2clima,meas2mcf,meas2sum[!meas2sum%in%"EM"])
growthdata<-allgrowth
growthmeas<-restrictdata(growthdata,noemissions)
growthdata<-growthdata[growthdata$variableUID %in% growthmeas$variableUID,]
growthdata<-growthdata[growthdata$party!="EU28",]
growthdata<-growthdata[order(growthdata$sector_number,growthdata$category),]
growthmeas<-growthmeas[order(growthmeas$sector_number,growthmeas$category),]

parammeasures<-c(meas2popweight,meas2mcf,"POP")
paramdata<-allagri
parammeas<-restrictdata(paramdata,parammeasures)
parammeas<-parammeas[parammeas$measure!="Nitrogen excretion per MMS",]
parammeas<-parammeas[order(parammeas$sector_number,parammeas$category),]
paramdata<-paramdata[paramdata$variableUID %in% parammeas$variableUID,]
paramdata<-paramdata[paramdata$party!="EU28",]
paramdata<-paramdata[order(paramdata$sector_number,paramdata$category),]

# Autocorrections - detected errors in units etc. ####
#empty data.frame http://stackoverflow.com/questions/10689055/create-an-empty-data-frame
autocorrections<-data.frame(Characters=character(),Characters=character(),Ints=integer())
names(autocorrections)<-c("variableUID","party","autocorr")

# DIGESTIBILITY (Table 3As2) is in % - multiply values < 1 with 100
mult<-100
v<-which(paramdata$meastype=="DIGEST" & paramdata[years]<1 & paramdata[years]!=0,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"DIGEST"
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

# FRACBURN can must have data below 1 (unit is fraction not percent)
# ... but if it is there is no 'outlier' that can be identified ... therefore remove from list
v<-which(paramdata$meastype=="FracBURN" & paramdata[years]>=1,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"FracBURN"
mult<-0.01
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)
v<-which(paramdata$meastype=="FracBURN" & paramdata[years]<1 & paramdata[years]>=0,arr.ind = TRUE)
if(nrow(v)>0) {
    v<-unique(v[,1])
    paramdata<-paramdata[!c(1:nrow(paramdata))%in%v,]}

# DIRECT N2O EMISSIONS - IEF FRACTION NOT PERCENT (DIVIDE BY 100)
v<-which((grepl("3.D.1.[14]",paramdata$sector_number) | grepl("3.D.2.[12]",paramdata$sector_number)) & 
             paramdata$meastype=="IEF" & paramdata[years]>=1,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"N2OIEF"
mult<-0.01
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

# VEXC - VALUES MUST BE REPORTED IN (kg dm/head/day)
#        VALUES >>100 ARE LIKELY IN (kg dm/head/YEAR) - (DIVIDE BY 365)
v<-which(paramdata$meastype=="VEXC" & paramdata[years]>100,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"DIGEST"
mult<-1/365
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

# YM - VALUE REPORTED 10 TIMES TOO HIGH (OCCURS FOR GR)
v<-which((grepl("3.A.1",paramdata$sector_number) ) & 
             paramdata$meastype=="YM" & paramdata[years]>=10,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"YM"
mult<-0.1
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

# MCF - VALUE REPORTED AS FRACTION INSTEAD OF PERCENT (MULTIPLY WITH 100)
v<-which( (paramdata$meastype=="MCF" & paramdata$source!="Daily spread" & paramdata[years]==0.1) |
              (paramdata$meastype=="MCF" & paramdata[years]<0.08 & paramdata[years]!=0)
              ,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"MCF"
mult<-100
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

# GE - VALUES ARE REPORTED IN ABSOLUTE VALUES (DIVIDE BY POPULATION DATA)
v<-which(paramdata$meastype=="GE" & paramdata[years]>10000,arr.ind = TRUE)
c<-paste(unique(paramdata$party[v[,1]]),collapse="-")
name<-"GE"
mult<-"POP"
filnam<-paste0("corrections_",name,"_",c,".csv")
autocorrections<-writeautoc(v,autocorrections,paramdata,mult,filnam)
paramdata<-writecorrection(v,paramdata,mult,name)

autocorrections<-merge(autocorrections,paramdata,by=c("variableUID","party"),all=FALSE)
autocorrections[,docfields]<-""
tn<-names(autocorrections)
co<-c("sector_number","category","party","meastype","autocorr",docfields,"file")
co<-c(co,tn[!tn%in%co])
ro<-order(autocorrections$party,autocorrections$sector_number,autocorrections$meastype)
autocorrections<-autocorrections[ro,co]


filnamt<-paste0(issuedir,"/autocorrections/corrections",cursubm,".csv")
con <- file(filnamt, open="wt")
writeLines(docflags,con)
if(nrow(autocorrections)>0){
    writeLines("#\n# Issues on parameters which were corrected for calcultating EU-average values",con)
    write.csv(autocorrections,con)
}
close(con)

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
        n<-c("party","sector_number","meastype","unit","category","value","correction","variable",years,"gas",
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
        Dmeddevtest<-(((D[years]>D$uwhisk) | (D[years]<D$lwhisk)) & (abs(1-D[years]/D[,"p50"])>0.5)) 
        n1<-c("party","sector_number","meastype","category","min","lwhisk","p25","p50","p75",
              "uwhisk","max","range","value","variable")
        
    } else if (outlmethod==3){
        # Method 2 in outl tool
        # Obtaining 95% confidence intervall 
        bxplf <- 1.5
        D$range<-D[,"max"]/D[,"min"]
        D$range[is.nan(D$range)]<-0
        D$llim<-D[,"mean"]-bxplf*D[,"std"]
        D$ulim<-D[,"mean"]+bxplf*D[,"std"]
        
        #               Keep values which are outside of the 95% confidence intervall
        #               Do NOT keep values which are very close to the median (rounding errors)
        Dmeddevtest<-(((D[years]>D$ulim) | (D[years]<D$llim)) & ((abs(1-D[years]/D[,"median"])>0.5)))
        
        if(type=="param") n1<-c("party","sector_number","meastype","value","rellim","range","correction",resolved,docfields,"variable",
              "llim","ulim","unit","category","min","mean","p25","median","p75","max")
        if(type=="growth") n1<-c("party","sector_number","meastype","value","range","correction","variable",
              "llim","ulim","unit","category","min","mean","p25","median","p75","max")
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


if(nrow(growthcheck)>0){
    
    filnam<-paste0(invloc,"/checks/timeseries/checks",cursubm,"growthcheck.csv")
    con <- file(filnam, open="wt")
    writeLines(paste0("# File created by EU-GIRP v.4 on ",figdate), con)
    writeLines(paste0("# List of trend outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Growth Rate outside of the '95% confidence interval'. I.e. the growth rate was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the growth rates during the time period for this variable and country"), con)
    writeLines(paste0("# (2) Growth Rate larger than ", 1+ming," or smaller than ",1-ming), con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    writeLines(paste0("# years.growth: growth rates calculated as y{t}/y{t-1}"), con)
    write.csv(growthcheck,con)
    close(con)
}
if(nrow(paramcheck)>0){
    #Check of systematic errors
    
    filnam<-paste0(invloc,"/checks/countryoutliers/checks",cursubm,"countryoutliersserious.csv")
    con <- file(filnam, open="wt")
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)writeLines(outlmethod2,con)
    if(trendoutlmethod==3)writeLines(outlmethod3,con)
    writeLines(coutlexp2, con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    write.csv(paramV,con)
    close(con)
    
    # Generate the plots to illustrate the issues
    plotparamcheck<-1
    fignames<-character()
    source("eugirpD.2_iefplots.r")
    fignamesc<-gsub(paste0(issuedir,"countryoutliers/plots/"),"=HYPERLINK(\"",fignames)
    paramcheck$plot<-paste0(fignamesc,"\")")
    n<-names(paramcheck[!names(paramcheck)%in%"plot"])
    oc<-c(n[1:(which(n=="issuedate"))],"plot",n[(which(n=="issuedate")+1):length(n)])
    paramcheck<-paramcheck[,oc%in%names(paramcheck)]
    #n1<-c("cursubm","party","sector_number","meastype","unit","category","value","correction"))
    #n4<-names(paramcheck[!names(paramcheck)%in%c(n1,"std")])
    #paramcheck<-paramcheck[,oc]
    con <- file(filoutliers, open="wt")
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)writeLines(outlmethod2,con)
    if(trendoutlmethod==3)writeLines(outlmethod3,con)
    writeLines(coutlexp2, con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    writeLines("#\n#Note for column: correction: 0: value assumed to be a mistake. it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification. 1: value is assumed to be not an outlier despite the criteria (e.g. milk production). empty: to be clarified",con)
    write.csv(paramcheck,con)
    close(con)
    
    
}

