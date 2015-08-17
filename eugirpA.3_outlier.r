trendoutlmethod<-2

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
growthdata<-allgrowth
paramdata<-alldata

noemissions<-c(meas2popweight,meas2clima,meas2mcf,meas2sum[!meas2sum%in%"EM"])
parammeasures<-c(meas2popweight,meas2mcf)
growthmeas<-restrictdata(growthdata,noemissions)
parammeas<-restrictdata(paramdata,parammeasures)
parammeas<-parammeas[parammeas$measure!="Nitrogen excretion per MMS",]
#trendmeas<-restrictdata(trenddatac)

growthdata<-growthdata[growthdata$variableUID %in% growthmeas$variableUID,]
growthdata<-growthdata[growthdata$party!="EU28",]
growthdata<-growthdata[order(growthdata$sector_number,growthdata$category),]
growthmeas<-growthmeas[order(growthmeas$sector_number,growthmeas$category),]
paramdata<-paramdata[paramdata$variableUID %in% parammeas$variableUID,]
paramdata<-paramdata[paramdata$party!="EU28",]
paramdata<-paramdata[order(paramdata$sector_number,paramdata$category),]
parammeas<-parammeas[order(parammeas$sector_number,parammeas$category),]
#trenddata<-trenddata[trenddata$variableUID %in% trendmeas$variableUID,]
#trenddata<-trenddata[order(trenddata$sector_number,trenddata$category),]

quantls<-c(0.25, 0.5, 0.75)
maxr<-5
growth<-growthdata[,c(years,"party")]
param<-paramdata[,c(years,"party","variableUID")]
paramquantiles<-as.data.frame(unique(param$variableUID))
names(paramquantiles)<-"variableUID"
paramquantiles[,"25%"]<-0
paramquantiles[,"50%"]<-0
paramquantiles[,"75%"]<-0
#trend<-trenddata[,c(years,"party")]
# Save quantiles for growth rate
growthquantiles<-t(apply(growth[years], 1, quantile, probs = quantls,  na.rm = TRUE))
xyz<-(lapply(paramquantiles$variableUID,function(x) 
    as.vector(t(quantile(param[param$variableUID==x,years],probs=quantls,na.rm=TRUE)))
    ))
yz<-Reduce(rbind,xyz)
paramquantiles[,c("25%","50%","75%")]<-yz
# Store 'bias' in distibution: ratio of median/25-quartile and 75-quartile/median
#        -- If there is systematic mistakes in errors the bias is high
paramquantiles$R1<-paramquantiles[,"50%"]/paramquantiles[,"25%"]
paramquantiles$R2<-paramquantiles[,"75%"]/paramquantiles[,"50%"]
#trendquantiles<-t(apply(trend[years], 1, quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE))

# Median absolute deviation of the absolute deviations from the median
growthmad<-apply(growth[years], 1, mad,  na.rm = TRUE)
paramquantiles$mad<-unlist(lapply(paramquantiles$variableUID,function(x) 
    mad(param[param$variableUID==x,years],na.rm=TRUE)))
#trendmad<-apply(trend[years], 1, mad,  na.rm = TRUE)

#print("# Mulitply with factor according to outl Tool")
#growthmadn<-growthmad/0.6745

if(trendoutlmethod==1){
    # Method 1 in outl tool: Deviation from median
    growthmeddevtest<-abs(growth-growthquantiles[2,])>2*growthmadn
    meddevtest<-abs(trend-trendquantiles[2,])>2*trendmadn
    
} else if (trendoutlmethod==2){
    # Method 2 in outl tool
    # Obtaining 95% confidence intervall 
    bxplf <- 0.953
    #uwhisk = mdian + (1 + bxplf) * (uquart - mdian)
    #lwhisk = mdian + (1 + bxplf) * (lquart - mdian)
    growthuwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,3]-growthquantiles[,2])
    growthlwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,1]-growthquantiles[,2])
    growthmeddevtest<-(growth[years]>growthuwhisk) | (growth[years]<growthlwhisk)
    
    paramquantiles$uwhisk<-paramquantiles[,"50%"]+(1 + bxplf)*(paramquantiles[,"75%"]-paramquantiles[,"50%"])
    paramquantiles$lwhisk<-paramquantiles[,"50%"]+(1 + bxplf)*(paramquantiles[,"25%"]-paramquantiles[,"50%"])
    param<-merge(param,paramquantiles,by="variableUID")

    #               Keep values which are outside of the 95% confidence intervall
    parammeddevtest<-(((param[years]>param$uwhisk) | (param[years]<param$lwhisk)) &
        #               Do NOT keep values which are very close to the median (rounding errors)
        (abs(1-param[years]/param[,"50%"])>1.1)) | 
        #               Keep values with a very large distribution
        (param$R1>maxr | param$R2>maxr )
    
    #uwhisk<-trendquantiles[,2]+(1 + bxplf)*(trendquantiles[,3]-trendquantiles[,2])
    #lwhisk<-trendquantiles[,2]+(1 + bxplf)*(trendquantiles[,1]-trendquantiles[,2])
    #meddevtest<-(trend[years]>uwhisk) | (trend[years]<lwhisk)
}

growthoutl<-growthmeddevtest*growth[years]
ngrowthoutl<-sum(growthoutl!=0,na.rm=TRUE)
paramoutl<-parammeddevtest[,years]*param[,years]
nparamoutl<-sum(paramoutl!=0,na.rm=TRUE)
#trendoutl<-meddevtest*trend[years]
#ntrendoutl<-sum(trendoutl!=0)
#print("#unlist(growthoutl)")

if(ngrowthoutl>0){
    #listofoutls<-which(unlist(subset(growthoutl,select=-country))!=0)
    listofoutls<-which(unlist(growthoutl)!=0)
    growthoutl$party<-growth$party
    growthoutl$rown<-row.names(growthoutl)
    
    growthoutllist<-melt(growthoutl,id.vars=c("party","rown"),na.rm=F)
    select<-! is.na(growthoutllist$value)
    growthoutllist<-growthoutllist[! is.na(growthoutllist$value),]
    growthoutllist<-growthoutllist[growthoutllist$value !=0,]
    growthoutllist<-merge(growthoutllist,growthmeas,by.x="rown",by.y="row.names")
    growthoutllist<-merge(growthoutllist,growthquantiles,by.x="rown",by.y="row.names")
    
    #Restrict results to >2% growth rate
    ming<-0.03
    select<-(growthoutllist$value<(1-ming) | growthoutllist$value>(1+ming))
    growthoutllist<-growthoutllist[select,]
    
    growthoutlvalues<-subset(growthoutllist,select=c(rown,value,party,variableUID))
    growthoutllist<-subset(growthoutllist,select=-value)
    
    growthcheck<-simplifytestmatrix(check = growthoutllist,group = "variable",compare = years)
    growthcheckv<-aggregate(value ~ party + variableUID + rown,data =growthoutlvalues,mean)
    x<-simplifytestmatrix(check = growthoutlvalues, group = "value")
    x<-subset(x,select=c("rown","value"))
    growthcheck<-merge(growthcheck,x,by=c("rown"))
    growthcheck<-merge(growthcheck,alldata[years],by.x="rown",by.y="row.names")
    growthcheck<-merge(growthcheck,subset(growth,select=-party),by.x="rown",by.y="row.names")
    
    growthcheck<-cbind(cursubm,growthcheck,row.names=NULL)                 
    o<-order(growthcheck$party,growthcheck$sector_number,growthcheck$category)
    n1<-c("party","sector_number","meastype","category","25%","50%","75%","value","variable")
    n3<-c("gas","unit","method","classification","source","target","type","option","measure",
          "variableUID","cursubm","rown")
    n<-c(n1,paste0(years,".x"),paste0(years,".y"),n3)
    growthcheck<-growthcheck[o,n]
    names(growthcheck)<-c(n1,years,paste0(years,".growth"),n3)
    con <- file(paste0(csvfil,"_growthcheck.csv"), open="wt")
    writeLines(paste0("# File created by EU-GIRP v.4 on ",figdate), con)
    writeLines(paste0("# List of trend outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Growth Rate outside of the '95% confidence interval'. I.e. the growth rate was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the growth rates during the time period for this variable and country"), con)
    writeLines(paste0("# (2) Growth Rate larger than ", 1+ming," or smaller than ",1-ming), con)
    writeLines(paste0("# Explanation of column names (different from CRF-dimensions)"), con)
    writeLines(paste0("# 25%-50%-75%: 25percentile-50percentile (median)-75percentile of the distribution of growth rates for the variable in the country over time period",years[1],"-",years[length(years)]), con)
    writeLines(paste0("# value: list of outlier values (growth rates) identified-space seprated "), con)
    writeLines(paste0("# variable: list of years for which outlier values (growth rates) were identified"), con)
    writeLines(paste0("# years: Values reported"), con)
    writeLines(paste0("# years.growth: growth rates calculated as y{t}/y{t-1}"), con)
    write.csv(growthcheck,con)
    close(con)
}
if(nparamoutl>0){
    #listofoutls<-which(unlist(subset(growthoutl,select=-country))!=0)
    listofoutls<-which(unlist(paramoutl)!=0)
    paramoutl$party<-param$party
    paramoutl$variableUID<-param$variableUID
    
    paramoutllist<-melt(paramoutl,id.vars=c("party","variableUID"),na.rm=F)
    select<-! is.na(paramoutllist$value)
    paramoutllist<-paramoutllist[! is.na(paramoutllist$value),]
    paramoutllist<-paramoutllist[paramoutllist$value !=0,]
    paramoutllist<-merge(paramoutllist,parammeas,by="variableUID")
    #paramoutllist<-merge(paramoutllist,param,by="variableUID")
    
    paramoutlvalues<-subset(paramoutllist,select=c(value,party,variableUID))
    paramoutllist<-subset(paramoutllist,select=-value)
    
    paramcheck<-simplifytestmatrix(check = paramoutllist,group = "variable",compare = years)
    paramcheckv<-aggregate(value ~ party + variableUID,data =paramoutlvalues,mean)
    
    paramcheck<-merge(paramcheck,paramcheckv,by=c("variableUID","party"))
    paramcheck<-merge(paramcheck,param,by=c("variableUID","party"))

    
    o<-order(paramcheck$sector_number,paramcheck$meastype,paramcheck$category,paramcheck$party)
    n<-c("party","sector_number","meastype","category","lwhisk","25%","50%","75%","uwhisk",
         "R1","R2","value","variable",years,"gas","unit",
         "mad","method","classification","source","target","type","option","measure","variableUID")
    paramcheck<-paramcheck[o,n]
    paramcheck<-cbind(cursubm,paramcheck,row.names=NULL)    
    
    paramcheck$R1[is.infinite(paramcheck$R1)]<-0
    paramcheck$R1[is.nan(paramcheck$R1)]<-0
    paramcheck$R2[is.infinite(paramcheck$R2)]<-0
    paramcheck$R2[is.nan(paramcheck$R2)]<-0
    
    #Check of systematic errors
    paramR<-paramcheck[paramcheck$R1>maxr | paramcheck$R2>maxr ,]
    #paramcheck<-paramcheck[! (paramcheck$R1>maxr | paramcheck$R2>maxr),]
    
    write.csv(paramR,file=paste0(csvfil,"_countryoutliersserious.csv"))
    con <- file(paste0(csvfil,"_countryoutliersserious.csv"), open="wt")
    writeLines(paste0("# File created by EU-GIRP v.4 on ",figdate), con)
    writeLines(paste0("# List of country outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Country value outside of the '95% confidence interval'. I.e. the value was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the values reported for the variable during the time period and by all countries by which this variable is reported"), con)
    writeLines(paste0("# (2) To exclude minimim deviations though, the country values which are not more different that 10% from the median are NOT considered as outliers"), con)
    writeLines(paste0("# (3) A wide distribution indicates systematic errors (e.g. wrong unit used by MS(s)); therefore values with a ratio of 75perc/50perc or 50perc/25perc of more than ",maxr," are considered as outliers"), con)
    writeLines(paste0("# Explanation of column names (different from CRF-dimensions)"), con)
    writeLines(paste0("# lwhisk - uwhisk: lower and upper 'whisker': all values below or above this whiskers are outside of the 95% confidence interval"), con)
    writeLines(paste0("# 25%-50%-75%: 25percentile-50percentile (median)-75percentile of the distribution of growth rates for the variable in the country over time period",years[1],"-",years[length(years)]), con)
    writeLines(paste0("# R1-R2: ratio of 50pers/25pers and 75perc/50perc"), con)
    writeLines(paste0("# value: list of outlier values (growth rates) identified-space seprated "), con)
    writeLines(paste0("# variable: list of years for which outlier values (growth rates) were identified"), con)
    writeLines(paste0("# years: Values reported"), con)
    write.csv(paramR,con)
    close(con)
    

    con <- file(paste0(csvfil,"_countryoutliers.csv"), open="wt")
    writeLines(paste0("# File created by EU-GIRP v.4 on ",figdate), con)
    writeLines(paste0("# List of country outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Country value outside of the '95% confidence interval'. I.e. the value was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the values reported for the variable during the time period and by all countries by which this variable is reported"), con)
    writeLines(paste0("# (2) To exclude minimim deviations though, the country values which are not more different that 10% from the median are NOT considered as outliers"), con)
    writeLines(paste0("# (3) A wide distribution indicates systematic errors (e.g. wrong unit used by MS(s)); therefore values with a ratio of 75perc/50perc or 50perc/25perc of more than ",maxr," are considered as outliers"), con)
    writeLines(paste0("# Explanation of column names (different from CRF-dimensions)"), con)
    writeLines(paste0("# lwhisk - uwhisk: lower and upper 'whisker': all values below or above this whiskers are outside of the 95% confidence interval"), con)
    writeLines(paste0("# 25%-50%-75%: 25percentile-50percentile (median)-75percentile of the distribution of growth rates for the variable in the country over time period",years[1],"-",years[length(years)]), con)
    writeLines(paste0("# R1-R2: ratio of 50pers/25pers and 75perc/50perc"), con)
    writeLines(paste0("# value: list of outlier values (growth rates) identified-space seprated "), con)
    writeLines(paste0("# variable: list of years for which outlier values (growth rates) were identified"), con)
    writeLines(paste0("# years: Values reported"), con)
    write.csv(paramcheck,con)
    close(con)
    
}
#paste0(runcateg,runpar)

