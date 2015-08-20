library(reshape2) #required for melt function - for outls
trendoutlmethod<-2
maxr<-5
colexpl<-paste0("# Explanation of column names (different from CRF-dimensions)\n",
                "# min-max: minimum (ignoring reported zeroes) and maximum values\n",
                "# lwhisk-uwhisk: lower and upper 'whisker'. Values below the lower whisker",
                " or above the upper whisker are considered as outliers.",
                " The whiskers are determined to cover 95% of values when they were normally distributed.",
                "# p25-p50-p75: 25percentile-50percentile (median)-75percentile ",
                "of the distribution of growth rates for the variable in the ",
                "country over time period",years[1],"-",years[length(years)],"\n",
                "# range: max:min ratio - zeros excluded\n",
                "# value: list of outlier values identified-space separated\n",
                "# variable: list of years for which outlier values were identified\n",
                "# years: Values reported")
coutlexp<-paste0("# File created by EU-GIRP v.4 on ",figdate,"\n",
                 "# List of country outliers. Outliers were identified with the following criteria",
                 "# (1) Country value outside of the '95% confidence interval'.",
                 "I.e. the value was outside the range of median +/- 1.953 x (median-75percentile/25percentile)",
                 " - (see whiskers)",
                 "of the values reported for the variable during the time period and ",
                 "by all countries by which this variable is reported",
                 "# (2) To exclude minimim deviations though the country values ",
                 "which are not more different that 10% from the median ",
                 "are NOT considered as outliers",
                 "# (3) A wide distribution indicates systematic errors ",
                 "(e.g. wrong unit used by MS(s)); therefore values with a ratio ",
                 "of 75perc/50perc or 50perc/25perc of more than ",maxr,
                 " are considered as outliers")


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

noemissions<-c(meas2popweight,meas2clima,meas2mcf,meas2sum[!meas2sum%in%"EM"])
growthdata<-allgrowth
growthmeas<-restrictdata(growthdata,noemissions)
growthdata<-growthdata[growthdata$variableUID %in% growthmeas$variableUID,]
growthdata<-growthdata[growthdata$party!="EU28",]
growthdata<-growthdata[order(growthdata$sector_number,growthdata$category),]
growthmeas<-growthmeas[order(growthmeas$sector_number,growthmeas$category),]

parammeasures<-c(meas2popweight,meas2mcf)
paramdata<-alldata
parammeas<-restrictdata(paramdata,parammeasures)
parammeas<-parammeas[parammeas$measure!="Nitrogen excretion per MMS",]
parammeas<-parammeas[order(parammeas$sector_number,parammeas$category),]
paramdata<-paramdata[paramdata$variableUID %in% parammeas$variableUID,]
paramdata<-paramdata[paramdata$party!="EU28",]
paramdata<-paramdata[order(paramdata$sector_number,paramdata$category),]

# CORRECTION OF ERRONEOUS VALUES ####
writecorrection<-function(v,P,mult,name){
    if(nrow(v)>0) {
        v<-unique(v[,1])
        before<-P[v,]
        P[v,years]<-t(Reduce(cbind,lapply(c(1:length(v)),function(x) t(100*P[v[x],years]))))
        after<-P[v,]
        before$corr<-0
        after$corr<-1
        c<-paste(as.vector(unique(before$party)),collapse="-")
        
        con <- file(paste0(issuedir,name,"_",c,".csv"), open="wt")
        writeLines("# Data before correction\n#",con)
        write.csv(before,con)
        writeLines("#\n#\n# Data after correction\n#",con)
        write.csv(after,con)
        close(con)
    }
    return(P)
}

# DIGESTIBILITY (Table 3As2) is in % - multiply values < 1 with 100
v<-which(paramdata$meastype=="DIGEST" & paramdata[years]<1 & paramdata[years]!=0,arr.ind = TRUE)
paramdata<-writecorrection(v,paramdata,100,"DIGEST")
    
# FRACBURN can must have data below 1 (unit is fraction not percent)
# ... but if it is there is no 'outlier' that can be identified ... therefore remove from list
v<-which(paramdata$meastype=="FracBURN" & paramdata[years]>=1,arr.ind = TRUE)
paramdata<-writecorrection(v,paramdata,0.01,"FracBURN")
v<-which(paramdata$meastype=="FracBURN" & paramdata[years]<1 & paramdata[years]>=0,arr.ind = TRUE)
if(nrow(v)>0) {
    v<-unique(v[,1])
    paramdata<-paramdata[!c(1:nrow(paramdata))%in%v,]}

# DIRECT N2O EMISSIONS - IEF FRACTION NOT PERCENT (DIVIDE BY 100)
v<-which((grepl("3.D.1.[14]",paramdata$sector_number) | grepl("3.D.2.[12]",paramdata$sector_number)) & 
             paramdata$meastype=="IEF" & paramdata[years]>=1,arr.ind = TRUE)
paramdata<-writecorrection(v,paramdata,0.01,"N2OIEF")

# YM - VALUE REPORTED 10 TIMES TOO HIGH (OCCURS FOR GR)
v<-which((grepl("3.A.1",paramdata$sector_number) ) & 
             paramdata$meastype=="YM" & paramdata[years]>=10,arr.ind = TRUE)
paramdata<-writecorrection(v,paramdata,0.1,"YM")

# MCF - VALUE REPORTED AS FRACTION INSTEAD OF PERCENT (MULTIPLY WITH 100)
v<-which( (paramdata$meastype=="MCF" & paramdata$source!="Daily spread" & paramdata[years]==0.1) |
              (paramdata$meastype=="MCF" & paramdata[years]<0.08 & paramdata[years]!=0)
              ,arr.ind = TRUE)
paramdata<-writecorrection(v,paramdata,100,"MCF")

# EXCLUDE OTHER LIVESTOCK FROM CHECKS - INHOMOGENEOUS COMPOSITION
paramdata<-paramdata[! paramdata$category=="Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Livestock",]
paramdata<-paramdata[! paramdata$category=="Other Other Other Livestock",]


# 'SERIOUS' ERRORS, LIKLY DUE TO COMMA-PROBLEM
growth<-growthdata[,c(years,"party","variableUID")]
param<-paramdata[,c(years,"party","variableUID")]
paramV<-paramdata[unique(which(paramdata[years]>1000,arr.ind = TRUE)[,1]),]

# Save quantiles for growth rate
# Store 'bias' in distibution of parameters: ratio of max/min (zeros excluded)
#        -- If there is systematic mistakes in errors the bias is high
growthquantiles<-subset(growth,select=c("variableUID","party"))
growthquantiles[,c("min","p25","p50","p75","max")]<-t(Reduce(cbind,lapply(c(1:nrow(growth)),function(x) 
    selquantiles(growth[x,years]))))

paramquantiles<-as.data.frame(unique(param$variableUID))
names(paramquantiles)<-"variableUID"
paramquantiles[,c("min","p25","p50","p75","max")]<-t(Reduce(cbind,lapply(paramquantiles$variableUID,function(x) 
    selquantiles(param[param$variableUID==x,years]))))
paramquantiles$range<-paramquantiles[,"max"]/paramquantiles[,"min"]
paramquantiles$range[is.nan(paramquantiles$range)]<-0


# Median absolute deviation of the absolute deviations from the median
growthmad<-apply(growth[years], 1, mad,  na.rm = TRUE)
paramquantiles$mad<-unlist(lapply(paramquantiles$variableUID,function(x) 
    mad(param[param$variableUID==x,years],na.rm=TRUE)))

if(trendoutlmethod==1){
    # Method 1 in outl tool: Deviation from median
    growthmeddevtest<-abs(growth-growthquantiles[2,])>2*growthmadn
    meddevtest<-abs(trend-trendquantiles[2,])>2*trendmadn
    
} else if (trendoutlmethod==2){
    # Method 2 in outl tool
    # Obtaining 95% confidence intervall 
    bxplf <- 0.953

    growthquantiles$uwhisk<-growthquantiles[,"p50"]+(1 + bxplf)*(growthquantiles[,"p75"]-growthquantiles[,"p50"])
    growthquantiles$lwhisk<-growthquantiles[,"p50"]+(1 + bxplf)*(growthquantiles[,"p25"]-growthquantiles[,"p50"])
    growthmeddevtest<-(growth[years]>growthquantiles$uwhisk) | (growth[years]<growthquantiles$lwhisk)
    
    paramquantiles$uwhisk<-paramquantiles[,"p50"]+(1 + bxplf)*(paramquantiles[,"p75"]-paramquantiles[,"p50"])
    paramquantiles$lwhisk<-paramquantiles[,"p50"]+(1 + bxplf)*(paramquantiles[,"p25"]-paramquantiles[,"p50"])
    param<-merge(param,paramquantiles,by="variableUID")

    #               Keep values which are outside of the 95% confidence intervall
    parammeddevtest<-(((param[years]>param$uwhisk) | (param[years]<param$lwhisk)) &
        #               Do NOT keep values which are very close to the median (rounding errors)
        (abs(1-param[years]/param[,"p50"])>1.1))  #| 
        #               Keep values with a very large distribution
        #(param$range>maxr)
}

growthoutl<-growthmeddevtest*growth[years]
ngrowthoutl<-sum(growthoutl!=0,na.rm=TRUE)
paramoutl<-parammeddevtest[,years]*param[,years]
nparamoutl<-sum(paramoutl!=0,na.rm=TRUE)
#print("#unlist(growthoutl)")

if(ngrowthoutl>0){
    #listofoutls<-which(unlist(subset(growthoutl,select=-country))!=0)
    listofoutls<-which(unlist(growthoutl)!=0)
    growthoutl$party<-growth$party
    growthoutl$variableUID<-growth$variableUID
    #growthoutl$rown<-row.names(growthoutl)
    
    growthoutllist<-melt(growthoutl,id.vars=c("party","variableUID"),na.rm=F)
    select<-! is.na(growthoutllist$value)
    growthoutllist<-growthoutllist[! is.na(growthoutllist$value),]
    growthoutllist<-growthoutllist[growthoutllist$value !=0,]
    #Restrict results to >3% growth rate
    ming<-0.03
    select<-(growthoutllist$value<(1-ming) | growthoutllist$value>(1+ming))
    growthoutllist<-growthoutllist[select,]
    
    growthoutllist<-merge(growthoutllist,growthmeas,by="variableUID")
    growthoutllist<-merge(growthoutllist,growthquantiles,by=c("variableUID","party"))
    
    
    growthoutlvalues<-subset(growthoutllist,select=c(value,party,variableUID))
    growthoutllist<-subset(growthoutllist,select=-value)
    
    growthcheck<-simplifytestmatrix(check = growthoutllist,group = "variable",compare = years)
    growthcheckv<-aggregate(value ~ party + variableUID,data =growthoutlvalues,mean)
    x<-simplifytestmatrix(check = growthoutlvalues, group = "value")
    growthcheck<-merge(growthcheck,x,by=c("variableUID","party"))
    growthcheck<-merge(growthcheck,alldata[c("variableUID","party",years)],by=c("variableUID","party"))
    growthcheck<-merge(growthcheck,growth,by=c("variableUID","party"))
    
    growthcheck<-cbind(cursubm,growthcheck,row.names=NULL)                 
    o<-order(growthcheck$party,growthcheck$sector_number,growthcheck$category)
    n1<-c("party","sector_number","meastype","category","min","lwhisk","p25","p50","p75","uwhisk","max","value","variable")
    n3<-c("gas","unit","method","classification","source","target","type","option","measure",
          "variableUID","cursubm")
    n<-c(n1,paste0(years,".x"),paste0(years,".y"),n3)
    growthcheck<-growthcheck[o,n]
    names(growthcheck)<-c(n1,years,paste0(years,".growth"),n3)
    
    con <- file(paste0(csvfil,"_growthcheck.csv"), open="wt")
    writeLines(paste0("# File created by EU-GIRP v.4 on ",figdate), con)
    writeLines(paste0("# List of trend outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Growth Rate outside of the '95% confidence interval'. I.e. the growth rate was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the growth rates during the time period for this variable and country"), con)
    writeLines(paste0("# (2) Growth Rate larger than ", 1+ming," or smaller than ",1-ming), con)
    writeLines(colexpl, con)
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
    paramcheck$correction<-""
    
    o<-order(paramcheck$meastype,paramcheck$sector_number,paramcheck$category,paramcheck$party)
    n<-c("party","sector_number","meastype","unit","category","min","lwhisk","p25","p50","p75","uwhisk","max",
         "range","value","correction","variable",years,"gas",
         "mad","method","classification","source","target","type","option","measure","variableUID")
    paramcheck<-paramcheck[o,n]
    paramcheck<-cbind(cursubm,paramcheck,row.names=NULL)    
    
    #Check of systematic errors
    serious<-function(P,uid,c){
        if(uid %in% P$variableUID){if(c %in% P$party[P$variableUID==uid]){s<-"0"}else{s<-""}
        }else{s<-""}
        return(s)
    }
    paramcheck$correction<-unlist(lapply(c(1:nrow(paramcheck)),function(x)
        serious(paramV,paramcheck$variableUID[x],paramcheck$party[x])))


    
    paramR<-paramcheck[paramcheck$range>maxr,]
    
    con <- file(paste0(csvfil,"_countryoutliersserious.csv"), open="wt")
    writeLines(coutlexp, con)
    writeLines(colexpl, con)
    write.csv(paramV,con)
    close(con)
    

    con <- file(paste0(csvfil,"_countryoutliers.csv"), open="wt")
    writeLines(coutlexp, con)
    writeLines(colexpl, con)
    writeLines("#\n#Note for column: correction: 0: value assumed to be a mistake. it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification. 1: value is assumed to be not an outlier despite the criteria (e.g. milk production). empty: to be clarified",con)
    write.csv(paramcheck,con)
    close(con)
    
}
#paste0(runcateg,runpar)

