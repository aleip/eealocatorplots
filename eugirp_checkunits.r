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
