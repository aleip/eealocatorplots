# # Correction now - not needed later
cat3alltab[(cat3alltab$meastype=="" & grepl("3.F.1.",cat3alltab$sector_number)),"meastype"]<-"RatioResCrop"
cat3all[(cat3all$meastype=="" & grepl("3.F.1.",cat3all$sector_number)),"meastype"]<-"RatioResCrop"

duplicates<-c("Animal Manure Applied to Soils",
              "Organic Fertilizers Applied to Soils",
              "Sewage Sludge Applied to Soils")
cat3all<-cat3all[!(cat3all$sector_number=="3.D.1.2" & cat3all$allmethods%in%duplicates),]
cat3alltab<-cat3alltab[!(cat3alltab$sector_number=="3.D.1.2" & cat3alltab$allmethods%in%duplicates),]

measname<-as.data.frame(measname)
meas2sum<-c("EM","AD","PROD","AREA")
meas2popweight<-c("IEF","DIGEST","PREGNANT","YM","FEEDING","MILK","Milk","WORK","WEIGHT",
                  "GE","GEav","VSEXC","MASS","B0","NEXC",
                  "FracGASM","FracGASF","FracLEACH","FracBURN","FracOXIDIZED","RatioResCrop",
                  "Combustion","DM","ORGAMENDMENT","YIELD")
meas2clima<-c("CLIMA")
meas2mcf<-c("MCF")

measures2sum<-cat3alltab[cat3alltab$meastype %in% meas2sum,]
remarks1<-cat3alltab[!cat3alltab$meastype %in% c(meas2sum,meas2popweight,meas2clima,meas2mcf),]


selectadmeasures<-function(request,M,meas2sum,sec,mea){
    avail<-meas2sum[!meas2sum%in%"EM"]
    # Use "Total Biomass burned [kt dm]" for IEF CH4 and N2O in Table 3.F
    if(grepl("^3.F",sec) & mea=="IEF") avail<-"AD"
    # Use "Area burned [k ha/yr]" for YIELD "Biomass available [t dm/ha] in Table 3.F
    if(grepl("^3.F",sec) & mea=="YIELD") avail<-"AREA"
    # Use "Crop production [t]" for parameters in 'Additional information' in Table 3.F
    if(grepl("^3.F",sec) & mea%in%c("DM","FracBURN","FracOXIDIZED","Combustion","RatioResCrop")) avail<-"PROD"

    measOK<-as.vector(unique(M[,request][M$meastype %in% avail & M$sector_number==sec]))

    # For cat 3.B.2 ADs might not be give ... use those for 3.A instead
    if(grepl("^3.B.2.1",sec) & length(measOK)==0){
        sec<-gsub("3.B.2","3.A",sec)
        uid<-(unique(M[,"variableUID"][M$meastype=="AD" & M$sector_number==sec]))
        measOK<-as.vector(unique(M[,request][M$variableUID==uid]))
    }

    if(grepl("^3.D.1.3",sec) & length(measOK)==0) {
        if(request=="variableUID")measOK<-paste0("AD for",sec," (N Deposited by Grazing Animals) needs to be calculated from Table B(b)")
        if(request=="meastype")measOK<-"-"
    }
    if(grepl("^3.D.AI",sec) & length(measOK)==0) {
        if(request=="variableUID")measOK<-paste0("Fractions ",sec," needs to be checked")
        if(request=="meastype")measOK<-"-"
    }
    if(grepl("^3.B.2.5 N2O Emissions per MMS",sec) & length(measOK)==0) {
        if(request=="variableUID")measOK<-paste0("Total N handled in ",sec," needs to be calc&checked")
        if(request=="meastype")measOK<-"-"
    }
    
    
    
    return(paste0(measOK,collapse=" "))
}

# Set up table with infor AD to link with parameter
assignad2par<-unique(cat3alltab[cat3alltab$meastype %in% meas2popweight,!(names(cat3alltab)%in%c("allmethods","climat"))])
assignad2par$adpars<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("meastype",cat3alltab,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))
assignad2par$adunit<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("unit",cat3alltab,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))
assignad2par$aduids<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("variableUID",cat3alltab,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))

measures2wei<-cat3alltab[cat3alltab$variableUID %in% assignad2par$variableUID,]
remarks2<-(assignad2par[assignad2par$adunit=="",])

if(nrow(remarks1)>0) View(remarks1)
if(nrow(remarks2)>0) View (remarks2)

countriesnoic<-allcountries$party[!allcountries$party %in% "IC"]
countriesic<-allcountries$party 


sumovercountries<-function(D,uid,y,c){
    y<-as.character(y)
    s<-matrix(0,ncol=length(y))
    m<-matrix(0,ncol=length(y),nrow=length(c))
    m<-D[D$variableUID==uid,y]
    s<-apply(m,2,sum)
    return(s)
}
weightovercountries<-function(D,Auid,Puid,ok,y,c){
    if(ok=="-"){
        s<-rep(NA,length(y))
    }else{
        y<-as.character(y)
        s<-matrix(0,ncol=length(y))
        ad<-matrix(0,ncol=length(y),nrow=length(c))
        pa<-ad
        ad<-extractuiddata(D,Auid,c)
        pa<-extractuiddata(D,Puid,c)
        m<-ad*pa
        s<-apply(m,2,sum)/apply(ad,2,sum)
    }
    return(s)
}
euvalue<-function(todo,E,D,y,c){
    if(todo=="sum")l<-lapply(c(1:nrow(E)),function(x) sumovercountries(D,E$variableUID[x],y,c))
    if(todo=="weight"){
        
        #print(paste0("Auid<-",E$aduids[x]))
        #print(paste0("Puid<-",E$variableUID[x]))
        #print(paste0("ok<-",E$adpars[x]))
        
        l<-lapply(c(1:nrow(E)),function(x) weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c))
    }
    m<-matrix(unlist(l),ncol=length(y),byrow=T)
}

eu28sum<-as.data.frame(matrix(rep(0,ncol(cat3all)*nrow(measures2sum)),ncol=ncol(cat3all),nrow=nrow(measures2sum)))
names(eu28sum)<-names(cat3all)
eu28sum[,names(measures2sum)]<-measures2sum[,names(measures2sum)]

eu28sum[,years]<-euvalue("sum",eu28,cat3all,years,countriesic)


eu28wei<-as.data.frame(matrix(rep(0,ncol(cat3all)*nrow(assignad2par)),ncol=ncol(cat3all),nrow=nrow(measures2wei)))
names(eu28wei)<-names(cat3all)
eu28wei[,names(measures2wei)]<-measures2wei[,names(measures2wei)]

eu28wei[,years]<-euvalue("weight",assignad2par,cat3all,years,countriesic)


