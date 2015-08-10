selectadmeasures<-function(request,M,A,meas2sum,x){
    avail<-meas2sum[!meas2sum%in%"EM"]
    sec<-A$sector_number[x]
    cat<-A$category[x]
    cla<-A$classification[x]
    sou<-A$source[x]
    tar<-A$target[x]
    mea<-A$meastype[x]
    gas<-A$gas[x]
    typ<-A$type[x]
    uid<-A$variableUID[x]
    uni<-A$unit[x]
    
    #print(paste0("x<-",x,";sec<-",sec,";mea",mea,";cat<-",cat))
    # Use "Total Biomass burned [kt dm]" for IEF CH4 and N2O in Table 3.F
    if(grepl("^3.F",sec) & mea=="IEF") avail<-"AD"
    # Use "Area burned [k ha/yr]" for YIELD "Biomass available [t dm/ha] in Table 3.F
    if(grepl("^3.F",sec) & mea=="YIELD") avail<-"AREA"
    # Use "Crop production [t]" for parameters in 'Additional information' in Table 3.F
    if(grepl("^3.F",sec) & mea%in%c("DM","FracBURN","FracOXIDIZED","Combustion","RatioResCrop")) avail<-"PROD"
    # Indirect emissions in Table 3.B.2
    if(grepl("^3.B.2.5",sec) & uid=="47100CA3-9371-4944-B302-BD76A154B0F4") avail<-"Nvol"
    if(grepl("^3.B.2.5",sec) & uid=="C548A926-2825-4F66-A6FE-DA55F429CB29") avail<-"Nleach"
    # 3.B.2.5 N2O Emissions per MMS
    if(grepl("3.B.2.5 N2O Emissions per MMS",sec)) avail<-"NEXC"
    # Solid waste disposal
    if(grepl("5.[AB].",sec)) avail<-"AD"
    # Waste incineration
    if(grepl("Clinical Waste",cat) & typ=="Biogenic") avail<-"ADbio"
    if(grepl("Clinical Waste",cat) & typ=="Non-biogenic") avail<-"ADnbio"
    # Waste Water treatment and discharge
    if(grepl("5.D.",sec) & gas=="CH4") avail<-"ADORG"
    if(grepl("5.D.",sec) & gas=="N2O") avail<-"ADEFFLUENT"
    
    
    measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
            & M$sector_number==sec & M$category==cat & M$classification==cla 
            & M$source==sou & M$target==tar]))
    
    # In Tables 3.B. ignore source (MMS) and classification 
    if(length(measOK)==0 & grepl("^3.B",sec)){
        measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                             & M$sector_number==sec & M$category==cat
                                             & M$target==tar]))
    }
    
    
    # In Tables 3.B. not all ADs are given - they are the same as in Table 3.A
    #                ignore source (MMS) and classification (Enteric vs. )
    if(length(measOK)==0 & grepl("^3.B",sec)){
        sec<-gsub("3.B.[12]","3.A",sec)
        measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                             & M$sector_number==sec & M$category==cat 
                                             & M$target==tar]))
        #print(measOK)
        if(length(measOK)==0) {
            tms<-paste0("x<-'",x,"';sec<-'",sec,"';mea<-'",mea,"';uid<-'",uid,"';cat<-'",cat,"';cla<-'",cla,"';sou<-'",sou,"';tar<-'",tar,"'")
            print(tms)
            stop()
        }
    }
    
    
    # For cat 3.B.2 ADs might not be give ... use those for 3.A instead
    if(length(measOK)==0){
        if(grepl("^3.B.2.1",sec)){
            sec<-gsub("3.B.2","3.A",sec)
            uid<-(unique(M[,"variableUID"][M$meastype=="AD" & M$sector_number==sec]))
            measOK<-as.vector(unique(M[,request][M$variableUID==uid]))
        }
        
        if(grepl("^3.D.1.3",sec)) {
            if(request=="variableUID")measOK<-paste0("AD for",sec," (N Deposited by Grazing Animals) needs to be calculated from Table B(b)")
            if(request=="meastype")measOK<-"-"
        }
        if(grepl("^3.D.AI",sec)) {
            if(request=="variableUID")measOK<-paste0("Fractions ",sec," needs to be checked")
            if(request=="meastype")measOK<-"-"
        }
        if(grepl("^3.B.2.5 N2O Emissions per MMS",sec)) {
            if(request=="variableUID")measOK<-paste0("Total N handled in ",sec," needs to be calc&checked")
            if(request=="meastype")measOK<-"-"
        }
    }else if(length(measOK)>1){
        measOK<-c(length(measOK),measOK)
        if(uni=="t/unit"){
            measOK<-c(measOK,"variable type")
        }else{
            View(A[x,])
            print(measOK)
            print(paste0("meastype=",mea))
            tms<-paste0("x<-'",x,"';sec<-'",sec,"';mea<-'",mea,"';uid<-'",uid,"';cat<-'",cat,"';cla<-'",cla,"';sou<-'",sou,"';tar<-'",tar,"'")
            print(tms)
            stop()
        }
    }
    
    return(paste0(measOK,collapse=" "))
}

calceu<-alldata
calcmeas<-unique(subset(calceu,select=allfields[!allfields %in% c("notation","party",years)]))
#measname<-as.data.frame(measname)
measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
listofmeasuresnotconsidered<-calcmeas[!calcmeas$meastype %in% c(meas2sum,meas2popweight,meas2clima,meas2mcf),]
# Set up table with infor AD to link with parameter
assignad2par<-unique(calcmeas[calcmeas$meastype %in% meas2popweight,!(names(calcmeas)%in%c("method","measure"))])
assignad2par$adpars<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("meastype",calcmeas,assignad2par,meas2sum,x)))
assignad2par$adunit<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("unit",calcmeas,assignad2par,meas2sum,x)))
assignad2par$aduids<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("variableUID",calcmeas,assignad2par,meas2sum,x)))
namesassignad2par<-c("meastype","gas","unit","sector_number","adpars","adunit",
                     "category","classification","source","target","option","variableUID","aduids")
assignad2par<-assignad2par[,namesassignad2par]
measures2wei<-calcmeas[calcmeas$variableUID %in% assignad2par$variableUID,]
parameterswithoutADs<-(assignad2par[assignad2par$adunit=="",])

#if(nrow(remarks1)>0) View(remarks1)
#if(nrow(remarks2)>0) View (remarks2)

countriesnoic<-allcountries[!allcountries %in% "IC"]
countriesic<-allcountries 


sumovercountries<-function(D,uid,y,c){
    y<-as.character(y)
    s<-matrix(0,ncol=length(y))
    m<-matrix(0,ncol=length(y),nrow=length(c))
    m<-D[D$variableUID==uid,y]
    s<-apply(m,2,sum)
    return(s)
}
weightovercountries<-function(D,Auid,Puid,ok,y,c){
    
    # Returns the weighted average over all countries
    # for which both AD and a Value for the variable exist.
    # In case a country as a value (e.g. IEF) but does not 
    # report Ad, then this is excluded from the EU weighted average!
    
    #print(paste0("Auid<-",Auid))
    #print(paste0("Puid<-",Puid))
    #print(paste0("ok<-",ok))
    
    if(ok=="-" | ok=="" | grepl("^[1-9]",ok)){
        s<-rep(NA,length(y))
    }else{
        y<-as.character(y)
        s<-matrix(0,ncol=length(y))
        ad<-matrix(0,ncol=length(y),nrow=length(c))
        pa<-ad
        ad<-extractuiddata(D,Auid,c)
        pa<-extractuiddata(D,Puid,c)
        
        ad<-ad[!is.na(apply(pa,1,sum,rm.na=TRUE)),]
        pa<-pa[!is.na(apply(pa,1,sum,rm.na=TRUE)),]

        if(length(pa)<length(c)){
            ad<-t(ad)
            pa<-t(pa)
        }
        pa<-pa[!is.na(apply(ad,1,sum,rm.na=TRUE)),]
        ad<-ad[!is.na(apply(ad,1,sum,rm.na=TRUE)),]
        if(length(pa)<length(c)){
            ad<-t(ad)
            pa<-t(pa)
        }
        
        if(nrow(ad)>0 & sum(apply(ad,2,sum))!=0 ){
            m<-ad*pa
            s<-apply(m,2,sum)/apply(ad,2,sum)
        }else if(nrow(ad)>0 & sum(apply(ad,2,sum))==0){
            #Calculate average
            s<-apply(pa,2,mean)   
        }else{
            s<-0
        }
    }
    #if(is.nan(s)) s<-0
    #if(is.na(s)) s<-0
    return(s)
}
euvalue<-function(todo,E,D,y,c){
    if(todo=="sum")l<-lapply(c(1:nrow(E)),function(x) sumovercountries(D,E$variableUID[x],y,c))
    if(todo=="weight"){
        
        l<-lapply(c(1:nrow(E)),function(x) weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c))
    }
    #m<-matrix(unlist(l),ncol=length(y),byrow=T)
    m<-t(Reduce(cbind,l))
}

eu28sum<-as.data.frame(matrix(rep(0,ncol(calceu)*nrow(measures2sum)),ncol=ncol(calceu),nrow=nrow(measures2sum)))
names(eu28sum)<-names(calceu)
eu28sum[,names(measures2sum)]<-measures2sum[,names(measures2sum)]

eu28sum[,years]<-euvalue("sum",eu28sum,calceu,years,countriesic)
eu28sum[,"party"]<-rep("EU28",nrow(eu28sum))

eu28wei<-as.data.frame(matrix(rep(0,ncol(calceu)*nrow(assignad2par)),ncol=ncol(calceu),nrow=nrow(measures2wei)))
names(eu28wei)<-names(calceu)
eu28wei[,names(measures2wei)]<-measures2wei[,names(measures2wei)]

eu28wei[,years]<-euvalue("weight",assignad2par,calceu,years,countriesic)
eu28wei[,"party"]<-rep("EU28",nrow(eu28wei))

eu28sum$notation[eu28sum$notation==0]<-""
eu28wei$notation[eu28wei$notation==0]<-""

alldata<-rbind(alldata,eu28sum)
alldata<-rbind(alldata,eu28wei)
write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_cat3step2.csv"),sep=",")


