selectadmeasures<-function(request,M,A,meas2sum,x){
    avail<-meas2sum[!meas2sum%in%c("EM","NEXC")]
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
            tms<-paste0("measOK<-",measOK,"x<-'",x,"';sec<-'",sec,"';mea<-'",mea,"';uid<-'",uid,"';cat<-'",cat,"';cla<-'",cla,"';sou<-'",sou,"';tar<-'",tar,"'")
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

#allagri<-alldata[grepl("^3",alldata$sector_number),]
calcmeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
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

# CORRECTIONS

# 1. Autocorrections have an identified reason - update the data table
selection<-allagri$party=="EU28" & allagri$meastype%in%meas2popweight
calceu<-allagri[!selection,]
if(exists("autocorrections")){
    selection1<-autocorrections[,c("party","variableUID","autocorr")]
    calceu<-merge(calceu,selection1,by=c("party","variableUID"),all=TRUE)
    selection1<-!is.na(calceu$autocorr)
    correctcorrection<-function(autocorrections,party,uid){
        year<-autocorrections[autocorrections$party==party & autocorrections$variableUID==uid,years]
        return(year)
    }
    selc<-calceu[selection1,]
    t<-Reduce(rbind,lapply(c(1:sum(selection1)),function(x) 
        unlist(correctcorrection(autocorrections,selc$party[x],selc$variableUID[x]))))
    calceu[selection1,years]<-t
    allagri<-calceu
}
# 2. 'Unidentified' outliers are removed for calculation, but data table remains untouched
if(exists("paramcheck")){
    corcalceu<-subset(paramcheck,select=c("party","variableUID","correction"))
    calceucor<-merge(calceu,corcalceu,by=c("party","variableUID"),all=TRUE)
    calceucor$correction[is.na(calceucor$correction)]<-1
    selection2<-calceucor$correction==0
    calceucor[selection2,years]<-NA
}
eu28wei<-as.data.frame(matrix(rep(0,ncol(calceu)*nrow(assignad2par)),ncol=ncol(calceu),nrow=nrow(measures2wei)))
names(eu28wei)<-names(calceu)
eu28wei[,names(measures2wei)]<-measures2wei[,names(measures2wei)]
eu28wei[,years]<-euvalue("weight",assignad2par,calceucor,years,countriesic)
eu28wei[,"party"]<-rep("EU28",nrow(eu28wei))
eu28wei$notation[eu28wei$notation==0]<-""

allagri<-rbind(allagri[,allfields],eu28wei[,allfields])
write.table(allagri,file=paste0(csvfil,"_agri.csv"),sep=",")



