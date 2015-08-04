# # Correction now - not needed later OR do all meastype-adjustment here and NOT elsewhere.
calcmeas[(calcmeas$meastype=="" & grepl("3.F.1.",calcmeas$sector_number)),"meastype"]<-"RatioResCrop"
calceu[(calceu$meastype=="" & grepl("3.F.1.",calceu$sector_number)),"meastype"]<-"RatioResCrop"

### Adjust measures
calceu[grepl("^4",calceu$sector_number) & calceu$unit=="ha","meastype"]<-"AREA"
calceu[grepl("^4",calceu$sector_number) & calceu$unit=="kg N/yr","meastype"]<-"AD"
calceu[grepl("^4",calceu$sector_number) & calceu$unit=="kt","meastype"]<-"EM"
calceu[grepl("^4",calceu$sector_number) & calceu$unit=="t/unit","meastype"]<-"IEF"


### Remove duplicates ####
duplicates<-c("Animal Manure Applied to Soils",
              "Organic Fertilizers Applied to Soils",
              "Sewage Sludge Applied to Soils")
calceu<-calceu[!(calceu$sector_number=="3.D.1.2" & calceu$allmethods%in%duplicates),]

duplicates<-c("Direct N2O Emissions from N inputs_Land_Forest Land")
calceu<-calceu[!(calceu$sector_number=="4.A" & calceu$allmethods%in%duplicates),]

duplicates<-c("Direct N2O Emissions from N inputs_Wetlands_Wetlands")
calceu<-calceu[!(calceu$sector_number=="4.D" & calceu$allmethods%in%duplicates),]

duplicates<-c("Direct N2O Emissions from N inputs_Settlements_Settlements",
              "Direct N2O Emissions from N inputs_Land_Settlements",
              "Direct N2O Emissions from N inputs_Settlements")
calceu<-calceu[!(calceu$sector_number=="4.E" & calceu$allmethods%in%duplicates),]


calceunouid<-unique(subset(calceu,select=-variableUID))
calceunouid$variableUID<-calceu$variableUID[row.names(calceu)%in%row.names(calceunouid)]
calceu<-calceunouid
rm(calceunouid)

calcmeas<-unique(subset(calceu,select=cat3names[!cat3names %in% c("party",years)]))
measname<-as.data.frame(measname)
measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
listofmeasuresnotconsidered<-calcmeas[!calcmeas$meastype %in% c(meas2sum,meas2popweight,meas2clima,meas2mcf),]


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
    }
    
    return(paste0(measOK,collapse=" "))
}

# Set up table with infor AD to link with parameter
assignad2par<-unique(calcmeas[calcmeas$meastype %in% meas2popweight,!(names(calcmeas)%in%c("allmethods","climat"))])
assignad2par$adpars<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("meastype",calcmeas,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))
assignad2par$adunit<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("unit",calcmeas,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))
assignad2par$aduids<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("variableUID",calcmeas,meas2sum,assignad2par$sector_number[x],assignad2par$meastype[x])))

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
    #print(paste0("Auid<-",Auid))
    #print(paste0("Puid<-",Puid))
    #print(paste0("ok<-",ok))
    
    if(ok=="-" | ok==""){
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
        
        l<-lapply(c(1:nrow(E)),function(x) weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c))
    }
    m<-matrix(unlist(l),ncol=length(y),byrow=T)
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


