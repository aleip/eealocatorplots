rm(allinfos,allnotations)
# Clean up memory ####
torm1<-c("actcount","categorymatrix","eealocator")
torm2<-torm1[torm1 %in% ls()]
if(length(torm2)>0){rm(list=torm2)}

doplots<-2
doplotsv<-2

docateg<-"all"

# Filter emissions and ADs and IEFs
plotdata<-alldata
selectiefuid<-function(req,A,M,x){
    sec<-A$sector_number[x]
    cat<-A$category[x]
    cla<-A$classification[x]
    met<-A$method[x]
    sou<-A$source[x]
    tar<-A$target[x]
    mea<-A$meastype[x]
    msr<-A$measure[x]
    gas<-A$gas[x]
    typ<-A$type[x]
    opt<-A$option[x]
    uid<-A$variableUID[x]
    uni<-A$unit[x]
    
    if(req=="ief"){
        lookmeasure<-"Implied emission factor"
        if(sec=="3.B.2.5" & msr!="Emissions"){lookmeasure<-msr}
        measOK<-as.vector(unique(M[  M$sector_number==sec & M$category==cat 
                                     & M$classification==cla & M$method==met
                                     & M$source==sou & M$target==tar
                                     & M$gas==gas & M$option==opt
                                     & grepl(lookmeasure,M$measure)
                                     & M$meastype=="IEF","variableUID"]))
    }else if(req=="ad"){
        measOK<-as.vector(unique(M[  M$sector_number==sec & M$category==cat 
                                     & M$classification==cla & M$method==met
                                     & M$source==sou & M$target==tar
                                                & M$option==opt
                                     #& grepl(lookmeasure,M$measure)
                                     & M$meastype%in%c("AD","POP","AREA","FUEL"),"variableUID"]))
    }
    if(length(measOK)==0) {measOK<-0}
    if(length(measOK)>1){measOK<-paste(c(length(measOK),measOK),collapse=",")}
    return(measOK)
}
gases<-c("CH4","CO2","N2O","Aggregate GHGs")
emissions<-measures2sum[measures2sum$meastype=="EM" & measures2sum$gas %in% gases,]
emissions$IEFuid<-unlist(lapply(c(1:nrow(emissions)),function(x)
    selectiefuid("ief",emissions,measures2wei,x)
    ))

emissions$ADuid<-unlist(lapply(c(1:nrow(emissions)),function(x)
    if(emissions$IEFuid[x]==0){"no IEF"}else
        if(grepl("^[2-9],",emissions$IEFuid[x])){"multiple IEF"}else{
            assignad2par$aduids[assignad2par$variableUID==emissions$IEFuid[x]]
        }
    ))
select<-emissions$IEFuid==0 | grepl("^[2-9],",emissions$IEFuid)
emissionsnoief<-emissions[select,]
emissions<-emissions[!select,]

select<-emissions$ADuid=="" | grepl("^[2-9] ",emissions$ADuid) | nchar(emissions$ADuid)>36
emissionsnoad<-emissions[select,]
emissions<-emissions[!select,]

emissionsnoief$ADuid<-unlist(lapply(c(1:nrow(emissionsnoief)),function(x)
    if(emissionsnoief$ADuid[x]==0){selectiefuid("ad",emissions,measures2wei,x)}else{
        emissionsnoief$ADuid[x]
    }
))

activitydata<-measures2sum[measures2sum$variableUID%in%unlist(as.vector(emissions$ADuid)),]
activitydata<-activitydata[order(activitydata$sector_number,activitydata$category),]
emissions<-emissions[order(emissions$sector_number,emissions$category),]

# Calculate shares for EM trends based on ADs and IEFs
emissiontrendshares<-function(DF,emuid,iefuid,aduid,nears,countries){
    
    
    # From run_emi_EU15_data.sh
    # c:\adrian\data\inventories\ghg\unfccc\inventorysystem\eea_locator_tool\plots\scripts\run_emi_EU15_data.sh
    #  pct_act = sprintf("%.0f",100 * ( ACT[country,endy] * ( vstarty / ACT[country,starty] ) - vstarty ) / ( vendy - vstarty ) )
    ### pct_act1<-100*(actcount[,nyears]*(sum(eealocator[,1])/actcount[,1])-sum(eealocator[,1]))/(sum(eealocator[,nyears])-sum(eealocator[,1]))
    
    # Alternative:
    # DeltaE = En - Ea = ADn * IEFn - ADa * IEFa
    # DeltaE* = (ADn - ADa) * IEFa
    # DeltaE& = (IEFn - IEFa) * ADn
    # DeltaE* + DeltaE& = DeltaE
    # Share AD: DeltaE*/DeltaE
    actcount<-extractuiddata(DF,uid = aduid,c=countries )
    iefcount<-extractuiddata(DF,uid = iefuid,c=countries )
    eealocator<-extractuiddata(DF,uid = emuid,c=countries )

    countriesavailable<-row.names(eealocator[row.names(eealocator)%in%row.names(iefcount),])
    countriesavailable<-countriesavailable[countriesavailable%in%actcountries]
    emcountries<-row.names(eealocator)%in%countriesavailable
    ifcountries<-row.names(iefcount)%in%countriesavailable
    adcountries<-row.names(actcount)%in%countriesavailable
    
    deltaEM_EU28<-sum(eealocator[emcountries,nyears])-sum(eealocator[emcountries,1])
    deltaEM<-eealocator[emcountries,nyears]-eealocator[emcountries,1]
    deltaAD<-actcount[adcountries,nyears]-actcount[adcountries,1]
    deltaIF<-iefcount[ifcountries,nyears]-iefcount[ifcountries,1]
    pct_act2<-100*((actcount[adcountries,nyears]-actcount[adcountries,1])
                   *iefcount[ifcountries,1])/
        (sum(eealocator[emcountries,nyears])-sum(eealocator[emcountries,1]))/1000
    
    # Share of AD and IEF on EU absolute trend
    act_pcteu<-100*(deltaAD*iefcount[ifcountries,1]/1000)/deltaEM_EU28
    ief_pcteu<-100*(deltaIF*actcount[adcountries,nyears]/1000)/deltaEM_EU28
    # Share of AD and IEF on country absolute trend
    act_pctcountry<-100*(deltaAD*iefcount[ifcountries,1]/1000)/deltaEM
    ief_pctcountry<-100*(deltaIF*actcount[adcountries,nyears]/1000)/deltaEM
    
    act_pctcouneu<-100*((acteu28[nyears]-acteu28[1])*iefeu28[1]/1000)/(emieu28[nyears]-emieu28[1])
    ief_pctcouneu<-100*((iefeu28[nyears]-iefeu28[1])*acteu28[nyears]/1000)/(emieu28[nyears]-emieu28[1])
    
    return(c(act_pcteu,ief_pcteu,act_pctcountry,ief_pctcountry,act_pctcouneu,ief_pctcouneu))
}
emissionshares<-c("act_pcteu","ief_pcteu","act_pctcountry","ief_pctcountry","act_pctcouneu","ief_pctcouneu")



# Filter out IEFs
if(restrictsector!'"'){
    select <- grepl(restrictsector,plotdata$sector_number)
    plotdata<-plotdata[select,]
}
if(restrictcategory!'"'){
    select <- grepl(restrictcategory,plotdata$category)
    plotdata<-plotdata[select,]
}


categorymatrix<-plotdata;


#First create data frame with AD
curunits<-as.vector(unique(subset(categorymatrix,select=unit))[,1])
curparts<-as.vector(unique(subset(categorymatrix,select=party))[,1])
# Analyse available measures
# List of measures which are not numerical
curmeasu<-as.vector(unique(subset(categorymatrix,select=meastype))[,1])
curuids<-as.vector(unique(subset(categorymatrix,select=variableUID))[,1])
curmeasuid<-as.vector(unique(subset(categorymatrix,select=c(variableUID,meastype))))


#tmp1<-subset(categorymatrix,select=-c(gas,unit,party,meastype,allmethods,variableUID,sector_number))
#years<-as.numeric(gsub("X","",gsub("X","",colnames(tmp1))))
#nyears<-length(years)

