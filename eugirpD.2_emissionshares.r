# Filter emissions and ADs and IEFs
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

# Calculate shares for EM trends based on ADs and IEFs
emissiontrendshares<-function(DF,emuid,iefuid,aduid,nyears,countries){
    
    
    # From run_emi_EU15_data.sh
    # c:\adrian\data\inventories\ghg\unfccc\inventorysystem\eea_locator_tool\plots\scripts\run_emi_EU15_data.sh
    #  pct_act = sprintf("%.0f",100 * ( ACT[country,endy] * ( vstarty / ACT[country,starty] ) - vstarty ) / ( vendy - vstarty ) )
    ### pct_act1<-100*(actcount[,nyears]*(sum(eealocator[,1])/actcount[,1])-sum(eealocator[,1]))/(sum(eealocator[,nyears])-sum(eealocator[,1]))
    
    #print(emuid)
    #print(aduid)
    #print(iefuid)
    
    curcount<-allcountries[!allcountries%in%eucountries]
    actcount<-extractuiddata(DF,uid = aduid,c=curcount)
    eealocator<-extractuiddata(DF,uid = emuid,c=curcount )
    #iefcount<-extractuiddata(DF,uid = iefuid,c=curcount )
    iefcount<-eealocator/actcount
    
    
    # Alternative:
    # DeltaE = En - Ea = ADn * IEFn - ADa * IEFa
    
    # DeltaE* = (ADn - ADa) * IEFa
    # DeltaE& = (IEFn - IEFa) * ADn
    # DeltaE* + DeltaE& = DeltaE
    
    # Share AD: DeltaE*/DeltaE
    
    acteu28<-extractuiddata(DF,uid = aduid,c="EU28")
    emieu28<-extractuiddata(DF,uid = emuid,c="EU28" )
    #iefeu28<-extractuiddata(DF,uid = iefuid,c="EU28" )
    iefeu28<-emieu28/acteu28
    
    deltaEM_EU28<-sum(eealocator[,nyears])-sum(eealocator[,1])
    act_pctcouneu<-100*((acteu28[nyears]-acteu28[1])*iefeu28[1])/deltaEM_EU28
    ief_pctcouneu<-100*((iefeu28[nyears]-iefeu28[1])*acteu28[nyears])/deltaEM_EU28
    
    # Share of AD and IEF on country absolute trend
    deltaEM<-eealocator[,nyears]-eealocator[,1]
    deltaAD<-actcount[,nyears]-actcount[,1]
    deltaIF<-iefcount[,nyears]-iefcount[,1]
    act_pctcountry<-100*(deltaAD*iefcount[,1])/deltaEM
    #ief_pctcountry<-100*(deltaIF*actcount[,nyears])/deltaEM
    ief_pctcountry<-100-act_pctcountry
    
    # Share of AD and IEF on EU absolute trend
    act_pcteu<-100*(deltaAD*iefcount[,1])/deltaEM_EU28
    ief_pcteu<-100*(deltaIF*actcount[,nyears])/deltaEM_EU28
    #ief_pcteu<-100-act_pcteu
    
    return(list(act_pcteu,ief_pcteu,
                act_pctcountry,ief_pctcountry,
                act_pctcouneu,ief_pctcouneu))
}



# activitydata<-measures2sum[measures2sum$variableUID%in%unlist(as.vector(emissions$ADuid)),]
# activitydata<-activitydata[order(activitydata$sector_number,activitydata$category),]
# emissions<-emissions[order(emissions$sector_number,emissions$category),]
# emissionshares<-c("act_pcteu","ief_pcteu","act_pctcountry","ief_pctcountry","act_pctcouneu","ief_pctcouneu")
