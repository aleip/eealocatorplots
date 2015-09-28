simplifyunit<-function(u,fac){
    nu<-"not defined"
    if(fac==1){nu<-as.character(u)}else
    {
        if(u==""){nu<-""}
        if(u=="kg N/year"){if(fac==1E-6){nu<-"kt N/year"}else if(fac==1E-3){nu<-"t N/year"}}
        if(u=="kg N/yr"){if(fac==1E-6){nu<-"kt N/yr"}else if(fac==1E-3){nu<-"t N/yr"}}
        if(u=="kg/year"){if(fac==1E-6){nu<-"kt/year"}else if(fac==1E-3){nu<-"t/year"}}
        if(u=="kg/t"){if(fac==1E-6){nu<-"kt/t"}else if(fac==1E-3){nu<-"t/t"}}
        if(u=="kg dm"){if(fac==1E-6){nu<-"kt dm"}else if(fac==1E-3){nu<-"t dm"}}
        if(u=="ha/year"){if(fac==1E-6){nu<-"Mha/year"}else if(fac==1E-3){nu<-"kha/year"}}
        if(u=="Mg"){if(fac==1E-6){nu<-"Mt"}else if(fac==1E-3){nu<-"kt"}}
        if(u=="ha"){if(fac==1E-6){nu<-"Mio ha"}else if(fac==1E-3){nu<-"kha"}}
        if(u=="kha"){if(fac==1E-6){nu<-"Mio kha"}else if(fac==1E-3){nu<-"Mio ha"}}
        if(u=="m^3"){if(fac==1E-6){nu<-"Mio m^3"}else if(fac==1E-3){nu<-"1000 m^3"}}
        if(u=="metric t"){if(fac==1E-6){nu<-"Mio metric t"}else if(fac==1E-3){nu<-"1000 metric t"}}
        if(u=="t"){if(fac==1E-6){nu<-"Mt"}else if(fac==1E-3){nu<-"kt"}}
        if(u=="t/year"){if(fac==1E-6){nu<-"Mt/year"}else if(fac==1E-3){nu<-"kt/year"}}
        if(u=="kt"){if(fac==1E-6){nu<-"1000 Mt"}else if(fac==1E-3){nu<-"Mt"}}
        if(u=="kt C"){if(fac==1E-6){nu<-"1000 Mt C"}else if(fac==1E-3){nu<-"Mt C"}}
        if(u=="kt DC"){if(fac==1E-6){nu<-"1000 Mt DC"}else if(fac==1E-3){nu<-"Mt DC"}}
        if(u=="kt CO2 equivalent"){if(fac==1E-6){nu<-"1000 Mt CO2 equivalent"}else if(fac==1E-3){nu<-"Mt CO2 equivalent"}} 
        if(u=="TJ"){if(fac==1E-6){nu<-"1Mio TJ"}else if(fac==1E-3){nu<-"1000 TJ"}}
        if(u=="1000s"){if(fac==1E-6){nu<-"Mio 1000s"}else if(fac==1E-3){nu<-"Mio"}}
    }
    return(nu)
}
getmax<-function(DF,y,uid){maxuid<-max(DF[DF$variableUID==uid,years],na.rm=TRUE)}
l<-unlist(lapply(measures2sum$variableUID,function(x) getmax(eu28sum[grepl("^3",eu28sum$sector_number),], years,as.vector(x))))

changemio<-100000000
changeths<-100000

selection<- l>changemio 
units2change<-as.data.frame(measures2sum$variableUID[selection])
names(units2change)<-"variableUID"
units2change$unit<-measures2sum$unit[selection]
units2change$fact<-as.numeric(1/1000000)
units2change$fact[units2change$unit==""]<-1

selection<-l>changeths & l <= changemio
units2changet<-as.data.frame(measures2sum$variableUID[selection])
names(units2changet)<-"variableUID"
units2changet$unit<-measures2sum$unit[selection]
units2changet$fact<-as.numeric(1/1000)
units2changet$fact[units2changet$unit==""]<-1
units2change<-rbind(units2change,units2changet)
rm(units2changet)

units2change$newunit<-unlist(lapply(c(1:nrow(units2change)),function(x) 
    simplifyunit(units2change$unit[x],units2change$fact[x])))
missing<-length(unique(units2change$unit[units2change$newunit=="not defined"]))
if(missing>0) View(unique(units2change$unit[units2change$newunit=="not defined"]),"unitsmiss")

calceu<-rbind(alldata,eu28sum)

calceu<-merge(calceu,units2change,by=c("variableUID","unit"),all=TRUE)
selection<-!is.na(calceu$newunit)
levels(calceu$unit)<-c(levels(calceu$unit),unique(calceu$newunit))
calceu$unit[selection]<-calceu$newunit[selection]
selection<-!is.na(calceu$fact)
calceu[selection,years]<-calceu[selection,years]*calceu[selection,"fact"]
