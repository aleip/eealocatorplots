makeemplot<-function(curuid,fdata,runfocus="value",rundata="adem",eukp,plotparamcheck=1,imeas=""){
    
    plotcoun<-as.vector(unlist((fdata$party[fdata[,"variableUID"]==curuid & !(fdata$party %in% eucountries)])))
    
    # Extract data: re-calculate sums for ADEM, use available EU-28 data for IEF ####
    if(rundata=="adem"){
        plotmatr<-as.data.frame(extractuiddata(DF = fdata[!fdata$party %in% eucountries,],uid = curuid,c = countries,narm = FALSE))
        eu28<-colSums(extractuiddata(DF = fdata[fdata$party=="EU28",],uid = curuid,c = countries,narm = FALSE),na.rm=TRUE)
    }else if(grepl("ief",rundata)){
        plotmatr<-as.data.frame(extractuiddata(DF = fdata,uid = curuid,c = countries,narm = FALSE))
        eu28<-plotmatr[nrow(plotmatr),]
        plotmatr<-plotmatr[1:(nrow(plotmatr)-1),]
    }
    #print(curuid)
    #print(plotcoun)
    
    # Negative and positive values: note this has different meaning for ADEM plots vs. IEF plots ####
    #  - ADEM: time series of sum of MS with positive/negative data
    #  - IEF: maximum/minimum values over the time series
    if(rundata=="adem"){
        temp<-plotmatr
        temp[temp<=0]<-NA
        eu28pos<-colSums(temp,na.rm=T)
        eu28pos[eu28pos==0]<-NA
        
        temp<-plotmatr
        temp[temp>=0]<-NA
        eu28neg<-colSums(temp,na.rm=T)
        eu28neg[eu28neg==0]<-NA
        
        # Relative value in country compared to EU value for all years
        # This is different from trend-plots!!!
        rel<-abs(plotmatr[,years])
        # Relative value in country compared to EU value averaged over all years
        relav<-rowMeans(rel,na.rm=TRUE)
        relav<-relav[!is.na(relav)]
        relav<-relav[!relav==0]
        
        plotmatr$party<-allcountries
        topn<-min(10,length(relav))
        topno<-max(0,length(relav)-topn)
        
        # Select the top countries with highest mean value over all years
        topneu28<-row.names(as.matrix(head(relav[order(relav,decreasing=T,na.last=T)],topn)))
        topother<-row.names(as.matrix(tail(relav[order(relav,decreasing=T,na.last=T)],topno)))
        
        eu28main<-plotmatr[row.names(plotmatr) %in% topneu28,]
        eu28main<-eu28main[order(rowSums(eu28main[,years],na.rm=TRUE),decreasing=FALSE),]
        Other<-as.data.frame(t(colSums(plotmatr[row.names(plotmatr) %in% topother,years],na.rm=TRUE)))
        Other$party<-"Other"
        topnnames<-eu28main$party
        
        if(length(relav)>length(topneu28)){eu28fin<-rbind(eu28main,Other)}else{eu28fin<-eu28main}
        finnames<-eu28fin$party
        eu28fin<-as.matrix(eu28fin[,years])
        finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28)*100
        finshares<-eu28fin[,years[length(years)]]/eu28[years[length(years)]]*100
        finshares[is.na(finshares)]<-0
        
        textorderadem1<-paste0("Countries are sorted by the average contribution to the sum of ",eukp," value over the the whole time period. ")
        if(topno>0) {
            textorderadem2<-paste0("The top ",topn," countries are displayed. ")
            textorderadem3<-paste0("The other ",topno," reporting countries with data are lumped to 'other'.")
        }else{
            textorderadem2<-paste0("The ",topn," reporting countries are displayed. ")
            textorderadem3<-paste0("")
        }
        textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
        
        runfocus<-"value"
        
    }else if(grepl("ief",rundata)){
        eu28pos<-max(plotmatr,na.rm=T)
        eu28neg<-min(plotmatr,na.rm=T)
        
        rel<-Reduce(rbind,apply(plotmatr,1,"/",eu28))
        #Use absolute relative deviation as sorting criterium
        #Store the maximum absolute relative deviation
        relabs<-abs(1-rel)
        #relav<-rowMeans(relabs,na.rm=T)
        relav<-apply(relabs,1,mean,na.rm=T)
        relav[is.infinite(relav)]<-NA
        relav[is.nan(relav)]<-NA
        
        # If there is no EU-value go on with relative value
        if(is.na(sum(relav,na.rm=TRUE))){ 
            eu28<-apply(plotmatr,2,mean,na.rm=T)
            relav<-apply(plotmatr,1,mean,na.rm=T)
            eukp<-paste0(eukp,"*")
        }
        #Keep only those values to plot which are not overlaying the boxplot
        #Attention: the endrange might make 'disappear' points that would be expected...
        endrange<-0.1
        lowerend<-apply(plotmatr,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
        upperend<-apply(plotmatr,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
        lowerok<-(plotmatr<lowerend)
        upperok<-(plotmatr>upperend)
        relna<-lowerok+upperok
        relplot=plotmatr*relna
        relplot[relplot==0]<-NA
        
        
        #plotmatr$party<-allcountries[!allcountries%in%eucountries]
        plotmatr$party<-allcountries[!allcountries%in%eucountries]
        plotmatr$relav<-relav
        plotmatr<-plotmatr[!is.na(relav),]
        plotmatr<-plotmatr[order(plotmatr$relav,decreasing=T),]
        #relav<-relav[!is.na(relav)]
        nexist<-length(relav[!is.na(relav)])
        topn<-min(10,nexist)
        topno<-max(0,nexist-topn)
        
        # Select the top countries with highest mean value over all years
        eu28main<-plotmatr[1:topn,c(years,"party")]
        topnnames<-eu28main$party
        if(topno>0){
            Other<-as.data.frame(t(unlist(apply(plotmatr[(topn+1):(topn+topno),years],2,mean))))
            Other$party<-"Other"
            eu28fin<-rbind(eu28main,Other)
        }else{
            eu28fin<-eu28main
        }
        ncountries<-nrow(eu28fin)
        finnames<-eu28fin$party
        eu28fin<-as.matrix(eu28fin[,years])
        
        #finshares<-rowMeans(eu28fin,na.rm=T)/apply(eu28,1,mean)*100
        finshares<-eu28fin[,length(years)]
        finshares[is.na(finshares)]<-0
        
        runfocus<-"value"
        if(runfocus=="value"){
            textiefval1<-paste0(eukp," value is obtained from a weighted average of country-values. ")
            textiefval2<-"The relative distance from MS/EU28 value is calculated for each year (e.g. 10% smaller). "
            textiefval3<-"Countries are sorted by average absolute relative distance calculated over the whole time period. "
        }
        if(topno>0){
            textiefval4<-paste0("The top ",topn," countries are displayed. ")
            textiefval5<-paste0("The other ",topno," countries with data are averaged to 'other'.")
        }else{
            textiefval4<-paste0("The ",topn," reporting countries are displayed. ")
            textiefval5<-paste0("")
        }
        textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
        
    }
    
    
    if(!(sum(eu28fin,na.rm=TRUE)==0)) {
        plotnow(curuid,fdata,eu28fin,finnames,finshares,eu28,eu28pos,eu28neg,
                runfocus,rundata,eukp,
                plotparamcheck,sharesexist=0,textorder,imeas)
        #source("eugirp_nirplots.r")
        #nirplotsdone<-nirplots(eu28fin,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }
}