### remaining snippets from early version ####
if(runfocus==1){runfocus<-"value"}
if(runfocus==2){runfocus<-"trend"}
if(runfocus==4){runfocus<-"countries"}

print(paste("Current task ",numrun,". AD=",runad,". Par=",runpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))



#print("# FOR TEST PURPOSE: ADD RANDOM NUMBER TO INCREASE NUMBER OF COUNTRIES")
# addrandomcountries in case there is none ####
addrandomcountries<-0
if(searchline)print(625)
if(addrandomcountries==1){
    attributes<-as.matrix(read.table("eugirp_attributes.txt",header=T,row.names=1,check.names=F))
    allcountries<-row.names(attributes)
    allcountries<-allcountries[allcountries != "Other"]
    restcountries<-allcountries[! allcountries %in% curparts]
    #Add countries
    nnewc<-25
    newcn<-restcountries[sample(1:length(restcountries),nnewc,replace=FALSE)]
    #Multiplicate with random between 0.7 and 1.3 times values
    randommulp<-matrix(runif(nyears*nnewc,min=0.9,max=1.2),ncol=nyears)
    
    pick<-matrix(sample(0:1,nyears*length(curparts),replace=T),ncol=nyears)
    pick[2,]<-1-pick[1,]
    pick<-as.vector(t(colSums(eealocator*pick)))
    newcv<-randommulp*pick
    rownames(newcv)<-newcn
    colnames(newcv)<-years
    eealocator2<-rbind(eealocator,newcv)
    eealocator<-eealocator2
}

# B.3a-selectcountries-adem ####    
if(rundata=="adem"){
}

# B.3a-selectcountries-ief ####    
if(rundata=="ief"){
    
    eu28pos<-max(curmatrix,na.rm=T)
    eu28neg<-min(curmatrix,na.rm=T)
    
    eu28mean<-colMeans(curmatrix,na.rm=T)
    #if((runfocus=="value") & ("AD" %in% curmeasu)){
    if((runfocus=="value") & sum(acteu28>0)){
        #Some parameter are not reported from all countries
        eu28<-colSums(curmatrix[rownames(curmatrix) %in% rownames(actcount),]
                      *actcount[rownames(actcount) %in% rownames(curmatrix),])/
            colSums(actcount[rownames(actcount) %in% rownames(curmatrix),])
    }else{
        eu28<-eu28mean
    }
    
    #Calculate relative value
    if(runfocus=="value"){
        rel<-t(t(curmatrix)/eu28)
        #Use absolute relative deviation as sorting criterium
        #Store the maximum absolute relative deviation
        relabs<-abs(1-rel)
        relav<-rowMeans(relabs,na.rm=T)
        relav<-apply(relabs,1,max,na.rm=T)
        
    }
    if(runfocus=="countries"){
        #Relative value against the mean value used by MS over the years
        rel<-curmatrix/apply(curmatrix,1,mean,na.rm=T)
        #Largest range of values; in case of negative values rel calculate distance vs max value
        relabs<-((apply(curmatrix,1,max,na.rm=T)-apply(curmatrix,1,min,na.rm=T))
                 /apply(curmatrix,1,max,na.rm=T)
        )
        relav<-relabs
        
    }
    if(runfocus=="trend"){
        rel<-eeatrend
        #Use absolute relative deviation as sorting criterium
        #Store the maximum absolute relative deviation
        relabs<-(abs(rel))
        relav<-rowMeans(relabs,na.rm=T)
        relav<-apply(relabs,1,max,na.rm=T)
        relav<-relav[!is.na(relav)]
    }
    
    #Keep only those values to plot which are not overlaying the boxplot
    #Attention: the endrange might make 'disappear' points that would be expected...
    endrange<-0.1
    
    lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
    upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
    lowerok<-(curmatrix<lowerend)
    upperok<-(curmatrix>upperend)
    relna<-lowerok+upperok
    
    
    #20150726 - commented keep for a while...
    #Trend from time series (do not use the mean trend)
    #eu28.trendmean<-colMeans(eeatrend,na.rm=T)
    #eu28.trend<-eu28[2:nyears]/eu28[1:(nyears-1)]
    #rel.trend<-t(t(eeatrend)/eu28.trend)
    #relav.trend<-rowMeans(rel.trend,na.rm=T)
    
    relplot=curmatrix*relna
    
    if(runfocus=="value"){
        relplot[relplot==0]<-NA
        #Criterion for selecting country: largest average deviation from EU average
        # --nr.rm=F keeps only those countries which have large deviations for the whole time series
        #relav<-rowMeans(relplot,na.rm=F)
    }
    
    if(runfocus=="trend"){
        #Criterion for selecting country: largest internnual change in timeseries
        #relav<-apply(relplot,1,max)
        relplot[relplot==0]<-NA
    }  
    
    #---> first determine number of non-NA elements
    topn<-min(10,length(sort(relav,decreasing=F)))
    topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
    topneu28<-sort(topneu28,decreasing=F)
    topother<-tail(relav[order(relav,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
    
    topn<-length(topneu28)
    topno<-max(0,nrow(curmatrix)-topn)
    
    if(runfocus=="value"){
        textiefval1<-"EU28+IC value is obtained from a weighted average of country-values. "
        textiefval2<-"The relative distance from MS/EU28 value is calculated for each year (e.g. 10% smaller). "
        textiefval3<-"Countries are sorted by average relative distance calculated over the whole time period. "
        textiefval4<-paste0("The top ",topn," countries are displayed. ")
        textiefval5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
        textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
    }
    if(runfocus=="countries"){
        textiefcnt3<-"Countries are sorted by their relative range of IEFs over the whole time period. "
        textiefcnt4<-paste0("The top ",topn," countries are displayed. ")
        textiefcnt5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
        textorder<-paste0(textiefcnt3,textiefcnt4,textiefcnt5)
    }
    if(runfocus=="trend"){
        textorderadem1<-"Countries are sorted by the average growth rate over the whole time period. "
        textorderadem2<-paste0("The top ",topn," countries are displayed. ")
        textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
        textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
    }
    
}

#print("Determine the the top n countries contributing on average most to EU28 values")
if(searchline)print(931)
topnnames<-names(topneu28)
toponames<-names(topother)
ncountries<-min(topn,length(topnnames))+min(1,length(toponames))

#print("Extract top n countries from dataset and group other together for plotting")
eu28main<-curmatrix[topnnames,]
eu28rest<-curmatrix[toponames,]
if(rundata=="ief" && runfocus=="trend"){
    eu28main<-relplot[topnnames,]
    eu28rest<-relplot[toponames,]
}
if(rundata=="adem"){
    Other<-colSums(eu28rest,na.rm=T)
}else{
    Other<-colMeans(eu28rest,na.rm=T)
    if((runfocus=="value") & sum(acteu28>0)){
        #Some parameter are not reported from all countries
        Other<-colSums(eu28rest*actcount[rownames(eu28rest),])/
            colSums(actcount[rownames(eu28rest),])
    }
}

#print("# ---> combine Main countries with the 'other' countries")
if(length(toponames)>0){
    eu28fin<-rbind(eu28main,Other)
}else{
    eu28fin<-eu28main
    #20150727 commented because the '1' for 'other' is already excluded (line 915)
    #ncountries<-ncountries-1
}
if(length(toponames)>0){
    finnames<-c(row.names(eu28fin)[1:(ncountries-1)],"Other")
}else{
    finnames<-row.names(eu28fin)
}
eu28fin<-as.matrix(eu28fin)

#rownames(eu28fin)<-finnames
finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28)*100
finshares[is.na(finshares)]<-0

if(runfocus=="trend"){
    # Calculate the finshares from trend over total time period
    
}
