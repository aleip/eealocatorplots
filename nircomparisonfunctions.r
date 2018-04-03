checkcurems<-function(line){
    sgas<-line$Gas
    scat<-line$`Source category`
    scat<-gsub(paste0(sgas," Emissions"),"",scat)
    scat<-gsub("- $","",scat)
    if(grepl("Indirect $",scat)){sgas<-paste0("indirect ",sgas);scat<-gsub("- Indirect $","",scat)}
    
    text<-paste0(sgas," emissions from category ",scat)
    return(text)
}
checkcurdif<-function(check="diff",n=1){
    if(check=="diff"){line<-curdiffs[n,]}else if(check=="reldiff"){line<-curdiffr[n,]}
    if(check=="diff"){unit<-"kt CO2-eq yr-1"}else if(check=="reldiff"){unit<-"%"}
    text<-paste0(checkcurems(line)," (",line[,check]," ",unit,", ")
    text<-paste0(text,"with larger emissions reported by ",if(line[,check]>0){"NIR"}else{"FAO"},")")
    return(text)
}
makecurdiff<-function(curmeastype,curcat,cursec,curgas="no gas"){
    sel<-plotdata$meastype==curmeastype
    sel<-sel&plotdata$category==curcat
    sel<-sel&grepl(cursec,plotdata$sector_number)
    sel<-sel&plotdata$gas==curgas
    
    curplotdata<-plotdata[sel,]
    if(grepl("3.C",cursec)) curplotdata<-filter(curplotdata,measure=="Harvested area"&classification=="Rice Cultivation")
    #View(curplotdata)
    curparties<-unique(curplotdata$party)
    curdiff<-curplotdata[curplotdata$datasource=="nir",]
    curdiff[,faoyears]<-t(sapply(1:nrow(curdiff),function(x) {
        lines<-curplotdata[curplotdata$party==curdiff$party[x],faoyears]
        nlines<-nrow(lines)
        if(nlines==2){
            curv<-as.vector(apply(curplotdata[curplotdata$party==curdiff$party[x],faoyears],2,diff,na.rm=TRUE))
        }else{
            curv<-rep(NA,length(faoyears))
        }
        #print(curv)
        return(curv)
    }))
    noyears<-years[!years%in%faoyears]
    #xavi20180327: curdiff<-curdiff[,names(curdiff[names(curdiff)!=noyears])]
    curdiff<-curdiff[,names(curdiff[!names(curdiff) %in% noyears])]
    #View(curdiff)
    return(curdiff)
}
makecurrel<-function(curmeastype,curcat,cursec,curgas="no gas"){
    #This is not yet used, but could be interesting to see if differences
    #are consistent or if there are abrupt changes.
    #
    sel<-plotdata$meastype==curmeastype
    sel<-sel&plotdata$category==curcat
    sel<-sel&grepl(cursec,plotdata$sector_number)
    sel<-sel&plotdata$gas==curgas
    
    curplotdata<-plotdata[sel,]
    if(grepl("3.C",cursec)) curplotdata<-filter(curplotdata,measure=="Harvested area"&classification=="Rice Cultivation")
    #make sure the FAO is first
    curplotdata<-curplotdata[order(curplotdata$datasource),]
    #View(curplotdata)
    curparties<-unique(curplotdata$party)
    curdiff<-curplotdata[curplotdata$datasource=="nir",]
    curdiff[,faoyears]<-t(sapply(1:nrow(curdiff),function(x) {
        lines<-curplotdata[curplotdata$party==curdiff$party[x],faoyears]
        nlines<-nrow(lines)
        if(nlines==2){
            curv<-as.vector(apply(curplotdata[curplotdata$party==curdiff$party[x],faoyears],2,function(x) x[1]/x[2]))
        }else{
            curv<-rep(NA,length(faoyears))
        }
        #print(curv)
        return(curv)
    }))
    noyears<-years[!years%in%faoyears]
    curdiff<-curdiff[,names(curdiff[names(curdiff)!=noyears])]
    #View(curdiff)
    return(curdiff)
}

rankalldiffs<-function(curmeastype,cursec,curgas="no gas"){
    
    sel<-plotdata$meastype==curmeastype
    sel<-sel&grepl(cursec,plotdata$sector_number)
    sel<-sel&plotdata$gas==curgas
    curplotdata<-plotdata[sel,]
    
    curcats<-c(livestock,"Poultry","Other Livestock")
    curcats<-curcats[!curcats=="Cattle"]
    doforcat<-function(x){
        curdiff<-makecurdiff(curmeastype,curcats[x],cursec,curgas="no gas")
        curdiff<-as.data.frame(curdiff[curdiff$party!="EUC",c("party","category",faoyears)])
        return(curdiff)
    }
    
    alldiffs<-Reduce(rbind,lapply(1:length(curcats),function(x) doforcat(x)))
    alldiffs$mean<-apply(alldiffs[,faoyears],1,mean,na.rm=TRUE)
    curplotdata$mean<-apply(curplotdata[,faoyears],1,mean,na.rm=TRUE)
    alldiffs<-melt(alldiffs,measure.vars=c(faoyears,"mean"))
    names(alldiffs)<-c("party","category","year","diff")
    
    alldiffs$nir<-sapply(1:nrow(alldiffs),function(x) 
        curplotdata[curplotdata$party==alldiffs$party[x]&
                        curplotdata$category==alldiffs$category[x]&
                        curplotdata$datasource=="nir",which(names(curplotdata)==alldiffs$year[x])])

    alldiffs$nireu<-sapply(1:nrow(alldiffs),function(x) 
        curplotdata[curplotdata$party=="EUC"&
                        curplotdata$category==alldiffs$category[x]&
                        curplotdata$datasource=="nir",which(names(curplotdata)==alldiffs$year[x])])
    alldiffs$rel<-alldiffs$diff/alldiffs$nir
    alldiffs$releu<-alldiffs$diff/alldiffs$nireu
    alldiffs$releutot<-sapply(1:nrow(alldiffs),function(x) 
        alldiffs$diff[x]/sum(curplotdata[curplotdata$party=="EUC"&
                        curplotdata$datasource=="nir",
                        which(names(curplotdata)==alldiffs$year[x])],na.rm=TRUE)
        )

    #Convert to percent
    alldiffs$rel<-round(100*alldiffs$rel,1)
    alldiffs$releu<-round(100*alldiffs$releu,1)
    alldiffs$releutot<-round(100*alldiffs$releutot,3)
    
    return(alldiffs)
}

allcompar<-function(curmeastype,cursec,curgas="no gas"){
    sel<-plotdata$meastype==curmeastype
    sel<-sel&grepl(cursec,plotdata$sector_number)
    sel<-sel&plotdata$gas==curgas
    
    curcats<-unique(plotdata$category[sel])
    
    eutot<-t(sapply(1:length(curcats),function(x) {
        curdiff<-makecurdiff(curmeastype,curcats[x],cursec,curgas="no gas")
        eutot<-as.numeric(curdiff[curdiff$party=="EUC",faoyears])
        return(eutot)
    }))
    eutot[is.na(eutot)]<-0
    avall<-apply(eutot,1,function(x) rounddigit(mean(x,na.rm=TRUE),1))
    
    curplotdata<-filter(plotdata,meastype==curmeastype&grepl(cursec,sector_number)&datasource=="nir"&gas==curgas&plotdata$party=="EUC")
    curplotdata<-curplotdata[match(curcats,curplotdata$category),c("category",faoyears)]
    avallrel<-curplotdata
    avallrel[,faoyears]<-round(100*eutot/curplotdata[,faoyears],1)
    avallrel$mean<-round(apply(avallrel[,faoyears],1,mean),1)
    return(avallrel)
}

eutotdiff<-function(curdiff){
    
    eutot<-as.numeric(curdiff[curdiff$party=="EUC",faoyears])
    #Note: the difference calculates FAO-NIR, thus
    #NEGATIVE values indicate that FAO is smaller
    #POSITIVE values indicate that FAO is larger
    
    npos<-sum(eutot>0)
    nneg<-sum(eutot<0)
    avall<-rounddigit(mean(eutot,na.rm=TRUE),1)
    diffunit<-paste0(" ",unique(curdiff$unit))
    diffunit<-gsub("1000s","thousand heads",diffunit)
    diffunit<-gsub("10\\^9m\\^2\\/year","thousand km^2^ year^-1^",diffunit)
    curcat<-unique(curdiff$category)
    
    curmea<-unique(curdiff$measure)
    curmea<-gsub("excreted","excretion",curmea)
    curmst<-unique(curdiff$meastype)
    cursec<-unique(curdiff$sector_number)
    curgas<-unique(curdiff$gas)
    
    curplotdata<-filter(plotdata,meastype==curmst&category==curcat&sector_number==cursec&datasource=="nir"&gas==curgas)
    if(grepl("3.C",cursec)) curplotdata<-filter(curplotdata,measure=="Harvested area"&classification=="Rice Cultivation")
    if(curcat=="Farming") curcat<-unique(curdiff$classification)
    curcatmea<-paste0(curcat," ",firstlow(curmea))
    if(cursec=="3.D.1.6")curcatmea<-curmea
    curcatmea<-gsub("Direct N2O Emissions From Managed Soils N","Nitrogen",curcatmea)
    curcatmea<-gsub("Crop Residues N","Nitrogen",curcatmea)
    curcatmea<-gsub("organic N fertilizers","organic nitrogen fertilisers",curcatmea)
    print(curcatmea)
    curcatmea<-gsub("Rice [cC]ultivation","Rice",curcatmea)
    
    allrel<-rounddigit(100*eutot/as.numeric(filter(curplotdata,party=="EUC")[,faoyears]))
    avallrel<-rounddigit(100*mean(eutot,na.rm=TRUE)/mean(as.numeric(filter(curplotdata,party=="EUC")[,faoyears])))
    
    diffmin<-floor(10*min(allrel))/10
    diffmax<-floor(10*max(allrel))/10
    textdiff<-paste0("Differences are in the range between ",diffmin,"% and ",diffmax,"%. ")
    
    
    checkdiff<-curdiff[curdiff$party!="EUC",faoyears]
    tmp<-abs(as.numeric(as.matrix(checkdiff)))
    
    maxdevi<-as.data.frame(matrix(NA,nrow=3,ncol=3))
    nmaxdev<-0
    topn<-0
    #while(nmaxdev<3 & topn<nrow(checkdiff)){
    while(nmaxdev<3){
        topn<-topn+1
        tmp1<-tmp[which(rank(-tmp)==topn)]
        if(length(tmp1)==0) break
        maxdev<-which(abs(checkdiff)>=tmp1,arr.ind = TRUE)
        nmaxdev<-length(unique(maxdev[,1]))
        maxdev1<-which(abs(checkdiff)==tmp1,arr.ind = TRUE)
        maxdevi[topn,1]<-curdiff$party[maxdev1[,1]]
        maxdevi[topn,2]<-checkdiff[maxdev1[,1],maxdev1[,2]]
        maxdevi[topn,3]<-faoyears[maxdev1[,2]]
    }
    nmaxparty<-unique(curdiff$party[maxdev[,1]])
    avmax<-rounddigit(100*maxdevi[1,2]/filter(curplotdata,party=="EUC")[,maxdevi[1,3]],1)
    #print(filter(curplotdata,party=="EUC"))
    text1<-paste0(firstup(curcatmea)," data from FAO ")
    if(npos>0&nneg>0){
        avpos<-rounddigit(mean(eutot[eutot>0]),1)
        avneg<-abs(round(mean(eutot[eutot<0]),1))
    
        text1<- paste0(text1,"are sometimes smaller","and sometimes larger than NIR data. ",
                       textdiff,
                      nneg," years showing values that are larger in NIR  (on average by ",avneg,diffunit,") and ",
                      npos, " years when FAO data are larger  (on average by ",avpos,diffunit,"). ")
        if(cursec=="3.C")text1<-paste0(text1,"Nevertheless, the data show very similar trends for both datasets. ")
        #text1<-paste0(text1,"Mean difference in ",firstlow(curcatmea)," for years where NIR is larger than FAO is ",avneg,diffunit,". ")
        #text1<-paste0(text1,"Mean difference for years when NIR is smaller than FAO is -",avpos,diffunit,". ")
    }else if(npos==0){
        text1<-paste0(text1,"are smaller than NIR data for all years. ")
    }else if(nneg==0){
        text1<-paste0(text1,"are larger than NIR data for all years. ")
    }
    # avall is negative when FAO is smaller
    if(avall<0){
        text1<-paste0(text1,"Comparing all years, NIR is larger by ",abs(avall),diffunit)
    }else{
        text1<-paste0(text1,"Comparing all years, NIR is smaller by ",abs(avall),diffunit)
    }
    text1<-paste0(text1," or ",avallrel,"% of the average value in the EU. ")
    
    text2<-paste0("The ",singular(n=nmaxdev,"countries",1)," with the largest differences in single years are ",
                  laender(nmaxparty),". ")
    text2<-paste0(text2,"The largest deviations (FAO minus NIR) are ",
                  rounddigit(maxdevi[1,2],1),diffunit," (",laender(maxdevi[1,1]),", ",maxdevi[1,3],"), ",
                  " corresponding to ",abs(avmax),"% of total EU ",tolower(curcatmea)," in this year (NIR), ",
                  rounddigit(maxdevi[2,2],1),diffunit," (",laender(maxdevi[2,1]),", ",maxdevi[2,3],"), and ",
                  rounddigit(maxdevi[3,2],1),diffunit," (",laender(maxdevi[3,1]),", ",maxdevi[3,3],")."
                  )
    text<-paste0(text1,text2)
    return(text)
}
