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
    curdiff<-curdiff[,names(curdiff[names(curdiff)!=noyears])]
    #View(curdiff)
    return(curdiff)
}
eutotdiff<-function(curdiff){
    
    eutot<-as.numeric(curdiff[curdiff$party=="EUC",faoyears])
    #print(eutot)
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
    
    avallrel<-rounddigit(100*mean(eutot,na.rm=TRUE)/mean(as.numeric(filter(curplotdata,party=="EUC")[,faoyears])))
    
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
    
        text1<- paste0(text1,"are both smaller and larger than NIR data along the time series, with ",
                      npos," years showing values that are larger in NIR and ",
                      nneg, " years when FAO data are larger. ")
        text1<-paste0(text1,"Mean difference in ",firstlow(curcatmea)," for years where NIR is larger than FAO is ",avneg,diffunit,". ")
        text1<-paste0(text1,"Mean difference for years when NIR is smaller than FAO is -",avpos,diffunit,". ")
    }else if(npos==0){
        text1<-paste0(text1,"are smaller than NIR data for all years. ")
    }else if(nneg==0){
        text1<-paste0(text1,"are larger than NIR data for all years. ")
    }
    if(avall<0){
        text1<-paste0(text1,"Over all years, NIR is larger by ",avall,diffunit)
    }else{
        text1<-paste0(text1,"Over all years, NIR is smaller by ",abs(avall),diffunit)
    }
    text1<-paste0(text1," or ",avallrel,"% of the average value in the EU. ")
    
    text2<-paste0("The ",singular(n=nmaxdev,"countries",1)," with the largest differences in single years are ",
                  laender(nmaxparty),". ")
    text2<-paste0(text2,"The largest deviations are ",
                  rounddigit(maxdevi[1,2],1),diffunit," (",laender(maxdevi[1,1]),", ",maxdevi[1,3],"), ",
                  " corresponding to ",abs(avmax),"% of total EU ",firstlow(curcatmea)," in this year, ",
                  rounddigit(maxdevi[2,2],1),diffunit," (",laender(maxdevi[2,1]),", ",maxdevi[2,3],"), and ",
                  rounddigit(maxdevi[3,2],1),diffunit," (",laender(maxdevi[3,1]),", ",maxdevi[3,3],")."
                  )
    text<-paste0(text1,text2)
    return(text)
}
