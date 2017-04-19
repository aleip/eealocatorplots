makecurdiff<-function(curmeastype,curcat,cursec){
    sel<-plotdata$meastype==curmeastype
    sel<-sel&plotdata$category==curcat
    sel<-sel&grepl(cursec,plotdata$sector_number)
    
    curplotdata<-plotdata[sel,]
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
eutotdiff<-function(){
    
    eutot<-as.numeric(curdiff[curdiff$party=="EUC",faoyears])
    print(eutot)
    npos<-sum(eutot>0)
    nneg<-sum(eutot<0)
    avall<-round(mean(eutot,na.rm=TRUE),0)
    diffunit<-paste0(" ",unique(curdiff$unit))
    diffunit<-gsub("1000s","thousand heads",diffunit)
    
    if(npos>0&nneg>0){
        avpos<-round(mean(eutot[eutot>0]),0)
        avneg<-abs(round(mean(eutot[eutot<0]),0))
    
        
        text1<-paste0("Data from FAO are both smaller and larger than NIR data along the time series, ",
                      "with ",npos," years showing values that are larger in NIR and ",
                      nneg, " years where FAO data are larger. ")
        text1<-paste0(text1,"Mean difference for years where NIR>FAO is ",avpos,diffunit,". ")
        text1<-paste0(text1,"Mean difference for years where NIR<FAO is ",avneg,diffunit,". ")
    }else if(npos==0){
        text1<-paste0("Data from FAO are larger than NIR data for all years. ")
    }else if(nneg==0){
        text1<-paste0("Data from FAO are smaller than NIR data for all years. ")
    }
    if(avall>0){
        text1<-paste0(text1,"Over all years, NIR is larger by ",avall,diffunit,". ")
    }else{
        text1<-paste0(text1,"Over all years, NIR is smaller by ",abs(avall),diffunit,". ")
    }
    
    return(text1)
}


curmeastype<-"POP"
curcat<-"Dairy Cattle"
cursec<-"3.A"

curdiff<-makecurdiff(curmeastype,curcat,cursec)
eutotdiff()


