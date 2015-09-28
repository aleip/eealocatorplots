writeissue<-function(D,line){
    print(line)
    
    observation<-paste0("#Observation: ",line$party,": ", 
                        line$measure," in sector",
                        line$sector_number," has been identified as an outlier ")
    if(line$years=="all"){observation<-paste0(observation," for all years.")}else{
        observation<-paste0(observation," for the following years: ",paste(line$years,collapse=""))}
    question<-paste0("#Question: The value of the outlier is with ", line$value," (mean of outlier years) ",
                     round(line$value/line$media,1), " times the median value, calculated on the basis of values reported from all countries, and ")
    if(line$value>line$ulim)question<-paste0(question,round(line$value/line$ulim,1)," times the calculated upper limit.")
    if(line$value<line$llim)question<-paste0(question,round(line$value/line$llim,1)," times the calculated lower limit.")
    if(line$correction==0){
        question<-paste0(question,"\n# We judge the value a serious outlier which is likely to be a mistake!")
    }else{
        question<-paste0(question,"\n# Please provide a justification for this outlier (or correct).")
    }
    question<-paste0(question,"\n# Further information and explanations and comparison with other countries' submissions see in the attached file.")
    
    con <- file(paste0(issuedir,"countryoutliers/countryoutlier",line$party,line$sector_number,line$meastype,".csv"), open="wt")
    writeLines(observation,con)
    writeLines(question,con)
    writeLines("#",con)
    writeLines("#",con)
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)writeLines(outlmethod2,con)
    if(trendoutlmethod==3)writeLines(outlmethod3,con)
    writeLines(coutlexp2, con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(paste0("\n#Note for column: correction: ",
               "\n#0: value assumed to be a mistake - it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification.",
               "\n# 1: value is assumed to be intended and requires justification"),con)
    write.csv(line,con)
    writeLines(paste0("\n\n# For comparison: other MS values for sector",line$sector_number,
                      "- category ",line$category,
                      "- measure ",line$meastype,"#\n#"),con)
    m<-subset(D,meastype==line$meastype & sector_number==line$sector_number & category==line$category)
    write.csv(m,con)
    close(con)
    return(1)
}

selectpop<-function(P,line){
    
    sect<-P$sector_number[line]
    cate<-P$category[line]
    part<-P$party[line]
    pop<-P[P$sector_number==sect & P $category==cate & P$party==part & P$meastype =="POP",years]
    #print(line)
    if(nrow(pop)!=1){
        stop()
    }
    return(pop)
}

writecorrection<-function(v,P,mult,name){
    if(nrow(v)>0) {
        vu<-unique(v[,1])
        before<-P[vu,]
        meas<-unique(P$measure[vu])
        unit<-unique(P$unit[vu])
        vp<-subset(before,select=c("sector_number","category","party"))
        vpp<-unique(vp$party)
        #psect<-unlist(lapply(vp$party, function(x) 
        #    unlist(lapply(c(1:sum(vp$party==x)),function(y)
        #    paste0(vp$sector_number[x][y]," ",vp$category[x][y])))))
        if(unit=="") unit<-"-"
        if(mult=="POP"){
            #Divide by population number
            #View(P[vu,years])
            pop<-Reduce(rbind,(lapply(c(1:length(vu)),function(x) selectpop(P,vu[x]))))
            P[vu,years]<-P[vu,years]/pop
            select<-unique(which(P[vu,years]>1000,arr.ind = TRUE)[,1])
            P[vu[select],years]<-P[vu[select],years]/1000
        }else{
            P[vu,years]<-t(Reduce(cbind,lapply(c(1:length(vu)),function(x) t(mult*P[vu[x],years]))))
        }
        after<-P[vu,]
        before$corr<-0
        after$corr<-1
        c<-paste(as.vector(unique(before$party)),collapse="-")
        observation<-unique(paste0(P$party[vu],": "))
        #observation<-paste0(observation,psect)
        observation<-paste0(observation," Values for ",meas," seem to be inconsistent with the unit (",unit,") requested.")
        observation<-paste0(observation," Question: please provide justification for the values used or otherwise correct the values used.")
        observation<-paste0(observation," Suggested corrected values and concerned source categories are found below.")
        filnam<-paste0("corrections_",name,"_",c,".csv")
        filnamt<-paste0(issuedir,"/autocorrections/",filnam)
        con <- file(filnamt, open="wt")
        writeLines(paste0("# Observation: ",observation),con)
        writeLines("# Data before correction\n#",con)
        write.csv(before,con)
        writeLines("#\n#\n# Data after correction\n#",con)
        write.csv(after,con)
        close(con)
    }
    return(P)
}
writeautoc<-function(v,A,P,mult,fn){
    if(nrow(v)>0) {
        newA<-subset(paramdata[v[,1],],select=c("variableUID","party"),row.names=F)
        newA$autocorr<-mult
        newA[,"file"]<-paste0("=HYPERLINK(\"",fn,"\")")
        A<-rbind(A,unique(newA))
    }
    return(A)
}

