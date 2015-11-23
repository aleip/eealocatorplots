writeissuelist<-function(D,file){
    
    con <- file(file, open="wt")
    writeLines(coutheader, con)
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)coutlexp2<-paste0(outlmethod2,coutlexp2)
    if(trendoutlmethod==3)coutlexp2<-paste0(outlmethod3,coutlexp2)
    writeLines(coutlexp2, con)
    writeLines(signexp1, con)
    writeLines(paste0(colexpl1,lulimexpl), con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    writeLines("#\n#Note for column: correction: 0: value assumed to be a mistake. it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification. 1: value is assumed to be not an outlier despite the criteria (e.g. milk production). empty: to be clarified",con)
    write.csv(D,con)
    close(con)
    
}
writeissue<-function(D,line,listoftext){
    nottoplot<-c("issuenr","issueflag","issuedate","explanation","date","resolved","comment","followup","plot")
    writenames<-names(line[!names(line)%in%nottoplot])
    observation<-paste0("# Observation: ",line$party,": ",  
                        line$measure," in sector ",
                        line$sector_number," has been identified as an outlier ")
    if(line$years=="all"){observation<-paste0(observation," for all years.")}else{
        observation<-paste0(observation," for the following years: ",paste(line$years,collapse=""))
    }
    question<-paste0("# Question: The value of the outlier is with ", line$value," (mean of outlier years) ",
                     round(line$value/line$media,1), " times the median value - calculated on the basis of values reported from all countries - and ")
    if(line$value>line$ulim)question<-paste0(question,round(line$value/line$ulim,1)," times the calculated upper limit.")
    if(line$value<line$llim)question<-paste0(question,round(line$value/line$llim,1)," times the calculated lower limit.")
    note<-"#"
    if(line$resolved==4){question<-paste0(question,"\n# The issue is a follow-up of a previous issue flagged as unresolved: ",gsub(","," - ",line$followup))}
    communication<-"#"
    if(line$communication!=""){question<-paste0(question,"\n# ",line$communication)}
    
    if(line$significant %in% c("over","under")){
        pottext<-"\n# This issue is considered a potential significant issue. If you think that this issue is not a significant issue because it is below the significance threshold please provide evidence why you think that it is below the significance threshold."
        question<-paste0(question,pottext,"\n# In the year ",signyear," the value used was ",format(as.numeric(line[,"2013"]),digits=3)," - ",format(as.numeric(line$relmedian),digits=2)," times the median value reported from all countries over all years (",line$median,"). ",
                               "\n# The ",line$measure," has significant impact on emissions with a share on national total (",
                               signclass,") of ",format(as.numeric(line$share)*100,digits=2),"%.",
                               "\n# We estimated the potential effect at ",format(as.numeric(line$effect)*100,digits=2),"%. It is thus larger than the threshold of ",format(as.numeric(signthreshold)*100,digits=2),"% of the national total.")
    }else{
        if(line$correction==0){
            question<-paste0(question,"\n# We judge the value a serious outlier which is likely to be a mistake!")
        }else{
            question<-paste0(question,"\n# Please provide a justification for this outlier (or correct).")
        }
    }
    question<-paste0(question,"\n# Further information and explanations and comparison with other countries' submissions see file.")
    
    curpath<-paste0(issuedir,"countryoutliers/")
    line[,is.na(line)]<-""
    curmet<-paste0(brief(line$option),line$meastype,brief(line$gas),line$method,line$source,brief(line$target),brief(line$type))
    curid<-paste0(cursubm,"co",line$party,gsub("\\.","",brief(line$sector_number)),curmet)
    
    curfile<-paste0(curid,".csv")
    curissue<-paste0(curpath,curfile)
    con <- file(curissue, open="wt")
    writeLines(observation,con)
    writeLines(question,con)
    writeLines(note,con)
    writeLines("#",con)
    
    writeLines(coutheader, con)
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)coutlexp2<-paste0(outlmethod2,coutlexp2)
    if(trendoutlmethod==3)coutlexp2<-paste0(outlmethod3,coutlexp2)
    writeLines(coutlexp2, con)
    if(line$significant %in% c("over","under")){
        writeLines(paste0("#\n",signexp1), con)
    }
    writeLines(paste0(colexpl1,lulimexpl), con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(colexpl2, con)
    #writeLines(colexpl3, con)
    writeLines("#\n#Note for column: correction: 0: value assumed to be a mistake. it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification. 1: value is assumed to be not an outlier despite the criteria (e.g. milk production). empty: to be clarified",con)
    
    writeLines(paste0("#\n#Note for column: correction: ",
               "\n# 0: value assumed to be a mistake - it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification.",
               "\n# 1: value is assumed to be intended and requires justification"),con)
    line<-line[,writenames]
    writeLines(paste0("#==================================================================================================\n",
                      "# Reported data"),con)
    write.csv(line[,allfields[allfields%in%names(line)]],con)
    writeLines("#\n# Data related to the identification of outliers",con)
    write.csv(line[,names(line)[!names(line)%in%c(allfields,testfields)]],con)
    writeLines("#\n# Data related to the identification of potential significant issues",con)
    write.csv(line[,testfields],con)
    writeLines(paste0("\n\n# For comparison: other MS values for sector",line$sector_number,
                      "- category ",line$category,
                      "- measure ",line$meastype,"#\n#"),con)
    comparison<-subset(D,meastype==line$meastype & sector_number==line$sector_number & category==line$category)
    write.csv(comparison[,-which(names(comparison)=="notation")],con)
    close(con)
    con1 <- file(listoftext, open="at")
    writeLines(paste0(curid,",","=HYPERLINK(\"",curfile,"\")",",",
                      gsub("# ","",observation),",",
                      gsub("# ","",gsub("\n"," ",question))),con1)
    close(con1)
    
    return(curid)
}

selectpop<-function(P,line){
    
    sect<-P$sector_number[line]
    cate<-P$category[line]
    part<-P$party[line]
    pop<-P[P$sector_number==sect & P $category==cate & P$party==part & P$meastype =="POP",years]
    #print(line)
    if(nrow(pop)!=1){
        stop("There is not exactly one population line")
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

reportchecks1<-function(check,data,x){
    reportfields<-c("party","sector_number","category","measure","meastype","gas","unit",
                    "method","source","notation",
                    "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
                    "classification","target","type","option","variableUID")
    checkfields<-c("test","val1","val2","sec","cat","obs","ms","yr","fac","val")
    n<-nrow(check)
    
    
    #resolve countries
    c<-check$ms
    y<-check$yr
    s<-check$sec
    cat<-check$cat
    
    if(grepl("all",c) & grepl("all",y)){
        # If all countries and years are concerned: general problem, 
        # no individual issue needs to be written
        check$val<-paste0("all countries and years")
    }else{
        #if 'all' or 'all except' report all 
        #next line gave error only for CLIMA-check...
        #if(grepl("all",c)){c<-allcountries}else{c<-unlist(strsplit(c," "))}
        if(grepl("all",c)){c<-allcountries}else{c<-unlist(strsplit(as.character(c)," "))}
        
        #selection<-data$sector_number==s & data$category==cat
        cats<-gsub("_"," ",strsplit(cat,split = " ")[[1]])
        secs<-strsplit(s,split = " ")[[1]]
        selection<-data$sector_number%in%secs & data$category%in%cats

        selp<-as.vector(unique(unlist(data$party[selection])))
        selp<-selp[!selp%in%c]
        
        observation<-paste(check[,checkfields],collapse="-")
        question<-" Question: Please correct/provide the missing information in your next submission."
        issue<-gsub(" ","",check$test)
        if(check$test=="NexTOT"){
            issue<-paste("Issue: Test ",check$test," (check of total manure excreted per animal type versus the sum of manure excretion over the MMS). ")
            if(grepl("total",check$obs)) observation<-paste0("Observation: No total N excretion is reported for animal type ",check$cat)
            if(grepl("val1 ",check$obs)) observation<-paste0("Observation: Sum of manure excretion over the MMS is different from Total N excretion for animal type ",check$cat," (",check$obs," - ",check$fac,")")
            question<-paste0("Question: Please include values for total N excretion for all animal types in your next submission. See file for further information.")
            extraline<-"# Note: countries which do not report both 'Nitrogen excretion per MMS' and 'Total N excreted' are not identified"
            
            selection<-selection & data$gas=="no gas" & (data$meastype=="EM" | data$meastype=="NEXC") 
        }
        if(check$test=="NexRATE"){
            issue<-paste("Issue: Test ",check$test," (check of the sum of manure excreted over the MMS per animal type versus the N-excretion rate multiplied by the animal population (heads)). ")
            animaltypes<-"animal type "
            if(length(cats)>1) animaltypes<-"animal types "
            animaltypes<-paste0(animaltypes,paste(secs,cats,collapse=" and "))
            if(grepl("not reported",check$obs)) {
                observation<-paste0("Observation: No N excretion rate is reported for ",animaltypes)
                question<-paste0("Question: Please include values for the N excretion rate all animal types in your next submission. See file for further information.")
            }
            if(grepl("val1 is fac ",check$obs)) {
                observation<-paste0("Observation: Sum of manure excretion over the MMS is different from POP/1000 x N-rate for ",animaltypes," (",check$fac,")")
                question<-paste0("Question: Please justify the reason for the discrepancy and/or correct in your next submission. See file for further information.",
                                 "Please compare measures 'Total N excreted'-meastype 'EM' with meastype 'TNEXC2'! ")
            }
            selection<-(selection | 
                             (data$sector_number%in%gsub("3.B.2","3.A",secs) & data$category%in%cats)
                         ) & (data$meastype%in%c("POP","NRATE","TNEXC2","EM") & 
                                  (data$gas%in%c("no gas","")))
        }
        
        if(grepl("N in ",check$test)){
            selection<-selection
        }
        if(grepl("N2O-IEF in ",check$test)){
            selection<-(selection | (data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$meastype=="IEF"))
        }
        
        if(grepl("N2O-NIEF",check$test)){
            issue<-paste("Issue: Test ",check$test," (check the N2O-IEF calculated with two different methods (see below)). ")
            animaltypes<-"animal type "
            if(length(cats)>1) animaltypes<-"animal types "
            animaltypes<-paste0(animaltypes,paste(secs,cats,collapse=" and "))
            if(grepl("not reported",check$obs)) {
                observation<-paste0("Observation: Missing information to perform the test for ",animaltypes)
                question<-paste0("Question: Please complete the CRF table with the missing information.")
            }
            if(grepl("val1 is fac ",check$obs)) {
                observation<-paste0("Observation: The results for the N2O-IEF differ for ",animaltypes," (",check$fac,")")
                question<-paste0("Question: Please justify the reason for the discrepancy and/or correct in your next submission. See file for further information.",
                                 "Please compare the values reported under 'IEFN1' and 'IEFN2'! ")
            }

            
            directsys<-manureSystems[!manureSystems%in%c("Pasture  range and paddock","Burned for fuel or as waste")]
            selsys<-directsys[directsys%in%unique(unlist(data$source[selection]))]
            selection<-(selection  | 
                            (data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$meastype%in%c("IEF","EM","NEXC"))) &
                data$source %in% c(selsys,"") 
            
            extraline<-paste0("# Please compare IEFN1 with IEFN2.\n",
                              "# IEFN1 is calculated as the weighted average of the IEF for the MMS (IEF_MMS) and the N excreted to the MMS, referred to Total N excreted (IEFN1=SUM(IEF_MMS * NEXC_MMS)/TNECX\n",
                              "# IEFN2 is calculated as from total (direct) Emissions and Total N excreted. IEFN2=EM/TNEXC*28/44\n",
                              "# IEF_MMS is calculated from Total N handled per MMS and Direct N2O emissions per MMS (IEF_MMS=EM_MMS/N_MMS*28/44\n",
                              "# --> A difference in IEFN1 and IEFN2 indicates \n",
                              "#     - The use of IEF_MMS that are different for different animal types (e.g. different sub-systems, w/o natural crust etc. In this case please provide an explanation!!\n",
                              "#     - An error in the calculation. In this case please provide a correction.\n",
                              "#")
        }

        if(grepl("CLIMA",check$test)){
            issue<-paste("Issue: Test ",check$test," (check that the allocation over all climate regions and MMS (Tier 2) sums up to 100). ")
            animaltypes<-"animal type "
            if(length(cats)>1) animaltypes<-"animal types "
            animaltypes<-paste0(animaltypes,paste(secs,cats,collapse=" and "))
            
            observation<-paste0("Observation: The allocation over all climate regions and MMS (Tier 2) sums does not sum up to 100 for ",animaltypes," (",check$fac,")")
            question<-paste0("Question: Please correct in your next submission. See file for further information.",
                             "Note that the values need to be in percent (not fraction)! ")
            selection<-selection  & data$meastype=="CLIMA"
        }
        
        selw<-selection & (data$party %in% c)
        selc<-selection & (data$party %in% selp)
        checkw<-data[selw & data$party != "EU28",reportfields]
        checke<-data[selw & data$party == "EU28",reportfields]
        checkc<-data[selc,reportfields]
        checkw<-checkw[order(checkw$party,checkw$category,checkw$notation,checkw$measure,checkw$source),]
        checke<-checke[order(checke$party,checke$category,checke$notation,checke$measure,checke$source),]
        checkc<-checkc[order(checkc$party,checkc$category,checkc$notation,checkc$measure,checkc$source),]
        
        if(exists("checkw")>0){
            
            checkdir<-paste0("agricheck_",check$test)
            checkdit<-paste0(issuedir,checkdir)
            if (! file.exists(checkdit)){dir.create(file.path(checkdit),showWarnings=FALSE)}
            
            for(cc in c) {
                checkfile<-gsub(" ","",paste0(check$test,"_",check$val1,"_vs_",check$val2,"_",check$obs,s,cat,"_",x,".csv"))
                checkfile<-gsub(" ","",paste0(check$test,"_",paste(cc,collapse="_"),"_",s,cat,"_",check$obs,".csv"))
                if(check$test=="NexRATE") checkfile<-gsub(" ","",paste0(check$test,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                if(check$test=="N2O-NIEF") checkfile<-gsub(" ","",paste0(check$test,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                if(check$test=="CLIMA") checkfile<-gsub(" ","",paste0(check$test,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                    
                con <- file(paste0(checkdit,"/",checkfile), open="wt")
                writeLines(paste0("# ",issue), con)
                writeLines(paste0("# ",observation), con)
                writeLines(paste0("# ",question,"\n#"), con)
                if(exists("extraline")) writeLines(extraline, con)
                writeLines(paste0("#\n#"), con)
                writeLines(paste0(",",paste(names(checkw),collapse=",")),con)
                for(cc in c[!c=="EU28"]) {
                    if(check$test=="N2O-NIEF"){
                        selection<-data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$party==cc
                        writeLines("#\n# N in MMS and N2O emissions and IEFs per MMS",con)
                        writedata<-data[selection,reportfields]
                        o<-order(writedata$meastype,writedata$source)
                        writedata<-writedata[o,]
                        write.table(writedata,sep=",",col.names=FALSE,con)
                        writeLines("#",con)
                        
                    }
                    writeLines("# Relevant data for the identified animal types",con)
                    for(cat in cats){
                        curtab<-checkw[checkw$party==cc & checkw$category==cat,]
                        write.table(curtab,sep=",",col.names=FALSE,con)
                        writeLines("#",con)
                    }
                }
                if(check$test!="N2O-NIEF" & check$test!="CLIMA"){
                    writeLines("#\n# Comparison: other countries",con)
                    #write.csv(checkc,con)
                    for(cc in selp) {
                        curtab<-checkc[checkc$party==cc,]
                        write.table(curtab,sep=",",col.names=FALSE,con)
                        writeLines("#",con)
                    }
                }
                close(con)
            }
            check$val<-paste0(checkdir,"/",checkfile)
        }
    }
    return(check)
}

