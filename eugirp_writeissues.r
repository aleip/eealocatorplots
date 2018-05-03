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
    writeLines(paste0("#\n#Note for column: correction: 0: value assumed to be a mistake. it is exlcuded from the calculation of the EU weighted average to not bias the EU-value and requires clarification. ",
                      "1: value is assumed to be not an outlier despite the criteria (e.g. milk production). empty: to be clarified. ",
                      "rellim: relaxing upper and lower limit by 10% or more for 3.B - issue OK but can be quickly checked. ",
                      "cattle: problem likely related for Dairy/Non-dairy cattle, thus duplication. "),con)
    write.csv(D,con)
    close(con)
    
}

observationyear<-function(line){
    if(line$years=="all"){
        observationyrs<-"all years"
    }else{
        observationyrs<-paste(line$years,collapse=",")
    }
    return(observationyrs)
}


observationoutlier<-function(line){
    
    #if(line$years=="all"){
    #    observationyrs<-"all years"
    #}else{
    #    observationyrs<-paste(line$years,collapse=",")
    #}
    observationyrs<-observationyear(line)
    observationsec<-paste(line$sector_number,line$category,observationyrs,sep=" - ")
    observationmea<-paste0(line$measure," (",line$meastype,")")
    
    observation<-paste0(observationsec," - ",observationmea,": ","Data has been identified as an outlier.")
    return(observation)
    
}
observationrecalc<-function(line,check){
    
    observationsec<-line$sector_number
    observationcat<-line$category
    yrs<-strsplit(line$years,"-")[[1]]
    observationyrs<-length(yrs)
    
    if(observationyrs>0){
        share<-0
        #for(i in c(1:observationyrs)){share<-share+(line[,paste0("s",yrs[i])])/observationyrs}
        for(i in c(1:observationyrs)){share<-share+(line[,paste0("sYear",i)])/observationyrs}
        observation<-paste0(line$sector_number," - ",line$category,": ",
                        "Recalculation for the year",if(observationyrs>1){"s"}," ",gsub("-"," and ",line$years),
                        if(share>0){" increased "}else{" decreased "},
                        "total national ",line$gas," emissions by more than 0.5%.")
        if(check=="recalc1"){observation<-paste0(observation," as compared to last years' submission.")}
        if(check=="recalc2"){observation<-paste0(observation," as previous submission of this year.")}
    }else{observation<-""}
    return(observation)
    
}
questionrecalc<-function(line){
    
    questionsec<-line$
    questioncat<-line$category
    yrs<-strsplit(line$years,"-")[[1]]
    questionyrs<-length(yrs)
    question<-""
    if(questionyrs>0){
        question<-paste0("Emissions were checked on significant recalculations for the years 1990 and ",as.numeric(lastyear)-1,". ")
        for(i in c(1:questionyrs)){
            #effect<-rounddigit(line[,paste0("e",yrs[i])])
            #share<-rounddigit(line[,paste0("s",yrs[i])])
            effect<-rounddigit(line[,paste0("eYear",i)])
            share<-rounddigit(line[,paste0("sYear",i)])
            question<-paste0(question,
                             "Recalculation in ",yrs[i]," was ",effect," ",line$unit," or ",
                            share*100,"% of total national ",line$gas," emissions. ")
            
        }
        question<-paste0(question,"Please explain the reason for this recalculation. ",
                         "Un-justified recalculations could be associated with over- or underestimation of emissions. ")
        
    }else{question<-""}
    return(question)
    
}
observationgrowth<-function(line){
    
    observationsec<-line$sector_number
    observationmea<-paste0(line$measure," (",line$meastype,", ",line$category,")")
    observationyrs<-paste(line$years,collapse=",")
    
    observation<-paste0(observationsec," - ",observationmea,": ","Irregularities in the time series have been been identified. Years flagged: ",observationyrs)
    return(observation)
    
}
observationagri<-function(line){
    
    if(!"range"%in%names(line)) line$range<-line$fac
    
    
    observationsec<-line$sector_number
    if(!"sector_number"%in%names(line)) observationsec<-line$sec
    observationsec<-as.character(observationsec)
    observationsecs<-strsplit(observationsec,",")[[1]]
    
    observationcat<-as.character(line$category)
    if(!"category"%in%names(line)) observationcat<-as.character(line$cat)
    observationcats<-strsplit(observationcat,",")[[1]]
    if(length(observationcats)<4){
        #observationcat<-gsub("_"," ",paste(observationcats,collapse=" and "))
        observationcats<-gsub("_"," ",observationcats)
    }else{
        observationcat<-"Various"
    }
    if(length(observationsecs)<4){
        #observationsec<-gsub("_"," ",paste(observationcats,collapse=" and "))
        observationsecs<-gsub("_"," ",observationsecs)
    }else{
        observationsec<-"Various"
    }
    observation<-paste0(paste(observationsecs," (",observationcats,")",collapse=" and "),": ")
    
    
    observationyrs<-paste(line$years,collapse=",")
    if(!"years"%in%names(line)) observationyrs<-line$yr
    
    test<-line$check
    val1<-line$val1
    val2<-line$val2
    
    
    question<-""
    issue<-""
    advice<-""
    
    
    if(test=="NexTOT"){
        question<-paste("Check ",line$check," (check of total manure excreted per animal type versus the sum of manure excretion over the MMS). ")
        if(grepl("total",line$obs)) issue<-paste0("No total N excretion is reported for animal type ",line$cat)
        if(grepl("val1 ",line$obs)) issue<-paste0("Sum of manure excretion over the MMS is different from Total N excretion for animal type ",line$cat," (",line$obs," - ",line$range,")")
        advice<-paste0("Please include values for total N excretion for all animal types in your next submission. See file for further information.")
    }
    if(test=="NexRATE"){
        question<-paste("We compared the sum of manure excreted over the MMS per animal type versus the N-excretion rate multiplied by the animal population (heads). ")
        animaltypes<-"animal type "
        if(grepl("not reported",line$obs)) {
            issue<-paste0("No N excretion rate is reported for ",observationcat)
            advice<-paste0("Question: Please include values for the N excretion rate all animal types in your next submission. See file for further information.")
        }
        if(grepl("val1 is fac ",line$obs)) {
            issue<-paste0("sum of manure excretion over the MMS is different from POP/1000 x N-rate for the identified ",animaltypes)
            advice<-paste0(" (range: ",line$range,"). ",
                           "Please justify the reason for the discrepancy and/or correct in your next submission for this ",animaltypes,
                           ". ")
        }
    }
    if(grepl("N2O-IEF in ",test)){
        question<-paste0("Calculate IEF from N available and N2O emissions. ")
    }
    if(grepl("N in ",test)){
        question<-paste0("Compare the sum of N handled in MMS over animal type vs. total N handled. ")
        issue<-paste0("Total N managed in MMS reported in '3.B.2.5 N2O Emissions per MMS' does not match with the sum of the values reported over the animal types. ")
        advice<-paste0("Please justify the reason for the discrepancy and/or correct in your next submission. ")
    }
    if(grepl("N2O-NIEF",test)){
        question<-paste("Calculation of N2O-IEF. ")
        #animaltypes<-"animal type "
        #if(length(cats)>1) animaltypes<-"animal types "
        #animaltypes<-paste0(animaltypes,paste(secs,cats,collapse=" and "))
        if(grepl("not reported",line$obs)) {
            issue<-paste0("Missing information. ")
            advice<-paste0("Please complete the CRF table with the missing information.")
        }
        if(grepl("val1 is fac ",line$obs)) {
            issue<-paste0("The results for the N2O-IEF differ between two calculation methods. ")
            advice<-paste0("Please justify the reason for the discrepancy and/or correct in your next submission. ")
            #"Please compare the values reported under 'IEFN1' and 'IEFN2'! ")
            advice<-paste0(advice,"See file for further information. Compare the values reported under 'IEFN1' and 'IEFN2'! ")
            advice<-paste0(advice,"IEFN1 is calculated as the weighted average of the IEF for the MMS (IEF_MMS) and the N excreted to the MMS (IEFN1=SUM(IEF_MMS * NEXC_MMS)/TNEXC). ",
                             "IEFN2 is calculated as from total (direct) Emissions and Total N excreted (IEFN2=EM/TNEXC*28/44). ",
                             "IEF_MMS is calculated from Total N handled per MMS and Direct N2O emissions per MMS (IEF_MMS=EM_MMS/N_MMS*28/44). ",
                             "--> A difference in IEFN1 and IEFN2 indicates ",
                             "that either the use of IEF_MMS that are different for different animal types (e.g. different sub-systems - w/o natural crust etc. In this case please provide an explanation! ",
                             "Otherwise this indicates an error in the calculation. In this case please provide a correction.")
        }
    }
    
    if(grepl("CLIMA",test)){
        question<-paste("")
        #animaltypes<-"animal type "
        #if(length(cats)>1) animaltypes<-"animal types "
        #animaltypes<-paste0(animaltypes,paste(secs,cats,collapse=" and "))
        
        issue<-paste0("The allocation over all climate regions and MMS (Tier 2) sums does not sum up to 100 for ",line$cat)
        advice<-paste0(" Please correct in your next submission. ",
                       " Alternative if the sum adds to 1:", 
                       " Please note that the allocation of manure (Tier 2) must be reported in percentage and add up to 100% over all climate regaions and MMS. It seems that you have reported the values as fractions. Please correct in your next submission")
    }
    
    if(test=="CPP and SO def IEFs"){
        question<-paste("We compared the IEF reported in category 3.D.1.3 (N2O emissions from Urine and Dung Deposited by Grazing Animals) ",
                     "with default IEFs EF3RPR_CPP for Cattle - Pigs and Poultry (0.02) and",
                     "EF3RPR_SO for Sheep and other animals (0.01) using the shares FracRPR_CPP and FracRPR_SO of manure",
                     "deposited by the two animal groups. ")
        #animaltypes<-paste0(paste(s,cat,collapse=" and "))
        if(grepl("not reported",line$obs)) {
            issue<-paste0("Missing information to perform the test for ",observationcat)
            advice<-paste0(question,"Please complete the CRF table with the missing information.")
        }
        if(grepl("val1 is fac ",line$obs)) {
            issue<-paste0("Reported N2O-IEF differ from calculated according to animal types. ")
            advice<-paste0("We found that the reported N2O-IEF value differs from default (ratio range: ",line$range,
                           " ) and we could not find any explanation on the use of non-default EFs in the NIR. ",
                           "Please justify the reason for the discrepancy in your next submission.")
        }
    }
    if(grepl("Manure_grazing",test)){
        question<-paste("We compared the manure 'managed' in Pasture Range and Paddock in category 3.B.2 ",
                     "with activity data in 3.D.1.3 (Urine and Dung Deposited by Grazing Animals).  ",
                     "According to IPCC equation 11.5 the annual amount of urine and dung N deposited on pasture range and paddock by grazing animals (FPRP) ",
                     "is calculated from the fraction deposited on PRP without subtracting any volatilization or leaching losses. ",
                     "The sum of FPRP over all animal types should therefore equal the activity data in category 3.D.1.3. ",
                     "We found ")
        issue<-paste0("inconsistent data between manure reported on pastures in category 3.B.2 and manure from grazing animals reported in 3.D.1.3 " )
        advice<-paste0("(ratio range: ",line$range,"). Please justify the difference in the values reported or correct in accordance with the IPCC guidelines. ")
    }
    if(grepl(" loss ratio",test)){
        if(line$obs=="Nvol is not reported"){
            question<-paste("Fraction of N lost in MMS via volatilization of NH3+NOx versus total managed manure.",
                         "According to IPCC Table 10.22 most of the loss fractions of N in managed manure are between 20% and 45%. ",
                         "")
            issue<-paste0("No N volatilization is reported.")
            advice<-paste0("Please report N volatilization in category 3.B.2 in accordance with the IPCC guidelines. ")
        }else{
            rmax<-max(unlist(strsplit(gsub("range: ","",line$range),split="-")))
            rmin<-min(unlist(strsplit(gsub("range: ","",line$range),split="-")))
            if(rmin>=0.45){rte<-" high"}else if(rmax<=0.2){rte<-" low"}else{rte<-""}
            cat(rmax,rmin,rte,"\n")
            question<-paste0("According to IPCC Table 10.22 most of the loss fractions of N in managed manure are between 20% and 45%. ",
                         "We identified N losses that are ", as.numeric(rmin)*100," - ", as.numeric(rmax)*100 , "%, therefore ")
            issue<-paste0("the fraction of N lost in MMS via volatilization of NH3+NOx is out of range.")
            advice<-paste0("Please justify the ",rte," N volatilization rates reported or correct in accordance with the IPCC guidelines.")
                             #,"Check the calculated fraction 'Nlossratio' that is obtained from 'Nvol' (not the factor 1000000 in the unit) and managed 'NEXC'.")
        }
    }
    if(grepl("N application ratio",test)){
        facv<-if(is.numeric(line$range)){round(line$range,3)}else{line$range}
        question<-paste("We compared manure applied to soils reported in sector 3.D.1.2.a with N managed in manure management systems minus 'Nvol' plus 'Nleach' in sector 3.B.2. ",
                     "")
        issue<-paste0(" The amount of N applied with animal manure in 3.D.1.2.a is too large as compared to N managed in MMS minus N lost as NH3+NOx or leaching")
        advice<-paste0("(inverse ratio ",facv,"). ",
                       "Please justify the difference in the values reported or correct in accordance with the IPCC guidelines. ",
                        "According to IPCC equation 11.4 FAM is calculted from NMMS_Avb (as calculated in Equation 10.34). ",
                        "NMMS_Avb is obtained from N managed in MMS and not lost (FracLOSSMS from Table 10.23) plus any addition of bedding material. ",
                        "The loss fractions in Table 10.23 include not only NH3 and NOx but also losses of N2 which are not included in the indirect emissions-volatilizations in 3.B.2. ",
                        "Therefore FAM is expected to be smaller than N managed in MMS minus N lost as NH3+NOx+leaching unless bedding material has been accounted for. ",
                        "Please check and correct/explain.")
    }
    
    if(observationyrs!="")observationyrs<-paste0(" Years: ",observationyrs)
    question<-paste0(observation,question,issue,advice)
    observation<-paste0(observation,issue,observationyrs)
    return(list(observation,question))
}
questionoutlier<-function(line){

    # Start question
    observationsec<-paste(line$sector_number,line$category,observationyear(line),sep=" - ")
    observationmea<-paste0(line$measure," (",line$meastype,", ",observationsec,")")
    question<-paste0("The value of the outlier for the ",observationmea,"is with ", rounddigit(line$value)," (mean of outlier years) ",
                     rounddigit(line$value/line$media), " times the median value reported from all countries and ")
    if(line$value>line$ulim)question<-paste0(question,round(line$value/line$ulim,1)," times the calculated upper limit.")
    if(line$value<line$llim)question<-paste0(question,round(line$value/line$llim,1)," times the calculated lower limit.")
    
    # Add part about significance
    if(grepl("over|under",line$significant)){
        pottext<-"This issue is considered a potential significant issue."
        sign<-paste0(pottext," In the year ",signyear," the value used was ",
                     format(as.numeric(line[,as.character(signyear)]),digits=3)," - ",
                     format(as.numeric(line$relmedian),digits=2),
                     " times the median value reported from all countries over all years (",rounddigit(line$median),"). ",
                     "The ",line$measure," has significant impact on emissions.")
        signestimate<-paste0(format(as.numeric(line$share)*100,digits=2),"%. ",
                     "We estimated the potential effect at ",format(as.numeric(line$effect)*100,digits=2),
                     "%. It is thus larger than the threshold of ",format(as.numeric(signthreshold)*100,digits=2),
                     "% of the national total.")
        sign<-paste0(sign," ",potsign)
    }else{
        if(line$correction==0){
            sign<-paste0("We judge the value a serious outlier which is likely to be a mistake!")
        }else{
            sign<-paste0("Please provide a justification for this outlier (or correct).")
        }
    }
    if(line$followup!=""){followup<-paste0("Note that this is a follow-up of issue# ",line$followup,".")}else{followup<-""}
    if(line$communication!=""){communic<-line$communication}else{communic<-""}
    #question<-paste0(question," Further information and explanations and comparison with other countries' submissions see file.")
    
    question<-paste(question,sign,followup,communic,sep=" ")
    return(question)
}

questiongrowth<-function(line){
    
    # Start question
    question<-paste0("Irregular time series can be caused by changes in the situation (changes in regulations ",
                     "or national circumstances, technology changes, environmental conditions, pests, ...). ",
                     "Irregular time series can also be caused by inconsistent data sources (e.g. changes in survey data), ",
                     "changes in calculation methods (e.g. due to lack of data for a certain period), or mistakes. ",
                     "Time series inconsistencies could be linked with an over- or underestimation of emissions in part of the ",
                     "time series. Please provide a justication/explanation for the observed irregularities.")
    #if(line$followup!=""){followup<-paste0("Note that this is a follow-up of issue# ",line$followup,".")}else{followup<-""}
    #if(line$communication!=""){communic<-line$communication}else{communic<-""}
    #question<-paste0(question," Further information and explanations and comparison with other countries' submissions see file.")
    
    #question<-paste(question,followup,communic,sep=" ")
    question<-paste(question,sep=" ")
    return(question)
}

growthtype<-function(line){
    gtype<-tolower(as.vector(line$correction))
    gtypes<-read.csv(file=paste0(issuedir,"timeseries/20160202_growth_IEFS.csv"))
    gdescr<-as.character(gtypes$Description[tolower(gtypes$gtype)==gtype])
    gdescr<-paste0("The time series was judged to be of type '",gtype,"': ",
                   gdescr," Note that the identified years not always correspond to the years which appear to be 'irregular'.")
    
    return(gdescr)
}

keysources<-function(){
    keycategories<-read.csv(lastkeyfile)
    keycategories<-keycategories[keycategories$party!="",c("party","category","gas","keylevel")]
    keycategories$party<-unlist(lapply(c(1:nrow(keycategories)),function(x) 
        if(keycategories$party[x]=="FRK"){
            "FM"
        }else if(keycategories$party[x]=="EU"){
            "EUC"
        }else{
            as.character(country4sub[country4sub$code3==keycategories$party[x],"code3"])
        }))
    keycategories$party[keycategories$party=="EU"]
    keycategories$category<-unlist(lapply(c(1:nrow(keycategories)),function(x) 
        {sec<-substr(keycategories$category[x],1,3)
        if(sec=="3.D") sec<-substr(keycategories$category[x],1,5)
        if(sec=="3.B"&keycategories$gas[x]=="CH4") sec<-"3.B.1"
        if(sec=="3.B"&keycategories$gas[x]=="N2O") sec<-"3.B.2"
        if(sec=="3.D.A") sec<-"3.D.2"
        return(sec)}
        ))
    levels(keycategories$keylevel)<-c(0,1)
    keycategories$keylevel[keycategories$keylevel=="X"]<-1
    keycategories$keylevel[keycategories$keylevel==""]<-0
    
    return(keycategories)    
}

getshortsector<-function(sector){
    sector<-as.character(sector)
    sec<-substr(sector,1,3)
    if(sec=="3.D" | sec=="3.B") sec<-substr(sector,1,5)
    if(sec=="3.D.A") sec<-"3.D.2"
    return(sec)    
}
identifygas<-function(sector){
    #print(sector)
    sec<-getshortsector(sector)
    if(sec=="3.A") gas<-"CH4"
    if(sec=="3.B.1") gas<-"CH4"
    if(sec=="3.B.2") gas<-"N2O"
    if(sec=="3.C") gas<-"CH4"
    if(sec=="3.D.1") gas<-"N2O"
    if(sec=="3.D.2") gas<-"N2O"
    if(sec=="3.F") gas<-"CH4-N2O"
    if(sec=="3.G") gas<-"CO2"
    if(sec=="3.H") gas<-"CO2"
    if(sec=="3.I") gas<-"CO2"
    if(sec=="3.i") gas<-"CO2"
    if(sec=="3.J") gas<-"CO2"
    
    return(gas)
}

keysourcecat<-function(line){
    prt<-line$party
    
    if(nchar(prt)>3){prt<-"EUC"}
    sec<-getshortsector(line$sector_number)
    uid<-line$variableUID
    if(grepl("^\\.",sec)) sec<-"3.A"
    gas<-identifygas(sec)
    #print(prt)
    #print(line$sector_number)
    #save(line,file="line")
    source("eugirp_functions.r")
    keycategories <- keycategories()
    keyline<-keycategories[sapply(1:nrow(keycategories),function(x) grepl(keycategories$sector_number[x],line$sector_number)),]
    keyeu<-as.logical(sum(keyline[,paste0("EUC","key")]))
    keyms<-as.logical(sum(keyline[,paste0(prt,"key")]))
    #if(length(keyms)==0)keyms<-0
    #print(sec)
    #print(gas)
    #print(prt)
    #print(keyms)
    #print(keyeu)
    return(list(keyms,keyeu))
}
isphase2<-function(line){
    oldissuefile<-"issues/issues2015.csv"
    if(file.exists(oldissuefile)) {
        oldissues<-read.csv(oldissuefile)
        phase2<-oldissues[oldissues$Phase.2!="",c("File.Name.","In.20160202colist","Phase.2","Conclusion.comment")]
        issue<-line$followup
        select<-as.character(phase2$File.Name.)==issue
    }else{
        select<-1
    }
    return(sum(select))
}

keyflags<-function(line,check){
    #ms,eu key
    key1<-as.numeric(as.vector(unlist((keysourcecat(line)))))
    #union
    key4<-isphase2(line)
    #psi1
    if(check=="init"){
        if(grepl("over|under",line$significant)){key5<-1}else{key5<-0}
        #unfccc
        key3<-line$unfccc
        key3[is.na(key3)]<-0
    }else{
        key5<-0
        key3<-0
    }
    #ptc    notsent	tc	revised
    key6<-rep(0,4)

    #phase of checks
    checks<-c("ne","init","recalc1","recalc2")
    key2<-rep(0,4)
    #if(check%in%c("init","outlier","growth","agri"))check<-"init"
    key2[which(checks==check)]<-1

    #print(paste0("key1=",key1))
    #print(paste0("key2=",key2))
    #print(paste0("key3=",key3))
    #print(paste0("key4=",key4))
    #print(paste0("key5=",key5))
    #print(paste0("key6=",key6))
    allflags<-list(key1,key2,key3,key4,key5,key6)
    #print(length(allflags))
    return(allflags)
}
emrtsector<-function(sectornumber){
    if(grepl("^3.A",sectornumber)){sector<-"3A Enteric Fermentation"}else
    if(grepl("^3.B",sectornumber)){sector<-"3B Manure Management"}else
    if(grepl("^3.C",sectornumber)){sector<-"3C Rice Cultivation"}else
    if(grepl("^3.D.1",sectornumber)){sector<-"3D1 Direct N2O from managed soils"}else
    if(grepl("^3.D.2",sectornumber)){sector<-"3D2 Indirect N2O from managed soils"}else
    if(grepl("^3.D",sectornumber)){sector<-"3D Agricultural soils"}else
    if(grepl("^3.E",sectornumber)){sector<-"3E Prescribed burning of Savannas"}else
    if(grepl("^3.F",sectornumber)){sector<-"3F Fiend burning of agricultural residues"}else
    if(grepl("^3.G",sectornumber)){sector<-"3G Liming"}else
    if(grepl("^3.H",sectornumber)){sector<-"3H Urea application"}else
    if(grepl("^3.I",sectornumber)){sector<-"3I Other Carbon-containing fertilizers"}else
    if(grepl("^3.J",sectornumber)){sector<-"3J Other"}else
    {sector<-"3 Agriculture"}
    
    return(sector)
    
}

flags4newissue<-function(line,check,x){
  
    #cat(x," ")
    country<-country4sub[country4sub$code3==as.character(line$party),"long"]
    if(length(country)==0)country<-as.character(line$party)
    if(check=="outlier") {
        observation<-observationoutlier(line)
        question<-questionoutlier(line)
    }
    if(check=="growth") {
        observation<-observationgrowth(line)
        question<-questiongrowth(line)
    }
    if(check=="agri"){
        observation<-observationagri(line)[[1]]
        question<-observationagri(line)[[2]]
    }
    if(grepl("recalc",check)){
        observation<-observationrecalc(line,check)
        question<-questionrecalc(line)
    }
    revyear<-invyear
    sector<-emrtsector(line$sector_number)
    gas<-identifygas(line$sector_number)
    if(line$years=="all" | line$years==""){
        yrs<-paste0("1990-",lastyear)
    }else{
        yrs<-as.character(line$years)
        #convert if this is 'except' years
        if(grepl("-",yrs)){
            yrs<-gsub("-"," ",yrs)
        }else{
            if(grepl("all except",yrs)){
                cury<-gsub("all except: ","",yrs)
                cury<-as.numeric(unlist(strsplit(cury," ")))
                cury<-yearsnum[!yearsnum %in% cury]
            }else{     
                cury<-as.numeric(unlist(strsplit(yrs," ")))
            }
            curymin<-min(cury)
            curymax<-max(cury)
            yrs<-paste0(curymin,"-",curymax)
            if(length(cury)==1)yrs<-cury
        }
    }
    if(check=="agri"){
        par<-"Other"   
    }else{
        if(line$meastype=="EM"){par<-"Emission"}else if(line$meastype=="IEF"){par<-"Emission factor"}else
            if(line$meastype%in%c("AD","AREA","POP")){par<-"Activity Data"}else{par<-"Other"}
    }
    flags<-keyflags(line,check)
    #print(length(flags))
    #cat(x,"(",length(unlist(list(country,observation,question,revyear,sector,gas,yrs,par,flags))),") ")
    #print(length(question)+length(revyear)+length(par))
    #print(length(observation)+length(gas)+length(yrs))
    #print(length(country))
    #print(length(sector))
    return(list(country,observation,question,revyear,sector,gas,yrs,par,flags))
}

writeoutlierissue<-function(D,line,listoftext){
    nottoplot<-c("issuenr","issueflag","issuedate","explanation","date","resolved","comment","followup","plot")
    writenames<-names(line[!names(line)%in%nottoplot])
    observation<-observationoutlier(line)
    question<-questionoutlier(line)
    note<-"#"
    if(line$resolved==4){question<-paste0(question,"\n# The issue is a follow-up of a previous issue flagged as unresolved: ",gsub(","," - ",line$followup))}
    communication<-"#"
    if(line$communication!=""){question<-paste0(question,"\n# ",line$communication)}
    
    
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
        observation<-unique(paste0(P$party[vu],", "))
        #observation<-paste0(observation,psect)
        observation<-paste0(observation," Values for ",meas," seem to be inconsistent with the unit (",unit,") requested.")
        observation<-paste0(observation," Question: please provide justification for the values used or otherwise correct the values used.")
        observation<-paste0(observation," Suggested corrected values and concerned source categories are found below.")
        filnam<-paste0("corrections_",name,"_",c,".csv")
        filfol<-paste0(issuedir,"autocorrections")
        filnamt<-paste0(filfol,"/",filnam)
        if (! file.exists(filfol)){dir.create(file.path(filfol))}
        con <- file(filnamt, open="wt")
        writeLines(paste0("# Observation: ",observation),con)
        writeLines("# Data before correction\n#",con)
        write.csv(before[order(before$party,before$sector_number),],con)
        writeLines("#\n#\n# Data after correction\n#",con)
        write.csv(after[order(after$party,after$sector_number),],con)
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
                    years,
                    "classification","target","type","option","variableUID")
    checkfields<-c("check","val1","val2","sec","cat","obs","ms","yr","fac","val")
    names(check)<-gsub("test","check",names(check))
    n<-nrow(check)
    
    
    #resolve countries
    c<-as.character(check$ms)
    y<-check$yr
    if(y == "all"){y2 <- years}else if(grepl("all except", y)){y2 <- unlist(strsplit(y,": "))[2] ; y2<-unlist(strsplit(y2, " "))}else{y2<-unlist(strsplit(y, " "))} 
    s<-as.character(check$sec)
    cat<-as.character(check$cat)
    print(cat)
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
        cats<-gsub("_"," ",strsplit(cat,split = ",")[[1]])
        secs<-gsub("_"," ",strsplit(s,split = " ")[[1]])
        selection<-data$sector_number%in%secs & data$category%in%cats

        
        observation<-paste(check[,checkfields],collapse="-")
        question<-" Question: Please correct/provide the missing information in your next submission."
        observation<-observationagri(check)[[1]]
        question<-observationagri(check)[[2]]
        issue<-gsub(" ","",check$check)
        #print(check$check)
        if(check$check=="NexTOT"){
            extraline<-"# Note: countries which do not report both 'Nitrogen excretion per MMS' and 'Total N excreted' are not identified"
            selection<-selection & data$gas=="no gas" & (data$meastype=="EM" | data$meastype=="NEXC") 
        }
        if(check$check=="NexRATE"){
            selection<-( 
                             (grepl("3.B.2|3.A",data$sector_number) & data$category%in%cats)
                         ) & (data$meastype%in%c("POP","NRATE","TNEXC2","EM") & 
                                  (data$gas%in%c("no gas","")))
        }
        
        if(grepl("N in ",check$check)){
            mms<-manureSystems[which(manureSysshrt==gsub("N in ","",check$check))]
            selection<-(data$sector_number%in%secs & data$meastype=="NEXC")| 
                (grepl("3.B.2",data$sector_number)&data$source==mms&data$cat%in%mainanimals)
        }

        if(grepl("N2O-IEF in ",check$check)){
            selection<-((data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$meastype=="IEF"))
        }
        
        if(grepl("N2O-NIEF",check$check)){
            directsys<-manureSystems[!manureSystems%in%c("Pasture  range and paddock","Burned for fuel or as waste")]
            #print(paste0("1. ",sec,"-",sum(selection),unique(unlist(data$source[selection]))))
            #selsys<-directsys[directsys%in%unique(unlist(data$source[selection]))]
            selection<-((data$sector_number=="3.B.2.5 N2O Emissions per MMS" & 
                             data$meastype%in%c("IEF","EM","NEXC"))| 
                            (data$meastype%in%c("IEFN1","IEFN2")
                            ))
            #print(sum(selection))
            #print(selsys)
            
            extraline<-paste0("# Please compare IEFN1 with IEFN2.\n",
                              "# IEFN1 is calculated as the weighted average of the IEF for the MMS (IEF_MMS) and the N excreted to the MMS, referred to Total N excreted (IEFN1=SUM(IEF_MMS * NEXC_MMS)/TNEXC\n",
                              "# IEFN2 is calculated as from total (direct) Emissions and Total N excreted. IEFN2=EM/TNEXC*28/44\n",
                              "# IEF_MMS is calculated from Total N handled per MMS and Direct N2O emissions per MMS (IEF_MMS=EM_MMS/N_MMS*28/44\n",
                              "# --> A difference in IEFN1 and IEFN2 indicates \n",
                              "#     - The use of IEF_MMS that are different for different animal types (e.g. different sub-systems, w/o natural crust etc. In this case please provide an explanation!!\n",
                              "#     - An error in the calculation. In this case please provide a correction.\n",
                              "#")
        }

        if(grepl("CLIMA",check$check)){
            print(cats)
            selection<-data$meastype=="CLIMA"&data$category%in%cats
        }
        
        if(check$check=="CPP and SO def IEFs"){
            if(grepl("val1 is fac ",check$obs)) {
                extraline<-"# Please compare the values for 'IEF' and 'EF3_default' below. The calculated fractions of manure from CPP and SO are given in 'FracPRP_CPP' and 'FracPRP_SO'. "
            }
            selection<-data$sector_number==s #& data$category==cat
        }
        if(grepl("Manure_grazing",check$check)){
            selection<-data$sector_number%in%secs | (data$source=="Pasture range and paddock")
        }
        if(grepl(" loss ratio",check$check)){
            selection<-grepl("3.B.2.5",data$sector_number) & data$meastype%in%c("Nvol","Nlea","Nlossratio","NEXC")
        }
        if(grepl("N application ratio",check$check)){
            facv<-if(is.numeric(check$fac)){round(check$fac,3)}else{check$fac}
            selection1<-data$sector_number==secs&data$meastype%in%c("AD","FracNavapp")
            selection2<-data$sector_number=="3.B.2.5 N2O Emissions per MMS"&data$meastype%in%c("NEXC","FracLiqOther")
            selection3<-data$sector_number=="3.B.2.5"&data$meastype%in%c("Nvol","Nleach")
            selection<-selection1|selection2|selection3
        }
        
        selp<-as.vector(unique(unlist(data$party[selection])))
        selp<-selp[!selp%in%c]
        selp<-selp[!selp%in%eu]
        selw<-selection & (data$party %in% c)
        selc<-selection & (data$party %in% selp)
        checkw<-data[selw & ! data$party %in% eu,reportfields]
        checke<-data[selw &   data$party %in% eu,reportfields]
        checkc<-data[selc,reportfields]
        #xavi20180207: checkw<-checkw[order(checkw$party,checkw$category,checkw$notation,checkw$measure,checkw$source),]
        checkw<-checkw[order(checkw$category,checkw$unit),]
        if(check$check=="NexRATE" & check$obs=="val1 is fac x val2"){
          rw_comp <- checkw[order(checkw$category,checkw$unit),] 
          categs <- unique(checkw$category)
          rw_comp1 <- data.frame()
          for (ct in categs){
            rw_comp <- checkw[checkw$category == ct, ]
            rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
            rw_comp[nrow(rw_comp), "party"] <- c
            rw_comp[nrow(rw_comp), "category"] <- ct
            rw_comp[nrow(rw_comp), "notation"] <- paste0("Comp.")
            rw_comp[is.na(rw_comp)]<- ""
            rw_comp1 <- rbind(rw_comp1, rw_comp)
          }
          checkw <- rw_comp1 
        }
        
        if(check$check=="N in burned"){
          rw_comp <- checkw[order(checkw$sector_number,-c(checkw$source)),]
          rw_comp[nrow(rw_comp)+1, years] <- colSums(rw_comp[-nrow(rw_comp), years])  
          rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
          rw_comp[, "party"] <- c
          #rw_comp[nrow(rw_comp), "category"] <- ct
          rw_comp[nrow(rw_comp)-1, "notation"] <- paste0("Sum ")
          rw_comp[nrow(rw_comp), "notation"] <-   paste0("Comp.")
          rw_comp[is.na(rw_comp)]<- ""
          checkw <- rw_comp 
        }
        
        if(check$check=="N2O-NIEF" & check$obs=="val1 is fac x val2"){
          rw_comp <- checkw[order(checkw$category,checkw$unit),] 
          categs <- unique(checkw$category)
          categs <- categs[!categs%in%c("Farming")]
          rw_comp1 <- data.frame()
          for (ct in categs){
            rw_comp <- checkw[checkw$category == ct, ]
            if(nrow(rw_comp)<2) next
            rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
            rw_comp[nrow(rw_comp), "party"] <- c
            rw_comp[nrow(rw_comp), "category"] <- ct
            rw_comp[nrow(rw_comp), "notation"] <- paste0("Comp.")
            rw_comp[is.na(rw_comp)]<- ""
            rw_comp1 <- rbind(rw_comp1, rw_comp)
          }
          checkw <- rw_comp1 
        }
        
        if(check$check=="CLIMA"){
          rw_comp <- checkw[order(checkw$category,checkw$unit),] 
          categs <- unique(checkw$category)
          rw_comp1 <- data.frame()
          for (ct in categs){
            rw_comp <- checkw[checkw$category == ct, ]
            rw_comp[nrow(rw_comp)+1, years] <- colSums(rw_comp[-nrow(rw_comp), years]) 
            rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
            rw_comp[, "party"] <- c
            rw_comp[, "category"] <- ct
            rw_comp[nrow(rw_comp), "notation"] <- paste0("Comp.")
            rw_comp[is.na(rw_comp)]<- ""
            rw_comp1 <- rbind(rw_comp1, rw_comp)
          }
          checkw <- rw_comp1 
        }
        
        if(check$check=="Manure_grazing"){
          rw_comp <- checkw[order(checkw$sector_number, checkw$meastype, decreasing = TRUE),] 
          rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
          rw_comp[nrow(rw_comp), "party"] <- c
          rw_comp[nrow(rw_comp), "category"] <- "Ratio"
          rw_comp[nrow(rw_comp), "notation"] <- paste0("Comp.")
          rw_comp[is.na(rw_comp)]<- ""
          checkw <- rw_comp
        }
        
        if(check$check=="CPP and SO def IEFs"){
          rw_comp <- checkw[order(checkw$party, as.numeric(checkw$meastype%in%c("IEF", "EF3_default")==TRUE)), ] #xavi: this is the optimal way to order, change previous blocks!!
          if(length(unique(rw_comp$party))>1){
            nrw <- nrow(rw_comp)/length(unique(rw_comp$party))
            nrnd <- seq(nrw, nrow(rw_comp), by=nrw)
            insr <- 0
            rw_comp1<-data.frame()
            for (n in nrnd){
              rw_comp2<-rw_comp[(insr+1):n,]
              rw_comp2[nrow(rw_comp2)+1, years] <- rw_comp2[nrow(rw_comp2)-1, years] / rw_comp2[nrow(rw_comp2), years]
              insr <- n
              rw_comp2[, "party"] <- unique(rw_comp2$party[!is.na(rw_comp2$party)])
              rw_comp2[nrow(rw_comp2), "category"] <- "Ratio"
              rw_comp2[nrow(rw_comp2), "notation"] <-   paste0("Comp.")
              rw_comp1<-rbind(rw_comp1, rw_comp2)
            }
            rw_comp<-rw_comp1
          }else{
            rw_comp[nrow(rw_comp)+1, years] <- rw_comp[nrow(rw_comp)-1, years] / rw_comp[nrow(rw_comp), years]
            rw_comp[, "party"] <- c
            rw_comp[nrow(rw_comp), "category"] <- "Ratio"
            rw_comp[nrow(rw_comp), "notation"] <-   paste0("Comp.")
          }
          rw_comp[is.na(rw_comp)]<- ""
          checkw <- rw_comp 
        }
        
        if(check$check=="N application ratio"){
          checkw <- checkw[order(checkw$party, as.numeric(checkw$meastype%in%c("FracNavapp")==TRUE)), ] #xavi: this is the optimal way to order, change previous blocks!!
          
        }
        
        #xavi20180207: if(check$check=="N2O-NIEF") checkw<-checkw[order(checkw$category,checkw$meastype,checkw$party,checkw$notation,checkw$measure,checkw$source),]
        checke<-checke[order(checke$party,checke$category,checke$notation,checke$measure,checke$source),]
        #xavi20180207: checkc<-checkc[order(checkc$party,checkc$category,checkc$notation,checkc$measure,checkc$source),]
        checkc<-checkc[order(checkc$party,checkc$category,checkc$unit),]
        if(check$check=="N in burned") checkc<-checkc[order(checkc$party,checkc$sector_number,-c(checkc$source)),]
        if(check$check=="Manure_grazing") checkc <- checkc[order(checkc$party,checkc$sector_number, checkc$meastype, decreasing = TRUE),] 
        if(check$check=="CPP and SO def IEFs") checkc <- checkc[order(checkc$party, as.numeric(checkc$meastype%in%c("IEF", "EF3_default")==TRUE)),] 
        if(check$check=="N application ratio") checkc <- checkc[order(checkc$party, as.numeric(checkc$meastype%in%c("FracNavapp")==TRUE)), ]
          
        
        if(exists("checkw")>0){
            
            checkdir<-paste0("agricheck_",check$check)
            checkdit<-paste0(issuedir,checkdir)
            if (! file.exists(checkdit)){dir.create(file.path(checkdit),showWarnings=FALSE)}
            
            for(cc in c) {
                checkfile<-gsub(" ","",paste0(check$check,"_",check$val1,"_vs_",check$val2,"_",check$obs,s,cat,"_",x,".csv"))
                checkfile<-gsub(" ","",paste0(check$check,"_",paste(cc,collapse="_"),"_",s,cat,"_",check$obs,".csv"))
                if(check$check=="NexRATE") checkfile<-gsub(" ","",paste0(check$check,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                if(check$check=="N2O-NIEF") checkfile<-gsub(" ","",paste0(check$check,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                if(check$check=="CLIMA") checkfile<-gsub(" ","",paste0(check$check,"_",paste(cc,collapse="_"),"_",check$obs,".csv"))
                    
                con <- file(paste0(checkdit,"/",checkfile), open="wt")
                writeLines(paste0("# Issue: ",issue), con)
                writeLines(paste0("# Observation: ",observation), con)
                writeLines(paste0("# Question: ",question,"\n#"), con)
                if(exists("extraline")) writeLines(extraline, con)
                if(check$check=="N2O-NIEF"){
                    testcat<-"Dairy Cattle"
                    testcat<-"Poultry"
                    testsec<-"3.B.2.4.7"
                    testy<-"1993"
                    t1<-data[data$measure=="Total N excreted"&data$party==cc&data$category==testcat,testy]
                    t2<-data[data$meastype=="NEXC"&data$source=="Pasture  range and paddock"&data$party==cc&data$category==testcat,testy]
                    t3<-data[data$meastype=="NEXC"&data$source=="Burned for fuel or as waste"&data$party==cc&data$category==testcat,testy]
                    t4<-sum(t1,-t2,-t3)
                    
                    cat(paste0("\n#Example for ",testy," and ",testcat,": Total manure handled: ", round(t4,1)," kg N/yr. "),file=con)
                    
                    t5<-data[data$meastype=="EM"&data$gas=="N2O"&data$sector_number==testsec&data$party==cc&data$category==testcat,testy]
                    cat(paste0("Total direct N2O emissions: ", round(t5,5)," kg N2O/yr. "),file=con)
                    cat(paste0("Emission factor wrt handled manure: ", round(t5/t4,6)," kg N2O/kg N handled (IEFN2=",round(t5/t4*28/44,6)," kg N2O-N/kg N handled). "),file=con)
                    
                    t6<-0
                    for (s in directsys){
                        t7<-data[data$source==s&data$meastype=="IEF"&data$sector_number=="3.B.2.5 N2O Emissions per MMS"&
                                     data$unit=="kg N2O/kg N handled"&data$party==cc,testy]
                        #print(t7)
                        if(is.numeric(t7))t7r<-round(t7,4)
                        #if(paste0("x",t7,"x")!="xx") {t7=0}
                        #if(paste0("x",t7,"x")!="xNOx") {t7=0}
                        t8<-data[data$source==s&data$meastype=="NEXC"&data$sector_number==testsec&data$category==testcat&data$party==cc,testy]
                        if(is.numeric(t8)) {
                            cat(paste0("Emission factor for ",s," ",t7r," kg N2O/kg N handled. "),file=con)
                            cat(paste0("Share of manure N handled in ",s," ",round(100*sum(t8)/t4,1),"%. "),file=con)
                        }
                        t6<-t6+sum(t8)/t4*sum(t7)
                        #print(paste0(cc,s,t6))
                    }
                    cat(paste0("Weighted N2O emission factor ",round(t6,6)," kg N2O/kg N handled (IEFN1=",round(t6*28/44,6)," kg N2O-N/kg N handled) ."),file=con)
                    cat(paste0("The two emission factors are different. Do you use different emission factors for MMS for different animal types?\n\n"),file=con)
                    
                    
                    #    selection<-data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$party==cc
                    #    writeLines("#\n# N in MMS and N2O emissions and IEFs per MMS",con)
                    #    writedata<-data[selection,reportfields]
                    #    o<-order(writedata$meastype,writedata$source)
                    #    writedata<-writedata[o,]
                    #    write.table(writedata,sep=",",col.names=FALSE,con)
                    #    writeLines("#",con)
                    #    
                } #else 
                writeLines(paste0("#\n#"), con)
                writeLines(paste0(",",paste(names(checkw),collapse=",")),con)
                for(cc in c[!c%in%eu]) {
                    if(check$check=="CPP and SO def IEFs"){
                        #selection<-data$sector_number=="3.D.1.3" & data$party==cc
                        curtab<-checkw[checkw$party==cc,]
                        write.table(curtab,sep=",",col.names=FALSE,con)
                        writeLines("#",con)
                    }else {
                        writeLines("# Relevant data for the identified animal types",con)
                        
                        catsok<-0
                        if(length(cats)>1) {catsok<-1} else if(cats!="Various"){catsok<-1}
                        if(check$check%in%c("Manure_grazing","N application ratio")){catsok<-0}
                        if(grepl("N in ",check$check)){catsok<-0}
                        #cat(cats,catsok)
                        if(catsok==1){
                            for(cat in cats){
                                curtab<-checkw[checkw$party==cc & checkw$category==cat,]
                                write.table(curtab,sep=",",col.names=FALSE,con)
                                writeLines("#",con)
                            }
                        }else{
                            curtab<-checkw[checkw$party==cc,]
                            checkw[checkw$party==cc,]
                            write.table(curtab,sep=",",col.names=FALSE,con)
                            writeLines("#",con)
                        }
                    }
                }
                
                ## Comparison with other countries
                if(check$check!="N2O-NIEF" & check$check!="CLIMA"){
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
    #print(as.character(check[,7]))
    #print((check[,7]))
    return(check)
}

