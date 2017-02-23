
#checkuids<-allanimals
tmp0<-extractuiddata(allagri,"",allcountries,noeu = TRUE)
checkpar<-"NRATE"
checkuids[,checkpar]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
    getuid(1,ok=1,mea=checkpar,gas="no gas",x=x,
           sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
           cat=paste0("^",checkuids$category[x],"$"),
           msr="Nitrogen excretion rate")))
    

checkpar<-"TNEXC"
checkuids[,checkpar]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
    getuid(1,ok=1,mea="EM",gas="no gas",x=x,
           sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
           cat=paste0("^",checkuids$category[x],"$"))
    ))

# N2O emissions
mea<-"EM";gas<-"N2O";pargas<-paste0(mea,gas)
checkuids[,pargas]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
    getuid(1,ok=1,mea=mea,gas=gas,x=x,msr="Emissions",
           sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
           cat=paste0("^",checkuids$category[x],"$"))))
mea<-"IEF";gas<-"N2O";pargas<-paste0(mea,gas)
checkuids[,pargas]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
    getuid(1,ok=1,,mea=mea,gas=gas,x=x,msr="Implied",
           sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
           cat=paste0("^",checkuids$category[x],"$"))))

for(i in c(1:length(manureSystems))){
    
    cursystem<-manureSystems[i]
    curshort<-manureSysshrt[i]
    checkuids[,curshort]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
        getuid(1,ok=1,mea="NEXC",gas="no gas",x=x,
               sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
               cat=paste0("^",checkuids$category[x],"$"),sou=cursystem)))

    mea<-"EM";gas<-"N2O"
    cursystem<-manureSystems[i]
    curshort<-paste0(manureSysshrt[i],mea,gas)
    #save.image("getuid.RData")
    checkuids[,curshort]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
        getuid(1,ok=1,mea=mea,gas="N2O",x=x,sou=cursystem,
               sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
               cat=paste0("^",checkuids$category[x],"$"))))
    
    mea<-"IEF";gas<-"N2O"
    cursystem<-manureSystems[i]
    curshort<-paste0(manureSysshrt[i],mea,gas)
    checkuids[,curshort]<-unlist(lapply(c(1:nrow(checkuids)), function(x)
        getuid(1,ok=1,mea=mea,gas="no gas",x=x,sou=cursystem,
               sec=paste0("^3.B.2",checkuids$sector_number[x],"$"),
               cat=paste0("^",checkuids$category[x],"$"))))
}

checkuids$sector_number<-gsub(" $","",checkuids$sector_number)
checkuids<-checkuids[checkuids$sector_number!="",]
# Check sum of manureSystems against total excretion
# Testresults
tmp0<-extractuiddata(allagri,"",allcountries,noeu = TRUE)
#tmp0[is.na(tmp0)]<-0
check1<-checktemp
ncheck1<-1

check2<-checktemp;ncheck2<-1

# Check 1: Difference between sum(NEXC,mms) and TNEXC ####
print("Check 1: Difference between sum(NEXC,mms) and TNEXC")
print("Check 2: Population number and N-excretion rates")
for(i in c(1:nrow(checkuids))){
#for(i in c(51:51)){
    #print(i)
    tmp1<-tmp0
    for(j in manureSysshrt){
        if(checkuids[i,j]!=0){
            tmp<-extractuiddata(allagri,checkuids[i,j],allcountries,noeu = TRUE)
            tmp1<-tmp+tmp1
        }
    }
    
    # Total N excretion reported
    tmp2<-extractuiddata(allagri,checkuids[i,"TNEXC"],allcountries,noeu = TRUE)
    
    # Difference between sum(NEXC,mms) and TNEXC
    test1<-"NexTOT"
    val1<-"sum(mms)"
    val2<-"total"
    
    sec<-paste0("3.B.2",checkuids$sector_number[i])
    cat<-checkuids$category[i]
    if(sec!="3.B.2.5 N2O Emissions per MMS" & cat!="Other Sheep" & cat!="Other Swine"){
        diffmatout<-diffmatrix(check1,ncheck1,tmp1,tmp2,test1,val1,val2,sec,cat,roundn=0)
        ncheck1<-diffmatout[[1]]
        check1<-diffmatout[[2]]
    }
    
    # Population number and N-excretion rates
    test2<-"NexRATE"
    val1<-"sum(mms)"
    val2<-"AD x Nrate"
    if(checkuids[i,"APOP"]!=0){
        if(checkuids[i,"B2POP"]==0){
            tmp3<-extractuiddata(allagri,checkuids[i,"APOP"],allcountries,noeu = TRUE)
        }else{
            tmp3<-extractuiddata(allagri,checkuids[i,"B2POP"],allcountries,noeu = TRUE)
        }
        if(checkuids[i,"NRATE"]!=0){
            tmp4<-extractuiddata(allagri,checkuids[i,"NRATE"],allcountries,noeu = TRUE)
            tmp5<-1/1000 * tmp3 * tmp4
            
            # Difference between sum(NEXC,mms) and POP*Nrate 
            sec<-paste0("3.B.2",checkuids$sector_number[i])
            cat<-checkuids$category[i]
            diffmatout<-diffmatrix(check2,ncheck2,tmp1,tmp5,test2,val1,val2,sec,cat,roundn=0)
            ncheck2<-diffmatout[[1]]
            check2<-diffmatout[[2]]
            if(ncheck2>1 & check2$test[1]!=0){
                uid<-newuid(sec,cat,"TNEXC2","kt N/year",metho="",sourc="",targe="",optio="",gasun="")
                addallagriout<-add2allagri(tmp5,sec,cat,"","kt N/year","","","TNEXC2","",uid,"calc POP and Nrate",noeu=TRUE)
                allagri<-addallagriout[[1]]
                rowsadded<-addallagriout[[2]]
                if(rowsadded>0) checkuids[i,"TNEXC2"]<-uid
            }
        }
    }
}


#check1$sec<-gsub("Other ","",check1$sec)
#check1<-unique(check1)
if(nrow(check1)>1 & !is.na(check1$val[1])){
    check1b<-check1
    check1<-check1b
    check1<-simplifytestmatrix(check1,c("yr","fac","val"),list(years,"range",0))
    #check1<-simplifytestmatrix(check1,"ms",allcountries[!allcountries%in%eu])
    check1<-check1[order(check1$ms,check1$sec,check1$yr),names(check1)]
    check1<-Reduce(rbind,lapply(c(1:nrow(check1)),function(x) Reduce(cbind,reportchecks1(check=check1[x,checkname],data=allagri,x))))
    check1<-as.data.frame(check1)
    names(check1)<-checkname[]
    checks<-rbind(checks,check1)
}
#check2<-simplifytestmatrix(check2,c("yr","fac","val"),list(years,"range",0))
#check2<-simplifytestmatrix(check2,"ms",allcountries[!allcountries%in%eu])
if(nrow(check2)>1 & !is.na(check2$val[1])){
    check2b<-check2
    
    check2<-check2b
    #check2<-simplifytestmatrix(check2,c("yr","fac","sec","cat","val"),list(years,"range",0,0,""))
    check2<-simplifytestmatrix(check2,c("yr","fac","val"),list(years,"range",""))
    check2<-check2[order(check2$ms,check2$sec,check2$yr),names(check2)]
    check2<-Reduce(rbind,lapply(c(1:nrow(check2)),function(x) Reduce(cbind,reportchecks1(check=check2[x,checkname],data=allagri,x))))
    check2<-as.data.frame(check2)
    names(check2)<-checkname
    checks$temp<-as.vector(checks$val2)
    checks$val2<-checks$temp
    
    checks<-rbind(checks[,checkname],check2)
}
# Check 3: Sum of N handled in MMS over animal type vs. total N handled ####
print("Check 3: Sum of N handled in MMS over animal type vs. total N handled")
checkuids$sector_number[checkuids$sector_number==".4."]<-".4"
check3<-checktemp
ncheck3<-1
names(check3)<-checkname
mainanimals<-c("Cattle","Swine","Sheep","Other Livestock")
for(j in manureSysshrt[!manureSysshrt %in% "pasture"]){
    tmp1<-tmp0
    for(i in mainanimals){
        k<-which(checkuids$sector_number%in%c(".1",".2",".3",".4") & checkuids$category==i)
        if(checkuids[k,j]!=0){
            tmp1<-tmp1+extractuiddata(allagri,checkuids[k,j],allcountries,noeu = TRUE)
        }
    }
    
    # Total N excretion reported
    icheck<-which(grepl(".5 N2O Emissions per MMS",checkuids$sector_number))
    tmp2<-extractuiddata(allagri,checkuids[icheck,j],allcountries,noeu = TRUE)
    test<-paste0("N in ",j)
    sec<-paste0("3.B.2",checkuids$sector_number[icheck])
    cat<-checkuids$category[i]
    #   diffmatout<-diffmatrix(check2,ncheck2,tmp1,tmp5,test,val1,val2,testsec,roundn=0)
    diffmatout<-diffmatrix(check3,ncheck3,tmp1,tmp2,test,"sum(animals)","total",sec,cat,roundn=0)
    ncheck3<-diffmatout[[1]]
    check3<-diffmatout[[2]]
}
if(nrow(check3)>1 & !is.na(check3$val[1])){
    #check3<-simplifytestmatrix(check3,c("yr","fac","val"),list(years,"range",0))
    #check3<-simplifytestmatrix(check3,"ms",allcountries[!allcountries%in%eu])
    check3$cat[is.na(check3$cat)]<-0
    #check3<-simplifytestmatrix(check3,c("ms","yr","fac","val"),list(allcountries[!allcountries%in%eu],years,"range",""))
    check3<-simplifytestmatrix(check3,c("yr","fac","val"),list(years,"range",""))
    check3<-check3[order(check3$ms,check3$sec,check3$yr),names(check3)]
    check3<-Reduce(rbind,lapply(c(1:nrow(check3)),function(x) Reduce(cbind,reportchecks1(check=check3[x,checkname],data=allagri,x))))
    check3<-as.data.frame(check3)
    names(check3)<-checkname
    checks<-rbind(checks,check3)
}


# Check 4: IEF per MMS ####
print("Check 4: IEF per MMS")
check4<-checktemp
names(check4)<-checkname
ncheck4<-1
for(j in c(1:length(manureSystems))){
    
    cursystem<-manureSysshrt[j]
    curem<-paste0(manureSysshrt[j],"EMN2O")
    curief<-paste0(manureSysshrt[j],"IEFN2O")
    icheck<-which(grepl(".5 N2O Emissions per MMS",checkuids$sector_number))
    
    # Total N excretion reported
    tmp2<-extractuiddata(allagri,checkuids[icheck,cursystem],allcountries,noeu = TRUE)
    
    # Total N2O emissions
    tmp3<-extractuiddata(allagri,checkuids[icheck,curem],allcountries,noeu = TRUE)
    
    # N2O IEF
    tmp4<-extractuiddata(allagri,checkuids[icheck,curief],allcountries,noeu = TRUE)
    tmp6<-tmp4
    tmp6[is.na(tmp6)]<-0
    
    # Calculate IEF from N available and N2O emissions
    # Convert N in MMS from kg to Gg (already done with unit-conversion)
    # Convert N2O emissions into N2O-N emissions
    tmp5<-(tmp3*28/44)/(tmp2)
    if(checkuids[icheck,curem]!=0){if(unique(allagri$unit[allagri$variableUID==checkuids[icheck,curem]])=="Mt"){tmp5<-tmp5*1000}}
    
    test<-paste0("N2O-IEF in ",cursystem)
    sec<-paste0("3.B.2",checkuids$sector_number[icheck])
    cat<-checkuids$category[icheck]
    if(sum(tmp6)!=0){
        diffmatout<-diffmatrix(checks=check4,ncheck=ncheck4,A=tmp5,B=tmp4,test,
                               val1="N2OEm/TOTNEXC",val2="IEF",sec,cat,roundn=0)
        ncheck4<-diffmatout[[1]]
        check4<-diffmatout[[2]]
    }else{
        diffmatout<-nodiff(check4,ncheck4,test,"N2OEm/TOTNEXC","IEF",sec,cat,"No N2O-IEF reported")
        ncheck4<-diffmatout[[1]]
        check4<-diffmatout[[2]]
        uid<-newuid(sec,cat,"IEF","kg N2O-N/kg N handled",metho="",sourc=manureSystems[j],targe="",optio="",gasun="N2O")
        addallagriout<-add2allagri(tmp5,sec,cat,"N2O","kg N2O-N/kg N handled",manureSystems[j],"","IEF","Implied Emission Factor",uid,"calc from EM and Totel N handled",noeu=TRUE)
        allagri<-addallagriout[[1]]
        rowsadded<-addallagriout[[2]]
        if(rowsadded>0) checkuids[icheck,curief]<-uid
    }
}
if(nrow(check4)>1 & !is.na(check4$val[1])){
    check4b<-check4
    check4$fac[is.na(check4$fac)]<-0
    check4<-simplifytestmatrix(check4,c("yr","fac","val"),list(years,"range",0))
    #check4<-simplifytestmatrix(check4,"ms",allcountries[!allcountries%in%eu])
    check4<-check4[order(check4$ms,check4$sec,check4$yr),names(check4)]
    check4<-Reduce(rbind,lapply(c(1:nrow(check4)),function(x) Reduce(cbind,reportchecks1(check=check4[x,checkname],data=allagri,x))))
    check4<-as.data.frame(check4)
    names(check4)<-checkname
    checks<-rbind(checks,check4)
}

# Check 5: Calculate IEFs per animal type and related to N excreted ####
print("Check 5: Calculate IEFs per animal type and related to N excreted")
test<-paste0("N2O-NIEF")
check5<-checktemp
ncheck5<-1
for(i in c(1:nrow(checkuids))){
    #tmp1 = Total N excretion per animal type (measure: Total N excreted, meastype: EM)
    #tmp1 1 [kt N/yr]
    tmp1<-extractuiddata(allagri,checkuids[i,"TNEXC"],allcountries,noeu = TRUE)
    tmp1past<-extractuiddata(allagri,checkuids[i,"pasture"],allcountries,noeu = TRUE)
    tmp1burn<-extractuiddata(allagri,checkuids[i,"burned"],allcountries,noeu = TRUE)
    tmp1<-tmp1-tmp1past-tmp1burn
    
    if(sum(tmp1)>0){
        tmp4<-tmp0
        for(j in c(1:length(manureSystems))){
            
            # Calculate weighted IEF by share of N in MMS times IEF for MMS
            cursystem<-manureSysshrt[j]
            tmp2<-tmp0
            tmp3<-tmp0
            icheck<-which(grepl(".5 N2O Emissions per MMS",checkuids$sector_number))
            if(checkuids[i,manureSysshrt[j]]!=0) tmp2<-extractuiddata(allagri,checkuids[i,manureSysshrt[j]],allcountries,noeu = TRUE)
            if(checkuids[icheck,paste0(manureSysshrt[j],"IEFN2O")]!=0) tmp3<-extractuiddata(allagri,checkuids[icheck,paste0(manureSysshrt[j],"IEFN2O")],allcountries,noeu = TRUE)
            if(sum(tmp2)>0 & sum(tmp3)>0) {
                #tmp1 [kt N/yr]: Total N excretion of animal type
                #tmp2 [kt N/yr]: N-excretion per MMS and animaltype; measure: Nitrogen excretion per MMS, meastype: NEXC
                #tmp3 [kg N2O-N/kg N handled in MMS] Implied N2O-EF per MMS; method: calc from EM and Totel N handled, meastype: IEF
                # !! Note tmp3: The CRFs give the unit 'kg N2O-N/kg N handled' but in fact values are 'kg N2O/kg N handled'
                
                #tmp4 [kg N2O-N/kg N handled for animal type]: 
                #      Implied N2o-EF, calculated as the weighted average of MMS-specific IEFs 
                #      (calculated above if not existing before)
                #      with the share of manure handled in the MMS by animal type.
                #print(paste(cursystem,tmp2[7,24],tmp3[7,24],tmp4[7,24],sep="-"))
                tmp4<-tmp4+tmp2/tmp1*tmp3
                tmp4[is.nan(tmp4)]<-0
            }
        }
        if(sum(tmp4)>0){
            sec<-paste0("3.B.2",checkuids$sector_number[i])
            cat<-checkuids$category[i]
            uid<-newuid(sec,cat,"IEFN1","kg N2O-N/kg N excreted",metho="",sourc="",targe="",optio="",gasun="N2O")
            addallagriout<-add2allagri(tmp4,sec,cat,"N2O","kg N2O-N/kg N excreted","","","IEFN1","Implied Emission Factor",uid,"calc as weighted average over MMS-IEFs",force=1,noeu=TRUE)
            allagri<-addallagriout[[1]]
            rowsadded<-addallagriout[[2]]
            if(rowsadded>0) checkuids[i,"IEFNN2O1"]<-uid
            checkuids[is.na(checkuids)]<-0
            
            #tmp1 = Total N excretion per animal type (measure: Total N excreted, meastype: EM)
            #tmp1 1 [kt N/yr]
            #tmp5 [kt N2O/yr for animal type] total N2O emissions as reported for animal types
            tmp5<-extractuiddata(allagri,checkuids[i,"EMN2O"],allcountries,noeu = TRUE)
            
            
            tmp6<-(tmp5*28/44)/(tmp1)

            uid<-newuid(sec,cat,"IEFN2","kg N2O-N/kg N excreted",metho="",sourc="",targe="",optio="",gasun="N2O")
            addallagriout<-add2allagri(tmp6,sec,cat,"N2O","kg N2O-N/kg N excreted","","","IEFN2","Implied Emission Factor",uid,"calc from Total N excreted and Emissions",force=1,noeu=TRUE)
            allagri<-addallagriout[[1]]
            rowsadded<-addallagriout[[2]]
            if(rowsadded>0) checkuids[i,"IEFNN2O2"]<-uid
            checkuids[is.na(checkuids)]<-0
            
            #print(paste(tmp5[7,24],tmp6[7,24],sep="-"))
            if(checkuids[icheck,curem]!=0){if(unique(allagri$unit[allagri$variableUID==checkuids[i,"EMN2O"]])=="Mt"){tmp6<-tmp6*1000}}
            diffmatout<-diffmatrix(check5,ncheck5,tmp6,tmp4,test,"N2OEm_TOTNEXC","N-IEF (calculated)",sec,cat,roundn=5)
            ncheck5<-diffmatout[[1]]
            check5<-diffmatout[[2]]
        }
    }
}
check5b<-check5
if(nrow(check5)>1 & !is.na(check5$val[1])){
    check5<-check5b
    #check5<-simplifytestmatrix(check5,c("ms","yr","fac","val"),list(allcountries[!allcountries%in%eu],years,"range",""))
    #check5<-simplifytestmatrix(check5,c("yr","fac","sec","cat","val"),list(years,"range",0,0,""))
    check5<-simplifytestmatrix(check5,c("yr","fac","val"),list(years,"range",""))
    check5<-check5[order(check5$sec,check5$cat,check5$ms,check5$yr),names(check5)]
    check5<-Reduce(rbind,lapply(c(1:nrow(check5)),function(x) Reduce(cbind,reportchecks1(check=check5[x,checkname],data=allagri,x))))
    check5<-as.data.frame(check5)
    names(check5)<-checkname
    checks<-rbind(checks,check5)
}

# Check 6: sum of climate allocations ####
climfields<-c("sector_number","category","classification","option","party")
allclimate<-allagri[allagri$meastype=="CLIMA",c(climfields,"target","source",years)]
allclimate<-allclimate[!allclimate$party%in%eu,]
allclimateval<-allclimate[,years]
allclimatedat<-allclimate[,climfields]
allclimateagg<-aggregate(allclimateval,by=as.list(allclimatedat),function(x) sum(x))
v<-unique(which(round(allclimateagg[,years],1)!=100,arr.ind = TRUE)[,1])
climcheck<-allclimateagg[v,]
climcheck<-climcheck[!(climcheck$party=="DE"&climcheck$category=="Buffalo"),]

#check6<-data.frame(matrix(rep(0,ncol(checktemp)*nrow(climcheck)),ncol=ncol(checktemp),nrow=nrow(climcheck)))
#names(check6)<-checkname
names(climcheck)[1:5]<-c("sector_number","category","classification","option","party")
names(climcheck)[1:5]<-c("sec","cat","classification","option","ms")
climcheck$check<-"CLIMA"
climcheck[,c("val","val1","val2","yr")]<-""
climcheck$obs<-"sum alloc over mms and climate not 100"
climcheck$fac<-apply(climcheck[,years],1,mean)
#climcheck<-simplifytestmatrix(climcheck,c("sec","cat"),list(0,0))
climcheck<-climcheck[order(climcheck$sec,climcheck$cat,climcheck$ms,climcheck$yr),names(climcheck)]
ms<-climcheck$ms
checkname<-gsub("test","check",checkname)
climcheck[,checkname]<-Reduce(rbind,lapply(c(1:nrow(climcheck)),function(x) Reduce(cbind,reportchecks1(check=climcheck[x,checkname],data=allagri,x))))
climcheck<-as.data.frame(climcheck)
climcheck$ms<-ms

climcheck[,c("sector_number","category","party","years","range","plot")]<-climcheck[,c("sec","cat","ms","yr","fac","val")]
climcheck<-climcheck[,-which(names(climcheck)%in%c("sec","cat","ms","yr","fac","val"))]

#names(check6)<-checkname
#checks<-rbind(checks,check6)


# Write out list of issues ####
checks$correction<-1
checks[,resolved]<-""
#checks[,docfields]<-""
sel<-grepl("agrichecks",checks$val)
checks$val<-paste0("=HYPERLINK(\"",checks$val,"\")")
#call<-names(checks)
#c<-c(call[1:which(call=="val")],"correction",resolved,docfields,call[(which(call=="val")+1):length(call)])
#checks<-checks[,c]
