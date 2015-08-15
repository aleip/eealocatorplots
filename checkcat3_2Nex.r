
#checkuids<-allanimals
tmp0<-extractuiddata(cat3all,"",allcountries)
checkpar<-"NEXC"
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

# Check sum of manureSystems against total excretion
# Testresults
tmp0<-extractuiddata(cat3all,"",allcountries)
#tmp0[is.na(tmp0)]<-0
check1<-checktemp
names(check1)<-checkname
ncheck1<-1

check2<-checktemp
ncheck2<-1
names(check2)<-checkname

print("Check1: Difference between sum(NEXC,mms) and TNEXC")
print("Check2: Population number and N-excretion rates")
for(i in c(1:nrow(checkuids))){
    tmp1<-tmp0
    for(j in manureSysshrt){
        if(checkuids[i,j]!=0){
            tmp<-extractuiddata(cat3all,checkuids[i,j],allcountries)
            tmp1<-tmp+tmp1
        }
    }
    
    # Total N excretion reported
    tmp2<-extractuiddata(cat3all,checkuids[i,"TNEXC"],allcountries)
    
    # Difference between sum(NEXC,mms) and TNEXC
    test<-"Nex"
    val1<-"sum(mms)"
    val2<-"total"
    
    testsec<-paste0("3.B.2",checkuids$sector_number[i]," ",checkuids$allmethods[i])
    diffmatout<-diffmatrix(check1,ncheck1,tmp1,tmp2,test,val1,val2,testsec,roundn=0)
    ncheck1<-diffmatout[[1]]
    check1<-diffmatout[[2]]

    # Population number and N-excretion rates
    test<-"Nex"
    val1<-"sum(mms)"
    val2<-"AD x Nrate"
    if(checkuids[i,"APOP"]!=0){
        if(checkuids[i,"B2POP"]==0){
            tmp3<-extractuiddata(cat3all,checkuids[i,"APOP"],allcountries)
        }else{
            tmp3<-extractuiddata(cat3all,checkuids[i,"B2POP"],allcountries)
        }
        if(checkuids[i,"NEXC"]!=0){
            tmp4<-extractuiddata(cat3all,checkuids[i,"NEXC"],allcountries)
            tmp5<-1000 * tmp3 * tmp4
            
            # Difference between sum(NEXC,mms) and POP*Nrate 
            testsec<-paste0("3.B.2",checkuids$sector_number[i]," ",checkuids$allmethods[i])
            diffmatout<-diffmatrix(check2,ncheck2,tmp1,tmp5,test,val1,val2,testsec,roundn=0)
            ncheck2<-diffmatout[[1]]
            check2<-diffmatout[[2]]
        }else{
            if(sum(tmp1,na.rm=TRUE)!=0){
                check2[ncheck2,"test"]<-test
                check2[ncheck2,"val1"]<-val1
                check2[ncheck2,"val2"]<-val2
                check2[ncheck2,"ms"]<-"all"
                check2[ncheck2,"yr"]<-"all"
                check2[ncheck2,"sec"]<-testsec
                check2[ncheck2,"val"]<-""
                check2[ncheck2,"obs"]<-"No N excr. rates"
                ncheck2<-ncheck2+1
            }
        }
    }
}
#Simplifytestmatrix
nullv<-NULL
check1<-simplifytestmatrix(check1,"yr",years)
#check1<-simplifytestmatrix(check1,"ms",as.vector(unlist(allcountries)))
check1<-check1[order(check1$ms,check1$sec,check1$yr),names(check1)]

check2<-simplifytestmatrix(check2,"yr",years)
#check2<-simplifytestmatrix(check2,"ms",as.vector(unlist(allcountries)))
check2<-check2[order(check2$ms,check2$sec,check2$yr),names(check2)]


print("Check3; Sum of N handled in MMS over animal type vs. total N handled")
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
            tmp1<-tmp1+extractuiddata(cat3all,checkuids[k,j],allcountries)
        }
    }
    
    # Total N excretion reported
    icheck<-which(grepl(".5 N2O Emissions per MMS",checkuids$sector_number))
    tmp2<-extractuiddata(cat3all,checkuids[icheck,j],allcountries)
    
    seccheck<-paste0("3.B.2",checkuids$sector_number[icheck])
#   diffmatout<-diffmatrix(check2,ncheck2,tmp1,tmp5,test,val1,val2,testsec,roundn=0)
    diffmatout<-diffmatrix(check3,ncheck3,tmp1,tmp2,paste0("N in ",j),"sum(animals)","total",seccheck,roundn=0)
    ncheck3<-diffmatout[[1]]
    check3<-diffmatout[[2]]
}
check3<-simplifytestmatrix(check3,"yr",years)
check3<-check3[order(check3$ms,check3$sec,check3$yr),names(check3)]
#stop("test")

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
    tmp2<-extractuiddata(cat3all,checkuids[icheck,cursystem],allcountries)
    
    # Total N2O emissions
    tmp3<-extractuiddata(cat3all,checkuids[icheck,curem],allcountries)
    
    # N2O IEF
    tmp4<-extractuiddata(cat3all,checkuids[icheck,curief],allcountries)
    tmp6<-tmp4
    tmp6[is.na(tmp6)]<-0
    
    
    # Calculate IEF from N available and N2O emissions
    # Convert N in MMS from kg to Gg
    # Convert N2O emissions into N2O-N emissions
    tmp5<-(tmp3*28/44)/(tmp2/1000000)
    
    
    seccheck<-paste0("3.B.2",checkuids$sector_number[icheck])
    if(sum(tmp6)!=0){
        diffmatout<-diffmatrix(check4,ncheck4,tmp5,tmp4,paste0("N2O-IEF in ",cursystem),"N2OEm/TOTNEXC (converted)","IEF",seccheck,roundn=0)
        ncheck4<-diffmatout[[1]]
        check4<-diffmatout[[2]]
    }else{
        diffmatout<-nodiff(check4,ncheck4,paste0("N2O-IEF in ",cursystem),"N2OEm/TOTNEXC (converted)","IEF",seccheck,"No N2O-IEF reported")
        ncheck4<-diffmatout[[1]]
        check4<-diffmatout[[2]]
        uid<-newuid()
        addcat3allout<-add2cat3all(tmp5,seccheck,"N2O","kg N2O-N/kg N handled",manureSystems[j],"","IEF",uid)
        cat3all<-addcat3allout[[1]]
        rowsadded<-addcat3allout[[2]]
        if(rowsadded>0) checkuids[icheck,curief]<-uid
    }
}
check4<-simplifytestmatrix(check4,"yr",years)
check4<-check4[order(check4$ms,check4$sec,check4$yr),names(check4)]

print("Check 5: Calculate IEFs per animal type and related to N excreted")
check5<-checktemp
names(check5)<-checkname
ncheck5<-1
for(i in c(1:nrow(checkuids))){
    
    tmp1<-extractuiddata(cat3all,checkuids[i,"TNEXC"],allcountries)
    tmp4<-tmp0
    
    if(sum(tmp1)>0){
        for(j in c(1:length(manureSystems))){
            
            # Calculate weighted IEF by share of N in MMS times IEF for MMS
            cursystem<-manureSysshrt[j]
            icheck<-which(grepl(".5 N2O Emissions per MMS",checkuids$sector_number))
            if(checkuids[i,manureSysshrt[j]]!=0) tmp2<-extractuiddata(cat3all,checkuids[i,manureSysshrt[j]],allcountries)
            if(checkuids[icheck,paste0(manureSysshrt[j],"IEFN2O")]!=0) tmp3<-extractuiddata(cat3all,checkuids[icheck,paste0(manureSysshrt[j],"IEFN2O")],allcountries)
            if(sum(tmp2)>0 & sum(tmp3)>0) {
                tmp4<-tmp4+tmp2/tmp1*tmp3
                tmp4[is.nan(tmp4)]<-0
            }
        }
        if(sum(tmp4)>0){
            uid<-newuid()
            addcat3allout<-add2cat3all(tmp4,checkuids$sector_number[i],"N2O","kg N2O-N/kg N excreted","","","IEFN",uid)
            cat3all<-addcat3allout[[1]]
            rowsadded<-addcat3allout[[2]]
            if(rowsadded>0) checkuids[i,"IEFNN2O"]<-uid
            checkuids[is.na(checkuids)]<-0
            
            tmp5<-extractuiddata(cat3all,checkuids[i,"EMN2O"],allcountries)
            tmp6<-(tmp5*28/44)/(tmp1/1000000)
            seccheck<-paste0("3.B.2",checkuids$sector_number[i])
            diffmatout<-diffmatrix(check5,ncheck5,tmp6,tmp4,paste0("N2O-NIEF for"),"N2OEm/TOTNEXC (converted)","N-IEF (calculated)",seccheck,roundn=5)
            ncheck5<-diffmatout[[1]]
            check5<-diffmatout[[2]]
        }
    }
}
check5<-simplifytestmatrix(check5,"yr",years)
check5<-check5[order(check5$ms,check5$sec,check5$yr),names(check5)]
