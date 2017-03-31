# Check 7: NH3 and NOx volatilized ####
print("Check 7: NH3 and NOx volatilized")

# FAM = annual amount of animal manure N applied to soils, kg N yr-1, Eqn 11.3
# The term FAMis determined by adjusting the amount of manure N available (NMMS_Avb; see Equation 10.34 in 
# Chapter 10) for the amount of managed manure used for feed (FracFEED), burned for fuel (FracFUEL), or used for 
# construction (FracCNST) as shown in Equation 11.4. 
# Equation 10.34 MANAGED MANURE NAVAILABLE FOR APPLICATION TO MANAGED SOILS,FEED,FUEL OR CONSTRUCTION USES
# .... = Sum over MMS and Livestock types of {
#        number of animals * Nexcretion rate * Share of animals in system}*
#            {1 - Fraction of N lost in MMS [FracLossMS= amount of managed manure nitrogen for livestock category 
#                 T that is lost in the manure management system S, % (see Table 10.23) ]}
#        + N in bedding added per MMS and animal type T time N content in bedding material
#        }
# Table 10.23: DEFAULT VALUES FOR TOTAL NITROGEN LOSS FROM MANURE MANAGEMENT
#   -- Table by animal tpye and MMS, gives Total N loss from MMS b FracLossMS(Range of FracLossMS) 
#      Footnote b: Total N loss rates based on judgement of IPCC ExpertGroup and following sources: 
#      Rotz ( 2003), Hutchings et al.(2001), and U.S EPA (2004). 
#      Rates include losses in forms of NH3, NOx, N2O, and N2 as well from leaching and runoff 
#      from solid storage and dry lots. Values represent average rates for typical housing and storage 
#      components without any significant nitrogen control measures in place. 
#      Ranges reflect values that appear in the literature. Where measures to control nitrogen losses 
#      are in place, alternative rates should be developed toreflect those measures. 


# Approach: generate List with matrices
# 
#
#         Managed          Pasture          Other            
# 
# CPP     manure[[1]][[1]] manure[[1]][[2]] manure[[1]][[3]] 
# SO      manure[[2]][[1]] manure[[2]][[2]] manure[[2]][[3]] 
#
#

tmp0<-extractuiddata(allagri,"",allcountries,noeu = TRUE)
allagritab<-unique(subset(allagri,select=uniquefields))
ccp<-c("Cattle","Swine","Poultry","Buffalo")
so<-c("Sheep",otherlivestock[-match(c("Other Livestock","Poultry","Buffalo","Other Other Livestock"),otherlivestock)])
for(i in c(ccp,so)){
    sel<-allagri$meastype=="TNEXC2"&grepl("^3.B.2",allagri$sector_number)& allagri$category==i
    curuid<-getuid(1,ok=1,mea="TNEXC2",sec="^3.B.2",cat=paste0("^",i,"$"))
    tmp2<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
    #if(i=="Poultry"){manurep<-tmp2}
    if(i%in%ccp){if(i==ccp[1]){manurecpp<-tmp2}else{manurecpp<-manurecpp+tmp2}}
    if(i%in%so){if(i==so[1]){manureso<-tmp2}else{manureso<-manureso+tmp2}}
}
#manureso<-manureso-manurepou

managedSys<-manureSysshrt[-match(c("pasture","burned"),manureSysshrt)]
tmplive<-otherlivestock[-match(c("Poultry","Other Livestock"),otherlivestock)]
otherSys<-c("burned")
for(i in c("Cattle","Swine","Poultry","Sheep",tmplive)){
    for(j in c(1:length(manureSystems))){
        cursystem<-manureSysshrt[j]
        icheck<-which(checkuids$category==i)
        
        # Total N excretion reported per animal type and MMS
        tmp2<-extractuiddata(allagri,checkuids[icheck,cursystem],allcountries,noeu = TRUE)
        
        if(cursystem=="pasture"){
            #if(i=="Poultry"){manurerprp<-tmp2}
            if(i%in%ccp){if(i==ccp[1]){manurerprccp<-tmp2}else{manurerprccp<-manurerprccp+tmp2}}
            if(i%in%so){if(i==so[1]){manurerprso<-tmp2}else{manurerprso<-manurerprso+tmp2}}
        }
        if(cursystem%in%managedSys){
            #if(i=="Poultry"){if(cursystem==managedSys[1]){manuremanp<-tmp2}else{manuremanp<-manuremanp+tmp2}}
            if(i%in%ccp){if(cursystem==managedSys[1] & i==ccp[1]){manuremanccp<-tmp2}else{manuremanccp<-manuremanccp+tmp2}}
            if(i%in%so){if(cursystem==managedSys[1] & i==so[1]){manuremanso<-tmp2}else{manuremanso<-manuremanso+tmp2}}
        }
        if(cursystem%in%otherSys){
            #if(i=="Poultry"){if(cursystem==otherSys[1]){manureothp<-tmp2}else{manureothp<-manureothp+tmp2}}
            if(i%in%ccp){if(cursystem==otherSys[1] & i==ccp[1]){manureothccp<-tmp2}else{manureothccp<-manureothccp+tmp2}}
            if(i%in%so){if(cursystem==otherSys[1] & i==so[1]){manureothso<-tmp2}else{manureothso<-manureothso+tmp2}}
        }
    }
}
#manurerprso<-manurerprso-manurerprp
#manuremanso<-manuremanso-manuremanp
#manureothso<-manureothso-manureothp

manurelcpp<-list(man=manuremanccp,prp=manurerprccp,oth=manureothccp,tot=manurecpp)
manurelsso<-list(man=manuremanso,prp=manurerprso,oth=manureothso,tot=manureso)
#manurelpou<-list(man=manuremanp,prp=manurerprp,oth=manureothp,tot=manurep)
#manure<-list(cpp=manurelcpp,so=manurelsso,pou=manurelpou)
manure<-list(cpp=manurelcpp,so=manurelsso)

# Compare the fraction not-applied in 3.D.1.2.a with the fraction

# Compare IEF in 3.D.1.3 and share of CPP and SO
sec<-"3.B.2.5 N2O Emissions per MMS"
cat<-"Farming"
sou<-"Pasture range and paddock"
totPRP<-manure[[1]][[2]]+manure[[2]][[2]]
uid<-newuid(sector = sec,categ = cat,meast = "NEXC",units = "kt N/year",metho = "",sourc = "",targe = "",optio = "",gasun = "no gas")
addallagriout<-add2allagri(totPRP,sec,cat,"no gas","kt N/year",sou,"","NEXC","",uid,"Total manure excreted on grazing in 3B2",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]

sec<-"3.D.1.3"
cat<-"Urine and Dung Deposited by Grazing CPP"
uid<-newuid(sector = sec,categ = cat,meast = "FracPRP_CPP",units = "1",metho = "",sourc = "",targe = "",optio = "",gasun = "")
addallagriout<-add2allagri(manure[[1]][[2]]/totPRP,sec,cat,"","1","","","FracPRP_CPP","",uid,"calc N in PRP by animal types",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]
cat<-"Urine and Dung Deposited by Grazing S+O"
uid<-newuid(sector = sec,categ = cat,meast = "FracPRP_SO",units = "1",metho = "",sourc = "",targe = "",optio = "",gasun = "")
addallagriout<-add2allagri(manure[[2]][[2]]/totPRP,sec,cat,"","1","","","FracPRP_SO","",uid,"calc N in PRP by animal types",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]

defIEF<-(0.02*manure[[1]][[2]]+0.01*manure[[2]][[2]])/totPRP
defIEF[is.nan(defIEF)]<-0
cat<-"Urine and Dung Deposited by Grazing Animals"
uid<-newuid(sector = sec,categ = cat,meast = "EF3_default",units = "kg N2O-N/kg N",metho = "",sourc = "",targe = "",optio = "",gasun = "")
addallagriout<-add2allagri(defIEF,sec,cat,"","1","","","EF3_default","",uid,"calc default EF3s times FracPRP_CPP and FracPRP_SO",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]


cursystem<-manureSysshrt[j]
icheck<-which(checkuids$category==i)

# manure on pasture, range and paddock shoudl equal activity data for 3.D.
check6<-checktemp
ncheck6<-1
test<-"Manure on grazing in 3.B.2 vs 3.D.1.3"
test<-"Manure_grazing"
if(sum(totPRP)!=0){
    
    curuid<-unique(allagri$variableUID[grepl("3.D.1.3",allagri$sector_number)&allagri$meastype=="AD"])
    if(length(curuid)==0) curuid<-newuid(sector = sec,categ = cat,meast = "AD",units = "kt N/year",metho = "",sourc = "",targe = "",optio = "",gasun = "no gas")
    tmpuid<-unique(allagri$variableUID[grepl("3.D.1.3",allagri$sector_number)&allagri$meastype=="EM"])
    tmp1<-extractuiddata(allagri,tmpuid,allcountries,noeu = TRUE)
    tmpuid<-unique(allagri$variableUID[grepl("3.D.1.3",allagri$sector_number)&allagri$meastype=="IEF"])
    tmp2<-extractuiddata(allagri,tmpuid,allcountries,noeu = TRUE)
    
    addallagriout<-add2allagri(tmp1/tmp2*28/44,sec,cat,"no gas","kt N/year","","","AD","",curuid,"AD for grazing animals missing",noeu=TRUE)
    allagri<-addallagriout[[1]]
    rowsadded<-addallagriout[[2]]
    
    diffmatout<-diffmatrix(checks=check6,ncheck=ncheck6,A=totPRP,B=tmp1/tmp2*28/44,test = test,
                           val1="3D13_EM*28/44/3D13_IEF",val2="AD",sec,cat,roundn=3)
    ncheck6<-diffmatout[[1]]
    check6<-diffmatout[[2]]
}    
if(nrow(check6)>1 & check6$val[1]!=0){
    check6b<-check6
    check6<-check6b
    check6$fac[is.na(check6$fac)]<-0
    check6<-simplifytestmatrix(check6,c("yr","fac","val"),list(years,"range",0))
    #check6<-simplifytestmatrix(check6,"ms",allcountries[!allcountries%in%eu])
    check6<-check6[order(check6$ms,check6$sec,check6$yr),names(check6)]
    check6<-Reduce(rbind,lapply(c(1:nrow(check6)),function(x) Reduce(cbind,reportchecks1(check=check6[x,checkname],data=allagri,x))))
    check6<-as.data.frame(check6)
    names(check6)<-checkname
    checks<-rbind(checks,check6)
}

# IEF in pasture range and paddock
#TABLE 11.1 DEFAULT EMISSION FACTORS TO ESTIMATE DIRECT N2O EMISSIONS FROM MANAGED SOILS
#EF3PRP, CPP for cattle (dairy, non-dairy and buffalo), poultry and pigs [kg N2O–N (kg N)-1] 0.02 0.007 - 0.06
#EF3PRP, SO for sheep and ‘other animals’ [kg N2O–N (kg N)-1] 0.01 0.003 - 0.03
curuid<-unique(allagri$variableUID[allagri$meastype=="IEF"&allagri$sector_number=="3.D.1.3"])
d13ief<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
check7<-checktemp
ncheck7<-1
test<-"IEF in 3D13 vs default IEFs from manure CPP and SO"
test<-"CPP and SO def IEFs"
if(sum(d13ief)!=0){
    diffmatout<-diffmatrix(checks=check7,ncheck=ncheck7,A=defIEF,B=d13ief,test,
                           val1="EF3ccp*fccp+EF3so*fso",val2="IEF",sec,cat,roundn=5)
    ncheck7<-diffmatout[[1]]
    check7<-diffmatout[[2]]
}    
if(nrow(check7)>1 & check7$val[1]!=0){
    check7b<-check7
    check7$fac[is.na(check7$fac)]<-0
    check7<-simplifytestmatrix(check7,c("yr","fac","val"),list(years,"range",0))
    check7<-simplifytestmatrix(check7,"ms",allcountries[!allcountries%in%eu])
    check7<-check7[order(check7$ms,check7$sec,check7$yr),names(check7)]
    check7c<-Reduce(rbind,lapply(c(1:nrow(check7)),function(x) Reduce(cbind,reportchecks1(check=check7[x,checkname],data=allagri,x))))
    check7c<-as.data.frame(check7c)
    names(check7c)<-checkname
    checks<-rbind(checks,check7c)
}

#TABLE 10.22 DEFAULT VALUES FOR NITROGEN LOSS DUE TO VOLATILISATION OF NH3 AND NOX FROM MANURE MANAGEMENT
# Lowest
# Dairy Cows Daily spread 7% (5 – 60)
# Other Solid storage 12% (5 – 20)
# Swine Pit storage 25% (15 – 30)
# Dairy Cow Dry lot 20% (10 – 35)
# Largest
# Poultry without litter 55% (40 – 70) 
# Swine Liquid/slurry 48% (15 – 60)
# Swine and Other Cattle Solid storage 45% (10 – 65)

# N lost in MMS as NH3+NOx or leached versus total Managed N (exception of PRP and manure burning)
curuid<-unique(allagri$variableUID[grepl("3.B.2.5",allagri$sector_number)&allagri$meastype=="Nvol"])
nvol<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)/1000000
curuid<-unique(allagri$variableUID[grepl("3.B.2.5",allagri$sector_number)&allagri$meastype=="Nleach"])
nlea<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)/1000000
nloss<-nvol+nlea

totmanaged<-manure[[1]][[1]]+manure[[2]][[1]]
totother<-manure[[1]][[3]]+manure[[2]][[3]]
nlossratio<-nvol/totmanaged

totnotlost<-totmanaged-nvol-nlea


sec<-"3.B.2.5"
cat<-"Farming"
gas<-"no gas"
meas<-"Nlossratio"
uid<-newuid(sector = sec,categ = cat,meast=meas,units = "1",metho = "",sourc = "",targe = "",optio = "",gasun = gas)
addallagriout<-add2allagri(nlossratio,sec,cat,gas,"1","","",meas,"",uid,"calc Nvol/(managed N)",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]
meas<-"Nmanaged"
uid<-newuid(sector = sec,categ = cat,meast=meas,units = "kt N/year",metho = "",sourc = "",targe = "",optio = "",gasun = gas)
addallagriout<-add2allagri(totmanaged,sec,cat,gas,"kt N/year","","",meas,"",uid,"MMS excl PRP and Burning",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]
meas<-"N_available"
uid<-newuid(sector = sec,categ = cat,meast=meas,units = "kt N/year",metho = "",sourc = "",targe = "",optio = "",gasun = gas)
addallagriout<-add2allagri(totnotlost,sec,cat,gas,"kt N/year","","",meas,"",uid,"MMS excl PRP and Burning minus N lost by volatilizatin and leaching",noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]

# The N loss ratio should be between 20% and 45% 
# (solid storage 'other' 12%, Cattle and Swine 30-45%,
#  liquid systems 40%-48%)
selection<-nlossratio<0.45 & nlossratio>0.2
nlossratio[nlossratio<0.45 & nlossratio>0.2]<-1
check8<-checktemp
names(check8)<-checkname
tmp<-tmp0
tmp[tmp==0]<-1
ncheck8<-1
test<-"N loss ratio"
if(sum(selection,na.rm=TRUE)!=0){
    diffmatout<-diffmatrix(checks=check8,ncheck=ncheck8,A=nlossratio,B=tmp,test,
                           val1="Nlossratio not between 0.2 and 0.45",val2="Nvol",sec,cat,roundn=3)
    ncheck8<-diffmatout[[1]]
    check8<-diffmatout[[2]]
}    
if(nrow(check8)>1 & check8$val[1]!=0){
    check8b<-check8
    check8$fac[is.na(check8$fac)]<-0
    check8<-simplifytestmatrix(check8,c("yr","fac","val"),list(years,"range",0))
    #check8<-simplifytestmatrix(check8,"ms",allcountries[!allcountries%in%eu])
    check8<-check8[order(check8$ms,check8$sec,check8$yr),names(check8)]
    check8c<-Reduce(rbind,lapply(c(1:nrow(check8)),function(x) Reduce(cbind,reportchecks1(check=check8[x,checkname],data=allagri,x))))
    check8c<-as.data.frame(check8c)
    names(check8c)<-checkname
    checks<-rbind(checks,check8c)
}






#xxx ... Calculate share liquid/slurry where Nlosstot = Nloss (Talbe 10.23 vs 10.22)
#xxx If this share is < 50% then a value <1.1 is unlikely, otherwise use 1.0 as threshold
sec<-"3.D.1.2.a"
cat<-"Animal Manure Applied to Soils"
meas<-"FracNavapp"
unt<-"1"
gas<-"no gas"
note<-"Fraction of N not lost from MMS as NH3+NOx vs. N applied in manure"
curuid<-unique(allagri$variableUID[grepl("3.D.1.2.a",allagri$sector_number)&allagri$meastype=="AD"])
FAM<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
Frac_Nav_app<-totnotlost/FAM
uid<-newuid(sector = sec,categ = cat,meast=meas,units = unt,metho = "",sourc = "",targe = "",optio = "",gasun = gas)
addallagriout<-add2allagri(Frac_Nav_app,sec,cat,gas,unt,"","",meas,"",uid,note,noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]


# Liquid/slurry and pit storage have no additional N2 losses (comparison Table 23 vs Table 22)
curuid<-unique(allagri$variableUID[allagri$source=="Liquid system"&grepl("3.B.2.5 N2O Emissions per MMS",allagri$sector_number)&allagri$meastype=="NEXC"])
liquid<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
curuid<-unique(allagri$variableUID[allagri$source=="Other"&grepl("3.B.2.5 N2O Emissions per MMS",allagri$sector_number)&allagri$meastype=="NEXC"])
other<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
liqshare<-(liquid+other)/totmanaged
sec<-"3.B.2.5 N2O Emissions per MMS"
cat<-"Farming"
meas<-"FracLiqOther"
unt<-"1"
gas<-"no gas"
note<-"Fraction of N managed in Liquid/Slurry and other MMS"
curuid<-unique(allagri$variableUID[grepl("3.D.1.2.a",allagri$sector_number)&allagri$meastype=="AD"])
FAM<-extractuiddata(allagri,curuid,allcountries,noeu = TRUE)
Frac_Nav_app<-totnotlost/FAM
uid<-newuid(sector = sec,categ = cat,meast=meas,units = unt,metho = "",sourc = "",targe = "",optio = "",gasun = gas)
addallagriout<-add2allagri(liqshare,sec,cat,gas,unt,"","",meas,"",uid,note,noeu=TRUE)
allagri<-addallagriout[[1]]
rowsadded<-addallagriout[[2]]


liqcrit<-liqshare
liqcrit[liqshare>0.5]<-1
liqcrit[liqshare<0.5]<-1.1
Frac_Nav_app[Frac_Nav_app>liqcrit]<-999

#xxx Explain with both Tables and the possibility of adding bedding material to explain the difference
#yyy Check below ha the same objective but is likly more complicated to explain.

check9<-checktemp
names(check9)<-checkname
tmp<-tmp0
tmp[tmp==0]<-999
ncheck9<-1
test<-"N application ratio"
sec<-"3.D.1.2.a"
cat<-"Animal Manure Applied to Soils"
if(sum(Frac_Nav_app,na.rm=TRUE)!=0){
    diffmatout<-diffmatrix(checks=check9,ncheck=ncheck9,A=Frac_Nav_app,B=tmp,test,
                           val1="Ratio Nnotlost vs FAM small",val2="FAM",sec,cat,roundn=1)
    ncheck9<-diffmatout[[1]]
    check9<-diffmatout[[2]]
}    
if(nrow(check9)>1 & check9$val[1]!=0){
    check9b<-check9
    check9$fac[is.na(check9$fac)]<-0
    check9<-simplifytestmatrix(check9,c("yr","fac","val"),list(years,"range",0))
    #check9<-simplifytestmatrix(check9,"ms",allcountries[!allcountries%in%eu])
    check9<-check9[order(check9$ms,check9$sec,check9$yr),names(check9)]
    check9c<-Reduce(rbind,lapply(c(1:nrow(check9)),function(x) Reduce(cbind,reportchecks1(check=check9[x,checkname],data=allagri,x))))
    check9c<-as.data.frame(check9c)
    names(check9c)<-checkname
    checks<-rbind(checks,check9c)
}

