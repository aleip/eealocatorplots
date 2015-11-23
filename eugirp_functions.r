# see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

view<-function(D){View(D)}
viewlast<-function(n,allagri=allagri){View(allagri[(nrow(allagri)-n):nrow(allagri),])}
newuid<-function(){paste("EUGIRP",gsub("2015","15",cursubm),"-",format(Sys.time(),"%Y%m%d-%H%M.%S"),"-",MHmakeRandomString(1,6),sep="")}
firstup<-function(string){
    rstring<-tolower(string)
    rstring<-paste0(toupper(substr(rstring,0,1)),substr(rstring,2,nchar(rstring)))
}
uidall<-function(D,uid){D[D$variableUID%in%uid,]}
viewuid<-function(D,uid){
    View(uidall(D,uid),
         unique(paste0(D$sector_number[D$variableUID==uid],D$category[D$variableUID==uid])))
}

matbyind<-function(D,v){
    # Extract the data identified from which with option arr.ind=TRUE into a matrix
    if(nrow(v)>0){M<-unlist(lapply(c(1:nrow(v)),function(y) D[v[y,1],v[y,2]]))}else{M<-0}
    return(M)
}
whichmatrix<-function(D,v){
    # Puts the results from 'which' with option arr.ind=TRUE back into the matrix 
    M<-D
    if(is.vector(v)){
        M[]<-0
        if(length(v)>0){
            for(y in c(1:length(v))){
                M[v[y]]<-D[v[y]]
            }
        }
    }else{
        if(nrow(v)>0){
            vals<-matbyind(D,v)
            M[,]<-0
            for(y in c(1:nrow(v))){
                M[v[y,1],v[y,2]]<-vals[y]
            }
        }
    }
    return(M)
}

getuid<-function(mode=1,ok=1,x=1,sec="*",cat="*",met="*",cla="*",sou="*",tar="*",opt="*",msr="*",mea="*",gas="*"){
    
    # mode=1: returns number of UIDs available
    # mode=2: returns UIDs if #ist 1
    
    # Note all info a re set to default "*" which means that all are selected
    #      only those which are of interest need to be given and only if different
    selc<-c("sec","cat","met","cla","sou","tar","opt","msr","mea","gas")
    selv<-c(sec,cat,met,cla,sou,tar,opt,msr,mea,gas)
    sel<-"";for(i in c(1:length(selv))) {g<-paste0(selc[i],"<-'",selv[i],"'");sel<-paste(sel,g,sep=";")}
    
    #print(sel)
    #         getuidr<-nrow(allagritab[allagritab$sector_number==paste0(sec,myobject$sector_number[x]) &
    #                             allagritab$allmethods==met &
    #                             allagritab$clim==cli &
    #                             allagritab$meastype==mymeastype &
    #                             allagritab$gas==gas,])
    getuidr<-allagritab[grepl(sec,allagritab$sector_number) &
                            grepl(cat,allagritab$category) &
                            grepl(met,allagritab$method) &
                            grepl(cla,allagritab$classification) &
                            grepl(sou,allagritab$source) &
                            grepl(tar,allagritab$target) &
                            grepl(opt,allagritab$option) &
                            grepl(msr,allagritab$measure) &
                            grepl(mea,allagritab$meastype) &
                            grepl(gas,allagritab$gas),]
    ngetuidr<-nrow(getuidr)
    curuids<-getuidr$variableUID
    
    if(ngetuidr>1){View(allagri[allagri$variableUID%in%curuids,]);stop()}
    if(ngetuidr==1){ngetuidr<-unlist(as.vector(curuids))}
    return(ngetuidr)
}

brief<-function(text){
    
    # Provides short names for items (sector_name,measure,method,...) in CRF
    
    if(text=="Atmospheric Deposition") {br<-"AtmDep"}else
    if(text=="Nitrogen Leaching and Run-off") {br<-"NLR"}else
    if(text=="Organic N Fertilizers") {br<-"OrgN"}else
    if(text=="Inorganic N Fertilizers") {br<-"InorgN"}else
    if(text=="3.B.2.5 N2O Emissions per MMS") {br<-"MMS"}else
    if(text=="no gas") {br<-""}else
    if(text=="Aggregate GHGs") {br<-"GHG"}else
    if(text=="Option A") {br<-"A"}else
    if(text=="Option B") {br<-"B"}else
    if(text=="Option C") {br<-"C"}else
    {br<-text}
}

linkto<-function(text){text<-paste0("=HYPERLINK(\"",text,"\")")}

calculateshares<-function(uid,curval,totalval){
    # Calculate the share of a time series (curval) for all MS against a give total (totalval)
    val<-extractuiddata(DF = curval,uid = uid,c = allcountries,narm = FALSE)
    res<-val/totalval
    res<-as.data.frame(res)
    res$party<-allcountries
    res$variableUID<-uid
    #stop()
    return(res)
}

sumovercountries<-function(D,uid,y,c){
    y<-as.character(y)
    s<-matrix(0,ncol=length(y))
    m<-matrix(0,ncol=length(y),nrow=length(c))
    m<-D[D$variableUID==uid,y]
    s<-apply(m,2,sum)
    return(s)
}

eu28sums<-function(A){
    agrimeas<-unique(subset(A,select=allfields[!allfields %in% c("notation","party",years,"option")]))
    agri2sum<-agrimeas[agrimeas$meastype %in% meas2sum,]
    removeeu28<-A$meastype %in% meas2sum & A$party=="EU28"
    A<-A[!removeeu28,]
    
    eu28sum<-as.data.frame(matrix(rep(0,ncol(A)*nrow(agri2sum)),
                                  ncol=ncol(A),nrow=nrow(agri2sum)))
    names(eu28sum)<-names(A)
    eu28sum[,names(agri2sum)]<-agri2sum[,names(agri2sum)]
    eu28sum[,years]<-euvalue("sum",eu28sum,A,years,countriesic)
    eu28sum[,"party"]<-rep("EU28",nrow(eu28sum))
    eu28sum$notation[eu28sum$notation==0]<-""
    eu28sum$option[eu28sum$option==0]<-""
    A<-rbind(A,eu28sum)
    return(A)
}


weightovercountries<-function(D,Auid,Puid,ok,y,c){
    
    # Returns the weighted average over all countries
    # for which both AD and a Value for the variable exist.
    # In case a country as a value (e.g. IEF) but does not 
    # report Ad, then this is excluded from the EU weighted average!
    
    #print(paste0("Auid<-",Auid))
    #print(paste0("Puid<-",Puid))
    #print(paste0("ok<-",ok))
    Auid<-as.vector(unlist(Auid))
    Puid<-as.vector(unlist(Puid))
    if(ok=="-" | ok=="" | grepl("^[1-9]",ok)){
        s<-rep(NA,length(y))
    }else{
        y<-as.character(y)
        s<-matrix(0,ncol=length(y))
        ad<-matrix(0,ncol=length(y),nrow=length(c))
        pa<-ad
        ad<-extractuiddata(D,Auid,c,narm = FALSE)
        pa<-extractuiddata(D,Puid,c,narm = FALSE)
        
        ad<-ad[!is.na(apply(pa,1,sum,rm.na=TRUE)),]
        pa<-pa[!is.na(apply(pa,1,sum,rm.na=TRUE)),]
        if(length(pa)<length(c)){
            ad<-t(ad)
            pa<-t(pa)
        }
        
        pa<-pa[!is.na(apply(ad,1,sum,rm.na=TRUE)),]
        ad<-ad[!is.na(apply(ad,1,sum,rm.na=TRUE)),]
        
        if(length(pa)==0){
            s<-rep(NA,length(y))
        }else{
            if(length(pa)<length(c)){
                ad<-t(ad)
                pa<-t(pa)
            }
            
            if(nrow(ad)>0 & sum(apply(ad,2,sum))!=0 ){
                m<-ad*pa
                s<-apply(m,2,sum)/apply(ad,2,sum)
            }else if(nrow(ad)>0 & sum(apply(ad,2,sum))==0){
                #Calculate average
                s<-apply(pa,2,mean)   
            }else{
                s<-0
            }
        }
    }
#if(is.nan(s)) s<-0
    #if(is.na(s)) s<-0
    return(s)
}

euvalue<-function(todo,E,D,y,c){
    if(todo=="sum")l<-lapply(c(1:nrow(E)),function(x) sumovercountries(D,E$variableUID[x],y,c))
    if(todo=="weight"){
        
        l<-lapply(c(1:nrow(E)),function(x) weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c))
    }
    #m<-matrix(unlist(l),ncol=length(y),byrow=T)
    m<-t(Reduce(cbind,l))
}

# checkuid<-function(myobject=NULL,sec=NULL,met="",sou="",cat="",mea=NULL,gas="no gas"){
#     
#     pargas<-paste0(mea,gas)
#     
#     myobject[,pargas]<-unlist(lapply(c(1:nrow(myobject)), function(x)
#         getuid(1,ok=1,mea=mea,gas=gas,x=x,cat=cat,
#                sec=paste0("^",sec,checkuids$sector_number[x],"$"))))
# #         getuid(1,myobject,ok=1,sec,met,cli,mea,gas,x)))
# #     myobject[,pargas]<-unlist(lapply(c(1:nrow(myobject)), function(x) 
# #         getuid(2,myobject,ok=myobject[x,pargas],sec,met,cli,mea,gas,x)))
#     
#     return(myobject)
# }

convert2co2eq<-function(line){
    #Converts data into CO2eq 
    #Line needs to have just 'unit','gas' and the value-columns!!
    g<-as.character(line$gas)
    u<-as.character(line$unit)
    y<-names(line)[!names(line)%in%c("gas","unit")]
    convert<-gwps[which(gases==g)]
    if(u=="kt CO2 equivalent") convert<-1
    line[,y]<-line[,y]*convert
    return(list(g,"kt CO2 equivalent",line[,y]))
}

extractuiddata<-function(DF=NULL,uid=NULL,c,narm=TRUE){
    c<-as.data.frame(c)
    names(c)<-"party"
    #DF<-droplevels(DF)
    #if(length(levels(DF$variableUID))<length(levels(uid))) levels(DF$variableUID)<-levels(uid)
    tmp1<-unique(DF[DF[,"variableUID"]==uid,c("party",years)])
    tmp1<-tmp1[! is.na(tmp1$party),]
    
    ntmp<-nrow(tmp1)
    tmp1<-merge(c,tmp1,by="party",all=TRUE,sort=TRUE)
    tmp1<-tmp1[tmp1$party %in% c$party,]
    
    eucountries<-c("EU28","EU29")
    x<-tmp1[order(tmp1$party),]
    tmp1<-rbind(x[!(x$party %in% eucountries),],x[ (x$party %in% eucountries),])

    tmp1<-tmp1[,years]
    if(narm) tmp1[is.na(tmp1)]<-0
    tmp1<-as.matrix(tmp1)
    return(tmp1)
}

nodiff<-function(checks=NULL,ncheck=1,test=NULL,val1=NULL,val2=NULL,sec=NULL,cat=NULL,reas=""){
    checks[ncheck,"test"]<-test
    checks[ncheck,"val1"]<-val1
    checks[ncheck,"val2"]<-val2
    checks[ncheck,"ms"]<-"all"
    checks[ncheck,"yr"]<-"all"
    checks[ncheck,"sec"]<-sec
    checks[ncheck,"cat"]<-cat
    checks[ncheck,"val"]<-""
    checks[ncheck,"obs"]<-reas
    ncheck<-ncheck+1
    return(list(ncheck,checks))
}

diffmatrix<-function(checks=NULL,ncheck=1,A=NULL,B=NULL,test=NULL,val1=NULL,val2=NULL,sec=NULL,cat=NULL,roundn=3){
    diff=NULL
    diff<-which(round(A,roundn)!=round(B,roundn))
    if(length(diff)>0){
        for(d in c(1:length(diff))){
            rn<-diff[d] %% nrow(A)
            if(rn==0){rn=nrow(A)}
            cn<-ceiling(diff[d]/nrow(A))
            checks[ncheck,"test"]<-test
            checks[ncheck,"val1"]<-val1
            checks[ncheck,"val2"]<-val2
            checks[ncheck,"ms"]<-allcountries[rn]
            checks[ncheck,"yr"]<-years[cn]
            checks[ncheck,"sec"]<-sec
            checks[ncheck,"cat"]<-cat
            
            # Check if the values differ by orders of magnitude
            quotient<-log10(A[rn,cn]/B[rn,cn])
            obs<-""
            val<-paste0(round(A[rn,cn],roundn+2),"/",round(B[rn,cn],roundn+2))
            
            if(is.infinite(quotient)){
                obs<-paste0(val2," is not reported")
                val<-""
                fac<-0
            }else{
                #check order of mag of quotient
                quotord<-round(quotient,0)
                #Check if quotient is a multiple of 10
                if(round(quotient,2)==10^quotord){
                    cat("\n1",quotient,quotord)
                    obs<-paste0(val1," is 10^fac x ",val2)
                    val<-""
                    fac<-round(quotient,3)
                }else{
                    quotient<-round(A[rn,cn]/B[rn,cn],3)
                    if(quotient==0) quotient<-round(round(A[rn,cn],roundn)/round(B[rn,cn],roundn),3+3)
                    obs<-paste0("val1 is fac x val2")
                    fac<-round(quotient,3)
                    #cat("\nelse",quotient,fac)
                }
            }
            checks[ncheck,"val"]<-val
            checks[ncheck,"obs"]<-obs
            checks[ncheck,"fac"]<-fac
            ncheck<-ncheck+1
        }
    }
    return(list(ncheck,checks))
}

reportyears<-function(checkyx,compare){

    # Required to write out issues
    
    # Function group the years into a string
    #  - Returns "all" if all years in "years"
    #  - Returns the missing years if less than half are missin
    #  - Returns the selected years if less then half are selectd
    
    # If there are multiple columns compare must be passed as list
    if(! is.list(compare)) compare<-list(compare)
    
    # If there are multiple columns to simplify, then checkyx is a dataframe
    if(! is.data.frame(checkyx)) checkyx<-data.frame(checkyx)
    nchecks<-ncol(checkyx)
    
    if(length(compare)<nchecks){
        for(addcompare in c((length(compare)+1):nchecks)){
            compare[[addcompare]]<-0
        }
    }

    #print(paste0("checkyx=",checkyx))
    #print(paste0("years",maxn,"-",compare))
    for(docheck in c(1:nchecks)){
        # No comparison -> then the values will be returned as string
        # In case 'yr' was already compbined split again
        curcheckyx<-checkyx[,docheck]
        if(is.list(checkyx[,docheck])) curcheckyx<-unique(unlist(lapply(checkyx[,docheck],as.character)))
        #if(is.character(curcheckyx)) {curcheckyx<-unlist(strsplit(curcheckyx," "))}
        #if(group[docheck]=="yr"){curcheckyx<-unique(as.numeric(lapply(curcheckyx,as.character)))}
        curn<-length(curcheckyx)
        maxn<-length(compare[[docheck]])
        if(maxn==1){
            if(compare[[docheck]]==0){
                ret<-paste0(gsub(" ","_",curcheckyx),collapse=" ")
            }else if(compare[[docheck]]=="range"){
                curcheckyx<-as.numeric(curcheckyx)
                rmin<-min(curcheckyx,na.rm=TRUE)
                rmax<-max(curcheckyx,na.rm=TRUE)
                if(rmax!=rmin) {ret<-paste0("range: ",rmin,"-",rmax)}else{
                    ret<-paste0("val:",rmin)
                }
            }else if(compare[[docheck]]==""){
                ret<-""
            }
        }else{
            if(maxn==curn){
                ret<-"all"
            }else if(maxn==0){
                ret<-""
            }else if(curn<maxn/2){
                ret<-paste0(curcheckyx,collapse=" ")
            }else{
                misy<-paste0(compare[[docheck]][! compare[[docheck]] %in% curcheckyx],collapse=" ")
                ret<-paste0("all except: ",misy,collapse=" ")
            }
        }
        if(docheck==1) {retl<-list(ret)}else{retl<-append(retl,list(ret))}
    }
    #print(paste("return=",ret))
    return(retl)
}

simplifytestmatrix<-function(check,group,compare=0){
    # group: column which will be grouped.
    #        Note: this works also with a vector of columns
    # sorting: order of columns in return 
    
    testheaders<-names(check)
    check3<-subset(check,select=names(check)[! names(check) %in% group])
    checky<-as.data.frame(check[,group])
    
    #here row.names warning...
    checky<-aggregate(checky, by = as.list(check3), function(x) paste0(x,collapse=NULL))
    ncheck<-length(group)
    checkn<-c(ncol(checky)-ncheck+1:ncheck)
    #checky[,group]<-unlist(lapply(c(1:nrow(checky)),function(x) reportyears(checky$x[x],compare)))
    checky[,group]<-Reduce(rbind,(lapply(c(1:nrow(checky)),function(x) 
        Reduce(cbind,reportyears(checky[x,checkn],compare))
        )))
    check<-checky[,names(checky)%in%testheaders]
    
    return(check)
    
}

add2allagri<-function(matrix,sec="",cat="",gas="",unit="",sou="",tar="",mea="",msr="",uid="",note="",force=0,DATA=data.frame()){
    # Note this adds a new row only if it does not exist 
    # Existing rows will not be overwritten.
    
    curdata<-allagri
    if(nrow(DATA)>0) curdata<-DATA
    
    agruemp<-as.data.frame(matrix(rep("",ncol(curdata)),nrow=1,ncol=ncol(curdata)),stringsAsFactors = FALSE)
    names(agruemp)<-names(curdata)
    agruemp[,years]<-rep(0,length(years))
    
    exists<-nrow(curdata[curdata$sector_number==sec & curdata$category==cat & curdata$gas==gas & curdata$unit==unit &
                             curdata$source==sou & curdata$meastype==mea,])
    
    addr<-1
    if(exists==0 | force==1){
        if(note=="") note<-"calc"
        addrow<-agruemp
        addrow$sector_number[1]<-sec
        addrow$category[1]<-cat
        addrow$gas[1]<-gas
        addrow$unit[1]<-unit
        addrow$source[1]<-sou
        addrow$target[1]<-tar
        addrow$meastype[1]<-mea
        addrow$measure[1]<-msr
        addrow$variableUID[1]<-uid
        addrow$method[1]<-note
        addrow$notation[1]<-"x"
        
        addnewrow<-addrow
        
        for(i in c(1:nrow(matrix))){
            if(sum(matrix[i,],na.rm=TRUE)!=0){
                addnewrow[addr,]<-addrow
                #print(i)
                #View(addnewrow)
                addnewrow$party[addr]<-allcountries[i]
                for (j in c(1:length(years))){
                    if(is.nan(matrix[i,j])){
                        addnewrow[addr,years[j]]<-0
                    }else{
                        addnewrow[addr,years[j]]<-matrix[i,j]
                    }
                }
                addr<-addr+1
            }
        }
        addr<-addr-1
        #addnewrow[addnewrow[addr,uniquefields]==0,uniquefields]<-""
        if(addr>0) {
            levels(curdata$unit)<-c(levels(curdata$unit),unit)
            #levels(curdata$variableUID)<-c(levels(curdata$variableUID),uid)
            #levels(curdata$source)<-c(levels(curdata$source),sou)
        }
        if(addr>0) curdata<-rbind(curdata,addnewrow)
    }else{
        #print("exists")
    }
    return(list(curdata,addr))
}

ispotentialissue<-function(line,S,signyear,signthreshold){
    # Check if an issue identified as outlier is potentially significant
    median<-line$median
    y<-line[,signyear]
    lastyr<-line$years
    sect<-line$sector_number
    categ<-line$category
    targ<-line$target
    meas<-line$meastype
    type<-line$type
    significant<-0
    effect<-"no"
    if(sect=="3.D.AI.1") {sect<-"3.D.2.1";categ<-"Farming"; targ<-"Atmospheric Deposition"}
    if(sect=="3.D.AI.2") {sect<-"3.D.2.1";categ<-"Farming"; targ<-"Atmospheric Deposition"}
    if(sect=="3.D.AI.3") {sect<-"3.D.2.2";categ<-"Farming"; targ<-"Nitrogen Leaching and Run-off"}
    if(sect=="3.A.4.7" & meas=="WEIGHT") {sect<-"3.B.1.4.7"}
    if(targ%in%climateZones){targ<-""}
    if(type=="Organic amendments added"){type<-""}
    
    # Check if 2013 is included in the outliers
    if(lastyr=="all"){
        lastyr<-1
    }else if(grepl("except",lastyr)){
        if(signyear%in%unlist(strsplit(gsub("all except: ","",lastyr)," "))){
            lastyr<-0
        }else{
            lastyr<-1
        }
    }else if(signyear%in%unlist(strsplit(lastyr," "))){
        lastyr<-1
    }else{
        lastyr<-0
    }
    # In case 2013 is not included because it is zero add to the list
    if(lastyr==0 & y==0) lastyr<-1
    
    # Calculate value relative to median
    # retrieve the share in 2013 (or the max share if 2013 value is zero)
    selection<-S$sector_number==sect & S$category==categ & 
        S$target==targ & S$method==line$method & S$option==line$option &
        S$type==type & S$source==line$source & S$party==line$party
    if(sum(selection)==0){
        lastyr<-0
        relmedian<-0
        share<-"no emissions reported"
        if(line$party=="IS") share<-"Island excluded from check"
    }else{
        if(y!=0){
            relmedian<-y/median
            share<-mean(S[selection,signyear])
        }else{
            relmedian<-line$value/median
            share<-mean(S[selection,"maxshare"])
        }
        if(meas%in%c("PREGNANT","MILK","ORGAMENDMENT","DM","RatioResCrop","WEIGHT","YIELD")){
            note<-"measure excluded"
        }else if(meas%in%c("MCF")){
            note<-"Share of MMS needs to be assessed"
        }else{
            if(relmedian>1){
                #Potential overestimation
                effect<-(relmedian-1)/relmedian*share
                if(effect>signthreshold & lastyr==1) significant<-"over"
            }else{
                effect<-share/relmedian
                if(effect>signthreshold & lastyr==1) significant<-"under"
            }
        }
    }
    note<-""
    if(significant%in%c("over","under")){
        if(meas%in%c("GE","GEav")){note<-"CH4-EF for enteric fermentation direct proportional from Gross Energy Intake (IPCC2006, Equation 10.21)"}
        if(meas%in%c("YM")){note<-"CH4-EF for enteric fermentation direct proportional from Gross Energy Intake (IPCC2006, Equatioo 10.21)"}
        if(meas%in%c("FracGASF","FracGASM")){note<-"Indirect N2O emissions from volatilization are direct proportional to the Volatilization fractions (IPCC2006, Equation 11.9)"}
        if(meas%in%c("FracLEACH")){note<-"Indirect N2O emissions from leaching are direct proportional to the Leaching Fractions (IPCC2006, Equation 11.10)"}
        if(meas%in%c("NRATE")){note<-"Nitrogen Excretion Rate is important for several down-stream direct and indirect N2O emissions"}
        if(meas%in%c("VSEXC")){note<-"CH4 emissions from manure management direct proportional to Volatile Solid Excretion (IPCC2006, Equation 10.23)"}
    }
    return(list(significant,effect,relmedian,lastyr,share,note))
}
source("eugirp_writeissues.r")

# MHmakeRandomString(n, length)
# function generates a random string random string of the
# length (length), made up of numbers, small and capital letters
# see https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/

MHmakeRandomString <- function(n=1, lenght=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                        lenght, replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}

#  > MHmakeRandomString()
#  [1] "XM2xjggXX19r"



# FUNCTIONS REQUIRED FOR PLOTTING #####
source("eugirp_funemplot.r")
source("eugirp_funnirplots.r")
makepie<-function(piedata,pieradius=0.9,piename,piegrep=""){
    
    
    print(piegrep)
    piedata<-piedata[grepl(piegrep,piedata$sector_number),]
    if(nrow(piedata)>0){
        plotformat<-"jpg"
        piewidth=3*2
        pieheight=2
        piefont<-1.6*pieheight/6
        pieresolution=1000
        figdir<-gsub("checks","plots",issuedir)
        if (! file.exists(gsub("/$","",figdir))){
            dir.create(file.path(figdir))
            #    setwd(file.path(mainDir, figdate))
        }
        
        piegen<-agrigeneu
        piegen[,years]<-t(apply(piegen[,years],1,"/",apply(piegen[,years],2,sum)))
        select<-piegen[,lastyear]<0.01
        temp<-piegen[nrow(piegen),]
        temp$sector_number<-"Other"
        temp$category<-""
        temp[,years]<-apply(piegen[select,years],2,sum)
        piegen<-piegen[!select,]
        piegen<-rbind(piegen,temp)
        v<-piegen$sector_number
        n<-nrow(piegen)
        lin<-rep(1,n)
        lin<-rep(0.9,n)
        lin[which(v==piegrep)]<-0.6
        
        figname<-paste0(figdir,"/",cursubm,"agrimixeu",piegrep,".",plotformat,collapse=NULL)
        if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight)
        if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
        if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
        par(mfrow = c(1,2))
        par(omd=c(0,1,0,1))
        par(mar=c(0,0,0,0)) #bot,lef,top,rig
        par(lwd=0.5)
        pie(piegen[,lastyear],init.angle=180,
            radius=pieradius,
            label=paste(piegen$sector_number," ",100*round(piegen[,lastyear],3),"%"),
            clockwise=TRUE,
            col=grey(lin),
            cex=piefont
        )
        #graphics.off()
        
        
        piedata[,years]<-t(apply(piedata[,years],1,"/",apply(piedata[,years],2,sum)))
        select<-piedata[,lastyear]<0.01
        temp<-piedata[nrow(piedata),]
        temp$sector_number<-"Other"
        temp$category<-""
        temp[,years]<-apply(piedata[select,years],2,sum)
        piedata<-piedata[!select,]
        if(temp[,lastyear]>0.05) piedata<-rbind(piedata,temp)
        
        #figname<-paste0(figdir,"/",cursubm,piename,piegrep,".",plotformat,collapse=NULL)
        #if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight)
        #if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
        #if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
        
        #par(omd=c(0,1,0,1))
        #par(mar=c(0,0,0,0)) #bot,lef,top,rig
        par(lwd=0.5)
        pie(piedata[,lastyear],init.angle=180,
            radius=pieradius,
            label=paste(piedata$sector_number," ",100*round(piedata[,lastyear],3),"%"),
            clockwise=TRUE,col=gray(seq(0.3, 1.0, length = nrow(piedata))),
            cex=piefont
        )
        graphics.off()
    }
    return(1)
    
}



multilines<-function(text2split,maxWidth=30){
    #text2split: text
    vtext<-strwrap(text2split,1)
    nchartext<-lapply(c(1:length(vtext)), function(x) 1+ nchar(vtext[x]))
    nchartext<-ceiling(cumsum(nchartext) / maxWidth)
    restext<-c(1:max(nchartext))
    for(i in c(1:max(nchartext))){
        restext[i]<-paste(vtext[nchartext==i],collapse=" ")
    }
    return(restext)
}


gcCount <-  function(line, char){
    chars = strsplit(as.character(line),"")[[1]]
    length(which(tolower(chars) == char))
}