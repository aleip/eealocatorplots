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
yearheaders<-function(curhead){
    sel<-which(curhead%in%paste0("X",years))
    curhead[sel]<-years
    return(curhead)
}
convert2char<-function(DF,cols=NULL){
    
    if(is.null(cols)){
        for(i in 1:ncol(DF)) if(is.factor(DF[,i])) DF[,i]<-as.character(DF[,i])
    }
    return(DF)
}
convert2num<-function(DF,cols=NULL){
    
        for(i in 1:length(cols)) {
            curcol<-which(names(DF)==cols[i])
            DF[,curcol]<-as.numeric(DF[,curcol])
        }
    return(DF)
}
filldf<-function(DF,cols=allcheckfields){
    
    missing<-cols[!cols%in%names(DF)]
    nmissing<-length(missing)
    
    test0<-matrix(rep(0,nrow(DF)*nmissing),ncol=nmissing,nrow=nrow(DF))
    test0<-as.data.frame(test0)
    names(test0)<-missing
    DF<-cbind(DF,test0)
    
    return(DF)
    
}
last<-function(years){last<-years[length(years)]}
curdate<-function(){format(Sys.time(), "%Y%m%d")}
curtime<-function(){format(Sys.time(), "%Y%m%d-%H%M")}
rounddigit<-function(val){
    sig<-val/abs(val)
    n<-log10(abs(val))
    
    digs<-2-min(2,floor(n))
    valn<-round(val,digs)
    return(valn)
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
    #print(curuids)
    if(ngetuidr>1){View(allagri[allagri$variableUID%in%curuids,]);
                   ngetuidr<-paste0(ngetuidr,": ",paste(unlist(as.vector(curuids)),collapse=","))}
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
selquantiles<-function(D){
    #View(D)
    quantls<-c(0.25, 0.5, 0.75)
    Dnozero<-matbyind(D = D,v = which(D!=0,arr.ind = TRUE))
    mi<-min(Dnozero)
    mx<-max(Dnozero)
    q<-(as.vector(quantile(Dnozero,probs=quantls,na.rm=TRUE)))
    z<-c(mi,q,mx)
    return(z)
}
selmeansd<-function(D){
    #View(D)
    quantls<-c(0.5)
    Dnozero<-matbyind(D = D,v = which(D!=0,arr.ind = TRUE))
    mi<-min(Dnozero)
    mx<-max(Dnozero)
    mean<-mean(Dnozero)
    std<-sd(Dnozero)
    q<-(as.vector(quantile(Dnozero,probs=quantls,na.rm=TRUE)))
    z<-c(mean,std)
    return(z)
}
splitintoregions<-function(x,ref){
    # example usage: splitintoregions(c("2000","2001","2005","2006","2011","2012","2013"),years)
    
    #Which of ref are in x
    x1<-match(x,ref)
    #Calculate the difference
    x2<-diff(x1)
    #print(x2)
    #All those which are different from one separate regions
    x3<-as.vector(which(x2!=1))
    #print(length(x3))
    nreg<-length(x3)
    if(nreg>0){
        for(i in c(1:nreg)){
            if(i==1) {
                tv<-x1[1:x3[i]]
                v<-list(ref[tv])
            }else{
                tv<-x1[(x3[i-1]+1):x3[i]]
                v[[i]]<-ref[tv]
            }
        }
        tv<-x1[(x3[nreg]+1):length(x)]
        v[[nreg+1]]<-ref[tv]
    }
    
    return(v)
}


sumovercountries<-function(D,uid,y,c){
    y<-as.character(y)
    s<-matrix(0,ncol=length(y))
    m<-matrix(0,ncol=length(y),nrow=length(c))
    m<-D[D$variableUID==uid,y]
    s<-apply(m,2,sum,na.rm=T)
    return(s)
}

eu28sums<-function(A){
    agrimeas<-unique(subset(A,select=allfields[!allfields %in% c("notation","party",years,"option")]))
    agri2sum<-agrimeas[agrimeas$meastype %in% meas2sum,]
    removeeu28<-A$meastype %in% meas2sum & A$party%in%c("EU28",excludeparty)
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

getvariableUID<-function(DF=allagri,sec=NULL,cat=NULL,mt=NULL,gas=NULL){
    
    selection<-rep(1,nrow(DF))
    if(!is.null(sec)) {selection<-selection & DF$sector_number==sec}
    if(!is.null(cat)) selection<-selection & DF$category==cat
    if(!is.null(mt)) selection<-selection & DF$meastype==mt
    if(!is.null(gas)) selection<-selection & DF$gas==gas
    
    vuid<-as.character(unique(DF$variableUID[selection]))
    if(length(vuid)>1) {
        return(paste(length(vuid),": ",paste(vuid,collapse=";")))
    }else  if(length(vuid)==0){
        return(0)
    }else{
        return(vuid)
    }
}

fillbyvariableUID<-function(DF=allagri,col,uid){
    #cval<-as.character(unique(DF[DF$variableUID==uid,col]))
    f<-function(DF,col,uid){
        cval<-unique(as.character(DF[DF$variableUID==uid,col]))
        if(length(cval)==0) cval=""
        return(cval)
    }
    cval<-unlist(lapply(1:length(col),function(x) f(DF,col[x],uid)))
    return(cval)
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
                if(is.nan(quotient)){quotient=0;quotord<-0}
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
        checkname<-names(checkyx[docheck])
        curcheckyx<-checkyx[,docheck]
        if(is.list(checkyx[,docheck])) curcheckyx<-unique(unlist(lapply(checkyx[,docheck],as.character)))
        #if(is.character(curcheckyx)) {curcheckyx<-unlist(strsplit(curcheckyx," "))}
        #if(group[docheck]=="yr"){curcheckyx<-unique(as.numeric(lapply(curcheckyx,as.character)))}
        curn<-length(curcheckyx)
        maxn<-length(compare[[docheck]])
        if(maxn==1){
            if(compare[[docheck]]==0){
                ret<-paste(curcheckyx,collapse=",")
                if(checkname=="sec" & curn>2){
                    ret<-substr(curcheckyx[1],1,nchar(curcheckyx[1])-2)
                }
                if(checkname=="cat" & curn>2){
                    ret<-"Various"
                }
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

convertyear<-function(yrs){
    if(yrs=="all"){
        yremrt<-paste0("1990-",lastyear)
    }else if(grepl("except",yrs)){
        yremrt<-as.numeric(unlist(strsplit(gsub("all except: ","",yrs)," ")[[1]]))
        yremrt<-yearsnum[!yearsnum%in%yremrt]
        rmin<-min(yremrt,na.rm=TRUE)
        rmax<-max(yremrt,na.rm=TRUE)
        if(rmax!=rmin) {yremrt<-paste0(rmin,"-",rmax)}else{yremrt==rmin}
    }else{
        yremrt<-as.numeric(unlist(strsplit(yrs," ")[[1]]))
        rmin<-min(yremrt,na.rm=TRUE)
        rmax<-max(yremrt,na.rm=TRUE)
        if(rmax!=rmin) {yremrt<-paste0(rmin,"-",rmax)}else{yremrt==rmin}
    }
    return(yremrt)
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
    # Method: (a) Significant issue only if the last year is included in the list of outliers
    #         (b) Checks if the difference to using the median reported value is significant
    #             acc. to the significance threshold. Uses data frame significantcategories
    #             calculated earlier
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
        #if(line$party=="IS") share<-"Island excluded from check"
    }else{
        if(y!=0){
            relmedian<-y/median
            share<-mean(S[selection,signyear])
        }
        else{
            relmedian<-line$value/median
            share<-mean(S[selection,"maxshare"])
        }
    
        if(meas%in%c("PREGNANT","MILK","ORGAMENDMENT","DM","RatioResCrop","WEIGHT","YIELD")){
            note<-"measure excluded"
        }else if(meas%in%c("MCF")){
            note<-"Share of MMS needs to be assessed"
        }else{
            # Flags: over --> issue is a potential overestimation
            #        overnotlast ---> issue is a potential overestimation but last year is not identified
            #        oversource ---> issue interests a significant source category, but overestimation might be below threshold
            #        ... in analogy for 'under'
            if(relmedian>1){
                #Potential overestimation
                effect<-(relmedian-1)/relmedian*share
                if(effect>signthreshold){
                    if(lastyr==1){
                        significant<-"over"
                    }else if(lastyr==0){
                        significant<-"overnotlast"
                    }
                }else if(share>signthreshold){
                    significant<-"oversource"
                }
            }else if(relmedian<1){
                effect<-share/relmedian
                if(effect>signthreshold){
                    if(lastyr==1){
                        significant<-"under"
                    }else if(lastyr==0){
                        significant<-"undernotlast"
                    }
                }else if(share>signthreshold){
                    significant<-"oversource"
                }
            }
        }
    }
    note<-""
    if(grepl("over|under",significant)){
        if(meas%in%c("GE","GEav")){note<-"CH4-EF for enteric fermentation direct proportional from Gross Energy Intake (IPCC2006, Equation 10.21)"}
        if(meas%in%c("YM")){note<-"CH4-EF for enteric fermentation direct proportional from Gross Energy Intake (IPCC2006, Equatioo 10.21)"}
        if(meas%in%c("FracGASF","FracGASM")){note<-"Indirect N2O emissions from volatilization are direct proportional to the Volatilization fractions (IPCC2006, Equation 11.9)"}
        if(meas%in%c("FracLEACH")){note<-"Indirect N2O emissions from leaching are direct proportional to the Leaching Fractions (IPCC2006, Equation 11.10)"}
        if(meas%in%c("NRATE")){note<-"Nitrogen Excretion Rate is important for several down-stream direct and indirect N2O emissions"}
        if(meas%in%c("VSEXC")){note<-"CH4 emissions from manure management direct proportional to Volatile Solid Excretion (IPCC2006, Equation 10.23)"}
    }
    return(list(significant,effect,relmedian,lastyr,share,note))
}
checkhierarchy<-function(D,sec,dig){
    #Attention - unfinished .. should be part of keysource analysis
    Dsub1<-D[grepl(paste0("^",sec),D$sector_number)&D$digit==dig,]
    Dagg1<-aggregate(Dsub1[years],by=list(Dsub1$party),sum)
    Dsub2<-D[grepl(paste0("^",sec),D$sector_number)&D$digit==dig+1,]
    Dagg2<-aggregate(Dsub2[years],by=list(Dsub2$party),sum)
}

source("eugirp_writeissues.r")


loadipccdefaults<-function(D,xfr,xto,insert=NULL){
    ipccdefaults<-read.csv(file="ipcc_defaults.csv")
    ipccdefaults$notation<-""
    ipccdefaults[ipccdefaults==" "]<-""
    ipccdefaults<-ipccdefaults[,-which(names(ipccdefaults)=="variableUID")]
    ipccdefaults<-ipccdefaults[,-which(names(ipccdefaults)=="X")]
    
    paramnames<-names(D)
    paramfields<-uniquefields[uniquefields%in%paramnames]
    paramfields<-paramfields[paramfields!="variableUID"]
    D<-D[xfr:xto,]
    
    ipcc<-Reduce(rbind,lapply(c(1:nrow(D)),function(x) t(adddefaults(D[x,paramfields],x,ipccdefaults))))
    ipcc<-as.data.frame(ipcc)
    names(ipcc)<-ipccfields
    D<-cbind(D,ipcc)
    if(length(insert)>0) {
        ninsert<-which(paramnames=="value")
        o<-c(paramnames[1:ninsert],names(ipcc),paramnames[(ninsert+1):length(paramnames)])
        D<-D[,o]
    }
    
    return(D)
}

multidefaults<-function(D,keep,ignore){
    def<-unique(D[,which(!names(D)%in%ignore)])
    col<-which(names(def)%in%keep[1])
    colr<-which(names(def)%in%keep[2])
    ldefs<-unlist(lapply(c(1:ncol(def)),function(x) length(unique(as.vector(unlist(def[,x]))))))
    #print(def)
    ldefs<-which(ldefs>1)
    
    def<-def[def[,col]!="",]
    #print(def)
    defr<-unique(unlist(as.vector((def[,colr]))))
    #print(paste(col,colr,ldefs,col%in%ldefs))
    if(col%in%ldefs){
        defv<-unlist(as.vector((def[,col])))
    }else{
        defv<-unlist(as.vector(unique(def[,col])))
    }
    defo<-defv
    defv<-gsub("%","",defv)
    if(sum(gsub("+/-",",",defv)!=defv)>0) {
        defv<-unlist(lapply(c(1:length(defv)),function(x) 
            c(as.numeric(strsplit(defv[x],"\\+/-")[[1]][1])-as.numeric(strsplit(defv[x],"\\+/-")[[1]][2]),
              as.numeric(strsplit(defv[x],"\\+/-")[[1]][1])+as.numeric(strsplit(defv[x],"\\+/-")[[1]][2]))
        ))
    }
    if(sum(gsub("-",",",defv)!=defv)>0) {
        defv<-unlist(lapply(c(1:length(defv)),function(x) as.numeric(unlist(strsplit(defv[x],"-")))))
    }
    if(col%in%ldefs){
        defd<-subset(def,select=names(def[ldefs[ldefs!=col]]))
        defd<-as.data.frame(lapply(defd, as.character), stringsAsFactors=FALSE)
        #print(defv)
        defmin<-min(as.numeric(defv))
        defmax<-max(as.numeric(defv))
        def<-(unlist(lapply(c(1:nrow(def[,ldefs])),function(x)
            paste0(defo[x]," (",paste(as.character(defd[x,]),collapse="-"),")")
        )))
    }else{
        #print(length(defv))
        if(length(defv)>1){
            def<-defo
            defmin<-min(defv)
            defmax<-max(defv)
        }else{
            #print(length(defv)==0)
            if(length(defv)==0){
                def<-""
                defmin<-def
                defmax<-def
            }else{
                def<-defv
                defmin<-defv
                defmax<-defv
            }
        }
    }
    #print(paste(def,defmin,defmax,sep=","))
    #print(def)
    def<-paste(c(def,defr),collapse=", ")
    return(list(defmin,defmax,def))
}

adddefaults<-function(line,x,D){
    v2006<-c("IPCC2006")
    v1997<-c("IPCC1997")
    r2006<-c("Reference.2006")
    r1997<-c("Reference.1997")
    i2006<-c(v2006,r2006)
    i1997<-c(v1997,r1997)
    
    deffields<-c(v2006,v1997)
    
    selection<-D$meastype==line$meastype & D$sector_number==line$sector_number&
        D$category==line$category #& as.character(D$gas)==as.character(line$gas)
    #print(D$gas[selection])
    #print(line$gas)
    #print(as.character(D$gas[selection])==as.character(line$gas[selection]))
    #print(D[selection,])
    selection<-selection & D[1,v2006]!="" & D[1,v1997]!=""
    
    vdefaults<-unique(D[selection,])
    ndefaults<-nrow(vdefaults)
    if(ndefaults==0){
        ipcc<-rep(c("","","no defaults"),2)
    }else{
        ipcc2006<-unlist((multidefaults(vdefaults,i2006,i1997)))
        #ipcc2006<-""
        ipcc1997<-unlist(multidefaults(vdefaults,i1997,i2006))
        ipcc<-c(ipcc2006,ipcc1997)
    }
    return(ipcc)
}



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

plottime<-function(pr=NULL,sc=NULL,mt=NULL,ct=NULL,source=NULL,DF=allagri){
    
    #if(is.null(pr)){return("no country indicated")}
    if(is.null(mt)){return("no meastype")}
    
    sel<-rep(1,nrow(DF))
    if(!is.null(pr))sel<-sel & DF$party==pr
    if(!is.null(sc))sel<-sel & grepl(sc,DF$sector_number)
    if(!is.null(source))sel<-sel & grepl(source,DF$source)
    if(!is.null(mt))sel<-sel & DF$meastype==mt
    if(!is.null(ct))sel<-sel & DF$category==ct
    time_series<-DF[sel,]
    timelabel<-paste0(time_series[1,"measure"]," - ",time_series[1,"sector_number"]," ",
                      time_series[1,"category"]," [",time_series[1,"unit"],"]")
    timemin<-1*min(time_series[,years],na.rm=TRUE)
    timemax<-max(time_series[,years],na.rm=TRUE)
    timemax<-timemax+0.22*(timemax-timemin)
    timelegend<-subset(time_series,select=c("party","sector_number","category"))
    timelegend$leg<-""
    if(length(unique(timelegend$party))>1)timelegend$leg<-paste0(timelegend$party)
    if(length(unique(timelegend$category))>1){
        timelegend$leg<-paste0(timelegend$leg,timelegend$category)
    }
    View(time_series)
    if(nrow(time_series)>1){
        plot(years,time_series[1,years],ylim=c(timemin,timemax),ylab=timelabel)
        for(i in c(2:nrow(time_series))){
            #print(paste(i,time_series[i,"party"]))
            points(years,as.data.frame(time_series)[i,years],pch=i%%26)
        }
    }else{
        plot(years,as.data.frame(time_series)[1,years],ylim=c(timemin,timemax),ylab=timelabel)
        #plot(years,as.data.frame(time_series)[1,years],ylim=c(min(time_series,na.rm=TRUE),max(time_series,na.rm=TRUE)))
    }
    legend(x="topleft",ncol=5,legend=timelegend$leg,pch=seq(1,nrow(time_series))%%26)
}

mtexttit<-function(runsect,runmeta,runmeas){
    runsect[1]<-gsub(paste0(" ",unlist(runmeta[2])),"",unlist(runsect[1]))
    runsector<-as.vector(unlist(runsect[1]))
    runmeastype<-as.vector(unlist(runmeas[1]))
    #print(runmeastype)
    #rununit<-as.vector(unlist(runmeas[3]))
    
    runcateg<-paste(lapply(runsect,as.character),collapse=" ")
    runcateg<-gsub("/","-",runcateg)
    if(grepl("^4",runsect[1])) runcateg<-runsect[1]
    
    runsource<-as.vector(unlist(runmeta[1]))
    runsource<-as.vector(unlist(runmeta[2]))
    runsource<-as.vector(unlist(runmeta[3]))
    runcategory<-as.vector(unlist(runsect[2]))
    runcategory<-gsub("Farming","",runcategory)
    runtarget<-as.vector(unlist(runmeta[4]))
    runtype<-as.vector(unlist(runmeta[5]))
    rungas<-as.vector(unlist(runmeas[2]))
    selfield<-runmeta[metafields]!=""
    runmethod<-paste(as.vector(unlist(runmeta[metafields[selfield]])),collapse=". ")
    if(grepl("^4",runsect[1])) {
        if(tolower(runsource)==tolower(runtarget)) {
            runmethod<-paste0(firstup(runsource)," remaining ",firstup(runtarget))
        }else{
            runmethod<-paste0(firstup(runsource)," converted to ",firstup(runtarget))
        }
        if(runmeastype=="EM") {
            runmethod<-paste0(runcategory," - ",runmethod,": ",rungas)
        }else{
            runmethod<-paste0(runmethod,". ",runtype)
        }
        mtexttitle0<-paste0(runsector," ",runmethod)
    }else{
        #   ---> If the category name includes the "source" (eg animal type) - remove
        # First word which is beyond the max number of characters to be displayed
        mtexttitle0<-paste0(gsub(runmethod,"",runcateg)," - ",runmethod)
        if(rungas!="no gas" & runmeastype=="EM"){mtexttitle0<-paste0(mtexttitle0,": ",gsub(" ","",rungas))}
        mtexttitle0<-gsub("_","-",mtexttitle0)    
    }
    return(mtexttitle0)
}

source("curunit.r")
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

emissionshareplot<-function(sec,DF=agrimix){
    pwidth=16
    pheight=pwidth/1.833
    plotresolution<-plotresolution
    
    dfm<-DF[DF$measure=="Emissions"&grepl(sec,DF$sector_number),]
    if(grepl("A|B",sec)){
        dfm<-agridet[agridet$measure=="Emissions"&grepl(sec,agridet$sector_number)&!grepl(paste0(sec,".4"),agridet$sector_number),]
        dfm2<-agrimix[agrimix$measure=="Emissions"&grepl(sec,agrimix$sector_number)&!agrimix$sector_number%in%c(paste0(sec,".1")),]
        dfm<-rbind(dfm,dfm2)
        if(sec=="3.B.2.5"){
            dfm<-allagri[allagri$meastype=="NEXC"&grepl("3.B.2.5",allagri$sector_number)&allagri$source!="",]
        }
    }
    dfp<-countries2[countries2%in%unique(dfm$party)]
    dfc<-sapply(dfp,function(x) countriesl[which(countries2==x)])
    if(sec=="3.B.2.5"){
        dfm<-dcast(dfm,source ~ party,value.var = "2014")
        dfm[is.na(dfm)]<-0
        dfl<-dfm$source
        dfo<-order(sapply(dfl,function(x) which(x==manureSystems)))
        print(dfl)
        print(dfo)
        dfl<-dfl[dfo]
        print(dfl)
        dfm<-dfm[dfo,]
    }else if(grepl("A|B",sec)){
        dfm<-dcast(dfm,category ~ party,value.var = "2014")
        dfm[is.na(dfm)]<-0
        dfl<-dfm$category
        dfo<-order(sapply(dfl,function(x) which(x==c(livestock,otherlivestock,"Farming"))))
        dfl<-dfl[dfo]
        dfl<-gsub("Farming","Indirect emissions",dfl)
        dfm<-dfm[dfo,]
    }else{
        dfm<-dcast(dfm,sector_number ~ party,value.var = "2014")
        dfm[is.na(dfm)]<-0
        dfl<-dfm$sector_number
    }

    dmt<-as.vector(apply(dfm[,2:ncol(dfm)],2,sum))
    dfms<-rbind(sapply(1:nrow(dfm),function(x) dfm[x,2:ncol(dfm)]/dmt))
    row.names(dfms)<-dfc
    #View(dfms)
    
    figname<-paste0(plotsdir,"/",cursubm,"emissionshare_",sec,".",plotformat,collapse=NULL)
    
    if(plotformat=="pdf") pdf(file=figname,width=pwidth,height=pheight)
    if(plotformat=="png") png(file=gsub("pdf","png",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
    if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)

    par(mar=c(7,4,1,7), xpd=TRUE)
    par(cex=0.7)
    curcols<-grey.colors(length(dfl))
    curcols1<-curcols[as.logical(c(1:length(dfl))%%2)]
    curcols2<-curcols[!as.logical(c(1:length(dfl))%%2)]
    if(length(dfl)>8|sec=="3.B.2.5")curcols<-c(curcols1,curcols2)
    barplot(t(dfms),horiz = FALSE,las=2,cex.axis = 0.8,offset = 0,col = curcols)
    legend("topright",inset=c(-0.19,0), legend=dfl,fill=curcols,cex=0.7)
    graphics.off()
}

makegrowthplot<-function(pars,secs,cats="",meastype){
    figdir<-gsub("checks","plots",issuedir)
    
    #All parties for sectors and categories
    #Select sector_number via grepl
    #Predefine vector of categories
    if(paste(cats,collapse="")=="all" | paste(cats,collapse="")==""){
        cats<-unique(unlist(growthcheck$category[grepl(secs,growthcheck$sector_number)&growthcheck$meastype==meastype]))
    }
    
    
    t1<-growthcheck[grepl(secs,growthcheck$sector_number)&growthcheck$category%in%cats&growthcheck$meastype==meastype,]
    #cat(secs,cats,meastype)
    nparties<-nrow(t1)
    print(nparties)
    plotformat<-"pdf"
    pieunit<-5
    omititle<-1
    npiecols<-3
    piewidth<-npiecols*pieunit
    piefont<-pieunit/2
    maxperpage<-4
    pieheight=pieunit*0.5*min(nparties,maxperpage)+omititle
    page=1
    figname<-paste0(figdir,"/",cursubm,"growth",page,"_",gsub("*","",secs),"-",meastype,".",plotformat,collapse=NULL)
    pieresolution=300
    if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight,onefile=TRUE)
    if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
    if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
    print(paste0("nparties",nparties))
    par(mfrow = c(min(nparties,maxperpage),npiecols))
    par(omi=c(0,0.2,omititle,0.2))
    par(cex.main=1.7,cex.axis=1.5,cex.lab=1.5)
    #par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(lwd=0.5)
    
    mcommand<-paste0("makegrowthplot(pars,sec=,\"",secs,"\",cats=c(",paste(cats,collapse=","))
    mcommand<-paste0(mcommand,"),",meastype,")")
    print(mcommand)
    curplot<-0
    pars<-unique(as.vector(unlist(t1$party)))
    for(par in pars){
        t2<-t1[t1$party==par&grepl(secs,t1$sector_number)&t1$category%in%cats&t1$meastype==meastype,]
        secsl<-unique(as.vector(unlist(t2$sector_number)))
        secsl<-secsl[!secsl=="3.B.2.5 N2O Emissions per MMS"]
        #print(secsl)
        for(sec in secsl){
            t2<-t1[t1$party==par&t1$sector_number==sec&t1$category%in%cats&t1$meastype==meastype,]
            catsl<-unique(as.vector(unlist(t2$category)))
            for(cat in catsl){
                t2<-t1[t1$party==par&t1$sector_number==sec&t1$category==cat&t1$meastype==meastype,]
                #Needed for Rice
                clal<-unique(as.vector(unlist(t2$classification)))
                for(cla in clal){
                    #print(paste(par,sec,cat,sep="-"))
                    t2<-t1[t1$party==par&t1$sector_number==sec&t1$category==cat&t1$meastype==meastype&t1$classification==cla,]
                    curplot<-curplot+1
                    if(curplot>maxperpage & plotformat!="pdf"){
                        graphics.off()
                        page<-page+1
                        curplot<-1
                        nparties<-nparties-maxperpage
                        pieheight=piewidth*0.25*min(nparties,maxperpage)
                        figname<-paste0(figdir,"/",cursubm,"test",page,".",plotformat,collapse=NULL)
                        if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight)
                        if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
                        if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
                        par(mfrow = c(min(nparties,maxperpage),npiecols))
                    }
                    t2x<-unique(t2[,paste0(years,".x")])
                    t2y<-unique(t2[,paste0(years,".y")])
                    values<-as.vector((unlist(t2x)))
                    values<-as.vector((unlist(t2y)))
                    #valuid<-as.vector(unlist(t2$variableUID))
                    #print(values)
                    #curves<-as.vector(unlist(allgcurve[allgcurve$party==par&allgcurve$variableUID==valuid,years]))
                    t3<-t1$years[t1$party==par&t1$sector_number==sec&t1$category==cat&t1$meastype==meastype]
                    t3<-unlist(t3)
                    t3<-unlist(strsplit(t3," "))
                    t4<-match(years,as.numeric(t3))
                    t4<-which(!is.na(t4))
                    
                    mtext<-paste(par,sec,cat,meastype,sep="-")
                    #cat("\n",par,"sec=",sec,"cat=",cat,"cla=",cla,meastype,length(years),length(values))
                    plot(years,values,type="p",main=paste(mtext,": values"),pch=21,col="black",bg="black",cex=2)
                    points(years[t4],values[t4],type="p",pch=21,col="red",bg="red",cex=2.5,new=FALSE)
                    
                    if(curplot %% maxperpage == 1) mtext(mcommand, outer = TRUE, cex = 1.5)
                    
                    #plot(years,growths-1,type="p",,main=paste(mtext,": 1st growth"),pch=21,col="grey20",bg="grey20",cex=2)
                    #lines(years,rep(0,length(years)))
                    #points(years[t4],growths[t4]-1,type="p",pch=21,col="red",bg="red",cex=2.5,new=FALSE)
                    
                    #plot(years,curves-1,type="p",,main=paste(mtext,": 2nd growth"),pch=21,col="grey20",bg="grey20",cex=2)
                    #lines(years,rep(0,length(years)))
                    #points(years[t4],curves[t4]-1,type="p",pch=21,col="red",bg="red",cex=2.5,new=FALSE)
                }
            }
        }
    }
    graphics.off()

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