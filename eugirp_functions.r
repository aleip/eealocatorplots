# see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

viewlast<-function(n){View(allagri[(nrow(allagri)-n):nrow(allagri),])}
newuid<-function(){paste("EUGIRP",gsub("2015","15",cursubm),"-",format(Sys.time(),"%Y%m%d-%H%M.%S"),"-",MHmakeRandomString(1,6),sep="")}
firstup<-function(string){
    rstring<-tolower(string)
    rstring<-paste0(toupper(substr(rstring,0,1)),substr(rstring,2,nchar(rstring)))
}
uidall<-function(D,uid){D[D$variableUID%in%uid,]}
viewuid<-function(D,uid){
    View(uidall(D,uid),
         unique(paste0(D$sector_number[D$variableUID==uid],D$category[D$variableUID==uid])))}
matbyind<-function(D,v){
    if(nrow(v)>0){M<-unlist(lapply(c(1:nrow(v)),function(y) D[v[y,1],v[y,2]]))}else{M<-0}
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

sumovercountries<-function(D,uid,y,c){
    y<-as.character(y)
    s<-matrix(0,ncol=length(y))
    m<-matrix(0,ncol=length(y),nrow=length(c))
    m<-D[D$variableUID==uid,y]
    s<-apply(m,2,sum)
    return(s)
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
            quotient<-round(log10(round(A[rn,cn],roundn)/round(B[rn,cn],roundn)),0)
            obs<-""
            val<-paste0(round(A[rn,cn],roundn),"/",round(B[rn,cn],roundn))
            if(is.infinite(quotient)){
                obs<-paste0(val2," is not reported")
                val<-""
                fac<-0
            }else 
                if(round(round(A[rn,cn],roundn)/10**quotient,roundn)==round(B[rn,cn],roundn)){
                obs<-paste0(val1," is 10^fac x ",val2)
                val<-""
                fac<-quotient
            }else{
                quotient<-round(round(A[rn,cn],roundn)/round(B[rn,cn],roundn),3)
                if(quotient==0) quotient<-round(round(A[rn,cn],roundn)/round(B[rn,cn],roundn),3+3)
                obs<-paste0("val1 is fac x val2")
                fac<-quotient
            }
            checks[ncheck,"val"]<-val
            checks[ncheck,"obs"]<-obs
            checks[ncheck,"fac"]<-fac
            ncheck<-ncheck+1
        }
    }
    return(list(ncheck,checks))
}

# Function group the years into a string
#  - Returns "all" if all years in "years"
#  - Returns the missing years if less than half are missin
#  - Returns the selected years if less then half are selectd
reportyears<-function(checkyx,compare){
    
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
                ret<-paste0(curcheckyx,collapse=" ")
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
        check$val<-paste0("all countries and years")
    }else{
        #if 'all' or 'all except' report all 
        if(grepl("all",c)){c<-allcountries}else{c<-unlist(strsplit(c," "))}
        
        selection<-data$sector_number==s & data$category==cat
        selp<-as.vector(unique(unlist(data$party[selection])))
        selp<-selp[!selp%in%c]
        
        if(check$test=="NexTOT"){
            selection<-selection & data$gas=="no gas" & (data$meastype=="EM" | data$meastype=="NEXC") 
            extraline<-"# Note: countries which do not report both 'Nitrogen excretion per MMS' and 'Total N excreted' are not identified"
        }
        if(check$test=="NexRATE"){
            selection<-(selection | (data$sector_number==gsub("3.B.2","3.A",s) & data$category==cat)) &
                       (data$meastype%in%c("POP","NRATE","TNEXC2","EM") & (data$gas%in%c("no gas","")))
            
            extraline<-"# Note: countries which do not report both 'Nitrogen excretion rate' and 'Total N excreted' are not identified"
        }
        
        if(grepl("N in ",check$test)){
            selection<-selection
        }
        if(grepl("N2O-IEF in ",check$test)){
            selection<-(selection | (data$sector_number=="3.B.2.5 N2O Emissions per MMS" & data$meastype=="IEF"))
        }
        
        if(grepl("N2O-NIEF",check$test)){
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
                              "#     - The use of IEF_MMS that are different for different animal types. In this case please provide an explanation!!\n",
                              "#     - An error in the calculation. In this case please provide a correction.\n",
                              "#")
        }
        
        selw<-selection & (data$party %in% c)
        selc<-selection & (data$party %in% selp)
        checkw<-data[selw,reportfields]
        checkc<-data[selc,reportfields]
        checkw<-checkw[order(checkw$party,checkw$notation,checkw$category,checkw$measure),]
        checkc<-checkc[order(checkc$party,checkc$notation,checkc$category,checkc$measure),]
        
        if(exists("checkw")>0){
            checkfile<-gsub(" ","",paste0(check$test,"_",check$val1,"_vs_",check$val2,"_",check$obs,s,cat,"_",x,".csv"))
            checkfile<-gsub("/","",checkfile)
            line<-paste(check[,checkfields],collapse="-")
            con <- file(paste0(issuedir,checkfile), open="wt")
            writeLines(paste0("# ",line,"\n#"), con)
            if(exists("extraline")) writeLines(extraline, con)
            writeLines(paste0("#\n#"), con)
            for(cc in c) {write.csv(checkw[checkw$party==cc,],con);writeLines("#",con)}
            writeLines("#\n# Comparison: other countries",con)
            write.csv(checkc,con)
            close(con)
            check$val<-paste0(figdate,"/",checkfile)
        }
    }
    return(check)
}

add2allagri<-function(matrix,sec="",cat="",gas="",unit="",sou="",tar="",mea="",uid="",note="",force=0){
    # Note this adds a new row only if it does not exist 
    # Existing rows will not be overwritten.
    
    agruemp<-as.data.frame(matrix(rep("",ncol(allagri)),nrow=1,ncol=ncol(allagri)),stringsAsFactors = FALSE)
    names(agruemp)<-names(allagri)
    agruemp[,years]<-rep(0,length(years))
    
    exists<-nrow(allagri[allagri$sector_number==sec & allagri$category==cat & allagri$gas==gas & allagri$unit==unit &
                             allagri$source==sou & allagri$meastype==mea,])
    
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
            levels(allagri$unit)<-c(levels(allagri$unit),unit)
            #levels(allagri$variableUID)<-c(levels(allagri$variableUID),uid)
            #levels(allagri$source)<-c(levels(allagri$source),sou)
        }
        if(addr>0) allagri<-rbind(allagri,addnewrow)
    }else{
        print("exists")
    }
    return(list(allagri,addr))
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