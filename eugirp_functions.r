# see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

viewlast<-function(n){View(cat3all[(nrow(cat3all)-n):nrow(cat3all),])}
newuid<-function(){paste("EUGIRP",gsub("2015","15",cursubm),"-",format(Sys.time(),"%y%m%d%H%M%S"),"-",MHmakeRandomString(1,4),sep="")}

getuid<-function(mode=1,ok=1,x=1,sec="*",cat="*",met="*",cla="*",sou="*",tar="*",opt="*",msr="*",mea="*",gas="*"){
    
    # mode=1: returns number of UIDs available
    # mode=2: returns UIDs if #ist 1
    
    # Note all info a re set to default "*" which means that all are selected
    #      only those which are of interest need to be given and only if different
    selc<-c("sec","cat","met","cla","sou","tar","opt","msr","mea","gas")
    selv<-c(sec,cat,met,cla,sou,tar,opt,msr,mea,gas)
    sel<-"";for(i in c(1:length(selv))) {g<-paste0(selc[i],"<-'",selv[i],"'");sel<-paste(sel,g,sep=";")}
    
    #sel<-paste(mode=1,myobject=NULL,ok=1,sec="*",cat="*",met="*",cla="*",sou="*",tar="*",opt="*",msr="*",mea="*",gas="*",x=1,sep=";")
    #print(sel)
    #         getuidr<-nrow(cat3alltab[cat3alltab$sector_number==paste0(sec,myobject$sector_number[x]) &
    #                             cat3alltab$allmethods==met &
    #                             cat3alltab$clim==cli &
    #                             cat3alltab$meastype==mymeastype &
    #                             cat3alltab$gas==gas,])
    getuidr<-cat3alltab[grepl(sec,cat3alltab$sector_number) &
                            grepl(cat,cat3alltab$category) &
                            grepl(met,cat3alltab$method) &
                            grepl(cla,cat3alltab$classification) &
                            grepl(sou,cat3alltab$source) &
                            grepl(tar,cat3alltab$target) &
                            grepl(opt,cat3alltab$option) &
                            grepl(msr,cat3alltab$measure) &
                            grepl(mea,cat3alltab$meastype) &
                            grepl(gas,cat3alltab$gas),]
    ngetuidr<-nrow(getuidr)
    curuids<-getuidr$variableUID
    
    if(ngetuidr>1){View(cat3all[cat3all$variableUID%in%curuids,]);stop()}
    if(ngetuidr==1){ngetuidr<-unlist(as.vector(curuids))}
    return(ngetuidr)
}

checkuid<-function(myobject=NULL,sec=NULL,met="",sou="",cat="",mea=NULL,gas="no gas"){
    
    pargas<-paste0(mea,gas)
    
    myobject[,pargas]<-unlist(lapply(c(1:nrow(myobject)), function(x)
        getuid(1,ok=1,mea=mea,gas=gas,x=x,cat=cat,
               sec=paste0("^",sec,checkuids$sector_number[x],"$"))))
#         getuid(1,myobject,ok=1,sec,met,cli,mea,gas,x)))
#     myobject[,pargas]<-unlist(lapply(c(1:nrow(myobject)), function(x) 
#         getuid(2,myobject,ok=myobject[x,pargas],sec,met,cli,mea,gas,x)))
    
    return(myobject)
}

extractuiddata<-function(DF=NULL,uid=NULL,c){
    c<-as.data.frame(c)
    names(c)<-"party"
    tmp1<-unique(DF[DF[,"variableUID"]==uid,c("party",years)])
    
    ntmp<-nrow(tmp1)
    tmp1<-merge(c,tmp1,by="party",all=TRUE)
    tmp1<-tmp1[,years]
    #tmp1[is.na(tmp1)]<-0
    tmp1<-as.matrix(tmp1)
    return(tmp1)
}

nodiff<-function(checks=NULL,ncheck=1,test=NULL,val1=NULL,val2=NULL,sec=NULL,reas=""){
    checks[ncheck,"test"]<-test
    checks[ncheck,"val1"]<-val1
    checks[ncheck,"val2"]<-val2
    checks[ncheck,"ms"]<-"all"
    checks[ncheck,"yr"]<-"all"
    checks[ncheck,"sec"]<-sec
    checks[ncheck,"val"]<-""
    checks[ncheck,"obs"]<-reas
    ncheck<-ncheck+1
    return(list(ncheck,checks))
}

diffmatrix<-function(checks=NULL,ncheck=1,A=NULL,B=NULL,test=NULL,val1=NULL,val2=NULL,sec=NULL,roundn=3){
    diff=NULL
    ol<-ls()
    save(ol,file="test.RData")
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
            
            # Check if the values differ by orders of magnitude
            quotient<-round(log10(round(A[rn,cn],roundn)/round(B[rn,cn],roundn)),0)
            obs<-""
            val<-paste0(round(A[rn,cn],roundn)," / ",round(B[rn,cn],roundn))
            if(is.infinite(quotient)){
                obs<-paste0(val2," is not reported")
                val<-""
            }else 
                if(round(round(A[rn,cn],roundn)/10**quotient,roundn)==round(B[rn,cn],roundn)){
                obs<-paste0(val1," is 10",quotient," x ",val2)
                val<-""
            }else{
                quotient<-round(round(A[rn,cn],roundn)/round(B[rn,cn],roundn),3)
                if(quotient==0) quotient<-round(round(A[rn,cn],roundn)/round(B[rn,cn],roundn),3+3)
                obs<-paste0("val1 is ",quotient," x val2")
            }
            checks[ncheck,"val"]<-val
            checks[ncheck,"obs"]<-obs
            ncheck<-ncheck+1
        }
    }
    return(list(ncheck,checks))
}

# Function group the years into a string
#  - Returns "all" if all years in "years"
#  - Returns the missing years if less than half are missin
#  - Returns the selected years if less then half are selectd
reportyears<-function(checky,compare){
    maxn<-length(compare)
    if(is.null(compare)) maxn<-NULL
    curn<-length(unlist(checky))
    #print(paste0("checky=",checky))
    #print(paste0("years",maxn,"-",compare))
    if(is.null(maxn)){
        ret<-paste0(unlist(checky),collapse=" ")
    }else if(maxn==curn){
        ret<-"all"
    }else if(maxn==0){
        ret<-""
    }else if(curn<maxn/2){
        ret<-paste0(unlist(checky),collapse=" - ")
    }else{
        misy<-paste0(compare[! compare %in% unlist(checky)],collapse=" ")
        ret<-paste0("all except: ",misy,collapse=" ")
    }
    #print(paste("return=",ret))
    return(ret)
}

simplifytestmatrix<-function(check,group,compare){
    # group: column which will be grouped
    # sorting: order of columns in return 
    
    testheaders<-names(check)
    compare<-compare
    check3<-subset(check,select=names(check)[! names(check) %in% group])
    #checky<-check[,group]
    checky<-as.data.frame(check[,group])
    checky<-aggregate(checky, by = as.list(check3), function(x) paste0(x,collapse=NULL))
    checkn<-ncol(checky)
    #checky[,group]<-unlist(lapply(c(1:nrow(checky)),function(x) reportyears(checky$x[x],compare)))
    checky[,group]<-unlist(lapply(c(1:nrow(checky)),function(x) reportyears(checky[x,checkn],compare)))
    check<-checky[,names(checky)%in%testheaders]
    
    return(check)
    
}

add2cat3all<-function(matrix,sec="",gas="",unit="",met="",cli="",mea="",uid=""){
    # Note this adds a new row only if it does not exist 
    # Existing rows will not be overwritten.
    
    exists<-nrow(cat3all[cat3all$sector_number==sec & cat3all$gas==gas & cat3all$unit==unit &
                             cat3all$allmethods==met & cat3all$meastype==mea,])
    addr<-1
    if(exists==0){
        addrow<-cat3emp
        addrow$sector_number[1]<-sec
        addrow$gas[1]<-gas
        addrow$unit[1]<-unit
        addrow$allmethods[1]<-met
        addrow$meastype[1]<-mea
        addrow$variableUID[1]<-uid
        
        addnewrow<-addrow
        
        for(i in c(1:nrow(matrix))){
            if(sum(matrix[i,],na.rm=TRUE)!=0){
                addnewrow[addr,]<-addrow
                addnewrow$party[addr]<-allcountries$party[i]
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
        if(addr>0) levels(cat3all$unit)<-c(levels(cat3all$unit),unit)
        if(addr>0) cat3all<-rbind(cat3all,addnewrow)
    }
    return(list(cat3all,addr))
}


###############################################################
#
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

###############################################################



###############################################################
# FUNCTIONS REQUIRED FOR PLOTTING
###############################################################

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
