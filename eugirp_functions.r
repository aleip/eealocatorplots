# see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

view<-function(D){View(D)}
viewlast<-function(n,allagri=allagri){View(allagri[(nrow(allagri)-n):nrow(allagri),])}
newuid<-function(sector,categ,meast,units,metho,sourc,targe,optio,gasun){
    #paste("EUGIRP",substr(cursubm,3,nchar(cursubm)),"-",format(Sys.time(),"%Y%m%d-%H%M.%S"),"-",MHmakeRandomString(1,6),sep="")
    sector<-substring(paste0(gsub(" ","",gsub("\\.","",gsub("\\^","",sector))),"0000"),1,5) #5
    categ1<-gsub(" ","",gsub("-","",categ))
    categ<-substring(paste0(substring(categ1,1,4),substring(categ1,nchar(categ1)-3,nchar(categ1)),"00000"),1,8) #8
    meast<-substring(paste0(meast,"00000"),1,3)                           #3
    units<-substring(paste0((gsub(" ","",gsub("\\/","",gsub("\\^","",units)))),"00000"),1,6) #6
    sourc<-substring(paste0(gsub(" ","",gsub("-","",sourc)),"00000"),1,2) #2
    targe<-substring(paste0(gsub(" ","",gsub("-","",targe)),"00000"),1,3) #1
    gasun<-substring(paste0(gasun,"0"),1,2)                               #2
    optio <- as.character(optio)
    #save(sector,categ,meast,units,metho,sourc,targe,optio,gasun, file="a.rdata")
    optio<-if(optio!=""){substring(optio,nchar(optio),nchar(optio))}else{"0"}   #1
    metho<-substring(paste0(gsub(" ","",gsub("-","",metho)),"00000"),1,3) #3
    newid<-paste0("eugirp",sector,categ,meast,units,sourc,targe,gasun,optio,metho)
    #cat("\n",nchar(newid),"-",newid)
    return(newid)
    
}
firstup<-function(string){
    rstring<-tolower(string)
    rstring<-gsub(" n "," N ",rstring)
    rstring<-paste0(toupper(substr(rstring,0,1)),substr(rstring,2,nchar(rstring)))
}

savestep <- function(stepsdone, savelist){
  
  savefile <- gsub("_s[0-9]", "", rdatallem)
  save(list=savelist,file=savefile)
  save(list=savelist,file=gsub(".RData",paste0("_s", stepsdone,".RData"),savefile))
  save(list=savelist,file=gsub(".RData",paste0("_s", stepsdone, "~",figdate,".RData"),savefile))
  
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
filldf<-function(DF,cols=allcheckfields,fillwith=0){
    
    missing<-cols[!cols%in%names(DF)]
    nmissing<-length(missing)
    
    test0<-matrix(rep(fillwith,nrow(DF)*nmissing),ncol=nmissing,nrow=nrow(DF))
    test0<-as.data.frame(test0)
    names(test0)<-missing
    DF<-cbind(DF,test0)
    
    return(DF)
    
}
last<-function(years){last<-years[length(years)]}
curdate<-function(){format(Sys.time(), "%Y%m%d")}
curtime<-function(secs=FALSE){
    if(secs){format(Sys.time(), "%Y%m%d-%H%M%S")
    }else{format(Sys.time(), "%Y%m%d-%H%M")}}

rounddigit<-function(val,showdig=2){
    sig<-val/abs(val)
    n<-log10(abs(val))
    
    digs<-showdig-min(showdig,floor(n))
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
        ngetuidr<-paste0(ngetuidr,": ",paste(unlist(as.vector(paste0("xx",curuids,"yy"))),collapse=","))
        stop(ngetuidr)}
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
    m<-D[D$variableUID==uid&D$party%in%c,y]
    #View(m,"n")
    #stop()
    s<-apply(m,2,sum,na.rm=T)
    return(s)
}

eu28sums <- function(A, aeu=eu, years){
  
  A <- A[! party %in% aeu]
  
  # Aggregate only the 'summable' meastypes
  AB <- A[! meastype %in% meas2sum]
  AC <- A[  meastype %in% meas2sum]
  
  # Remove fields such as notes etc.
  AC <- AC[, -"notation", with=FALSE]
  
  for(i in aeu){
    # Aggregate over countries
    ac <- curcountries[variable == i & value == 1]$code3
    ac <- ac[! grepl("^EU", ac)]
    AD <- AC[party %in% ac]
    AD <- AD[, lapply(.SD, sum, na.rm=TRUE), by = setdiff(names(AD), c(years, "party")), .SDcols = years]
    AD$party <- i
    AD$notation <- "eugirp"
    
    A <- rbind(A, AD, fill = TRUE)
    # Add 
  }
  
  return(A)
  
}
eu28sums_old<-function(A,aeu=eu,years=years){
    A[,years]<-apply(A[,years],2,function(x) as.numeric(x))
    afields<-names(A)
    #cat(years,allfields)
    notselect<-allfields[!allfields %in% c("notation","party",years,"option")]
    notselect<-notselect[!grepl("^[12]",notselect)]
    agrimeas<-unique(subset(A,select=notselect))
    agri2sum<-agrimeas[agrimeas$meastype %in% meas2sum,]
    removeeu28<-A$meastype %in% meas2sum & A$party%in%c("EU28",aeu,excludeparty)
    A<-A[!removeeu28,]
    #A<-A[,allfields]
    if("datasource"%in%afields){multisource<-unique(A$datasource)}else{multisource<-"nir"}
    for(loopsource in multisource){
        for(i in 1:length(aeu)){
            cat("\nCalculate sum for ",aeu[i]," ")
            eu28sum<-as.data.frame(matrix(rep(0,ncol(A)*nrow(agri2sum)),
                                          ncol=ncol(A),nrow=nrow(agri2sum)))
            names(eu28sum)<-names(A)
            eu28sum[,names(agri2sum)]<-agri2sum[,names(agri2sum)]
            acountry<-as.character(country4sub[country4sub[,aeu[i]]==1,"code3"])
            acountry<-acountry[!acountry%in%eu]
            # remove unwanted countries
            B<-A[A$party%in%acountry,]
            if("datasource"%in%afields){B<-B[B$datasource==loopsource,]
                                        cat(loopsource," ... ")}
            # calculate the sum over remaining countries
            C<-B[,c(years,"variableUID")]
            D<-aggregate(C[,years],by=list(C$variableUID),sum,na.rm=TRUE)
            # get other columns back
            E<-unique(B[,names(B[!names(B)%in%c(years,"party","notation")])])
            eu28sum<-merge(E,D,by.x="variableUID",by.y="Group.1")
            eu28sum$notation<-"eugirp"
            eu28sum$party<-aeu[i]
            
            # sort to standard
            #eu28sum<-eu28sum[,allfields]
            eu28sum<-filldf(DF = eu28sum,afields)
            #print(names(A))
            #print(names(eu28sum))
            eu28sum<-eu28sum[,names(A)]
            A<-rbind(A,eu28sum)
            
        }
    }
    return(A)
}


weightovercountries<-function(D,Auid,Puid,ok,y,c, x){
    
    # Returns the weighted average over all countries
    # for which both AD and a Value for the variable exist.
    # In case a country as a value (e.g. IEF) but does not 
    # report Ad, then this is excluded from the EU weighted average!
   #AL20200130 for debugging purposes and to write to a file, which can be analyzed separately: 
   #debug <- TRUE
  #  if(debug){
  #    print(paste0("x<-",x))
  #    print(paste0("Auid<-",Auid))
  #    print(paste0("Puid<-",Puid))
  #    print(paste0("ok<-",ok))
  #   save(D,Auid,Puid,ok,y,c, x, file="debug20200131_alex.rdata")
  #     }
    Auid<-as.vector(unlist(Auid))
    Puid<-as.vector(unlist(Puid))
    if(ok=="-" | ok=="" | grepl("^[1-9]",ok)){
        ss<-rep(NA,length(y))
    }else{
        y<-as.character(y)
        #s<-matrix(0,ncol=length(y))
        ss<-matrix(0,ncol=length(y)) # alex 20200128 
        ad<-matrix(0,ncol=length(y),nrow=length(c))
        pa<-ad
        ad<-extractuiddata(D,Auid,c,narm = FALSE, cursubm = cursubm)
        pa<-extractuiddata(D,Puid,c,narm = FALSE, cursubm = cursubm)
        
        # alex20200130; changed rm.na to na.rm (rm.na is not a function)
        ad<-ad[!is.na(apply(pa,1,sum,na.rm=TRUE)),]
        pa<-pa[!is.na(apply(pa,1,sum,na.rm=TRUE)),]
        if(length(pa)<length(c)){
            ad<-t(ad)
            pa<-t(pa)
        }
        
      
        if(is.matrix(ad)){
          pa<-pa[!is.na(apply(ad,1,sum,na.rm=TRUE)),]
          ad<-ad[!is.na(apply(ad,1,sum,na.rm=TRUE)),]
        }else{
          ad <- as.matrix(t(ad))
          pa <- as.matrix(t(pa))
        }
       
        
        if(length(pa)==0){
            ss<-rep(NA,length(y))
        }else{
            if(length(pa)<length(c)){
                ad<-t(ad)
                pa<-t(pa)
            }
         
         #alex20200130 if(nrow(ad)>0 & sum(apply(ad,2,sum))==0){ # the script stops here, because it doesn't like NA values, need to correct when NA values are there. incl. na.rm=TRUE:
          if(nrow(ad)>0 & sum(apply(ad,2,sum, na.rm=TRUE))!=0 ){
                m<-ad*pa
                ss<-apply(m,2,sum)/apply(ad,2,sum)
          # alex20200130 }else if(nrow(ad)>0 & sum(apply(ad,2,sum))==0){
            }else if(nrow(ad)>0 & sum(apply(ad,2,sum, na.rm=TRUE))==0){
                #Calculate average
                ss<-apply(pa,2,mean)   
            }else{
                ss<-0 
            }
        }
    }
    #if(is.nan(s)) s<-0
    #if(is.na(s)) s<-0
    return(ss)
}

euvalue<-function(todo,E,D,y,c){
    if(todo=="sum")l<-lapply(c(1:nrow(E)),function(x) sumovercountries(D,E$variableUID[x],y,c))  #xavi20180411: I think this is no longer used. Instead, eu28sum
    if(todo=="weight"){
        
      l<-lapply(c(1:nrow(E)),function(x) weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c, x))
     # AL20200130 for debugging
       #x <- 1200:1260
      #cat("\n", x)
      #l <- weightovercountries(D,E$aduids[x],E$variableUID[x],E$adpars[x],y,c, x)
      #debug <- TRUE
      #if(debug){
      #  save(l, file="debug20200131_euvalue.rdata")
      #}
      
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

dt2CO2eq <- function(dt){
  dt <- merge(dt, gwps, by="gas")
  dt <- dt[, (years) := .SD * gwp, .SDcols=years]
  dt <- dt[, unit := "kt CO2 equivalent"]
}

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
        cval<-cval[!cval=="eugirp"]
        cval<-cval[!grepl("NE - values for .*are missing",cval)]
        if(length(cval)==0) cval=""
        #print(length(cval))
        if(length(cval)>1) print(cval)
        return(cval)
    }
    cval<-unlist(lapply(1:length(col),function(x) f(DF,col[x],uid)))
    return(cval)
}
#DF=allagri;uid=curuid;c=allcountries;noeu = TRUE
extractuiddata<-function(DF=NULL,uid=NULL,c,narm=TRUE,noeu=FALSE, cursubm = cursubm){
    
    # Extracts data for one variableUID and 
    # Resorts them thus that the eu-countries (EUC, EUA etc) which are in 'c' and in the data are last
    tmp1 <- unique(DF[variableUID==uid,c("party",years2keep), with=FALSE])
    tmp1 <- rbind(tmp1[! party %in% eu], tmp1[party %in% eu])
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
    diff<-which(round(A,roundn)!=round(B,roundn),arr.ind = TRUE)
    if(length(diff)>0){
        for(d in c(1:nrow(diff))){
            rn<-diff[d,1]
            cn<-diff[d,2]
            checks[ncheck,"test"]<-test
            checks[ncheck,"val1"]<-val1
            checks[ncheck,"val2"]<-val2
            checks[ncheck,"ms"]<-allcountries[!allcountries%in%eu][rn]
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
            }else if(B[rn,cn]==999){
                obs<-paste0(val1," is identified")
                val<-""
                fac<-round(A[rn,cn],roundn)
                
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
                #if(checkname=="cat" & curn>2){
                #    ret<-"Various"
                #}
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
checkforyear<-function(yrstring,yr){
    if(yrstring=="all"){
        yremrt<-TRUE
    }else if(grepl("except",yrstring)){
        yremrt<-as.numeric(unlist(strsplit(gsub("all except: ","",yrstring)," ")[[1]]))
        yremrt<-!yr%in%yremrt
    }else{
        
        yremrt<-as.numeric(unlist(strsplit(yrstring," ")[[1]]))
        yremrt<-yr%in%yremrt
        #cat(yrstring,yremrt,yr,"\n")
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
    options(warn=0)
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

add2allagri<-function(matrix,sec="",cat="",gas="",unit="",sou="",tar="",mea="",msr="",uid="",note="",force=0,DATA=data.frame(),noeu=FALSE){
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
                if(noeu)addnewrow$party[addr]<-allcountries[!allcountries%in%eu][i]
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

remagglevel<-function(T,mt=NULL){
    
    # Checks sector_numbers flags those rows for which 
    # the sector number exists at a more detailed level.
    # E.g. 3.A.1 will be flagged 'FALSE' if there is an entry with 3.A.1.1 
    
    # Requirement: the data frame has a columns with the name 'sector_number'
    # 1. Calculate length of sector_number string
    o1<-sapply(1:nrow(T),function(x) nchar(as.character(T$sector_number[x])))
    # 2. Select all lines which have the same sector number (or a sub-category)
    o2<-sapply(1:nrow(T),function(x) which(grepl(as.character(T$sector_number[x]),as.character(T$sector_number))))
    #o2mt<-sapply(1:nrow(T),function(x) which(T$meastype[x]==T$meastype))
    tmp<-function(x){
        tmpx<-grepl(as.character(T$sector_number[x]),as.character(T$sector_number)) & !grepl("3.B.2.5",T$sector_number[x])
        #print(tmpx)
        tmpy<-T$meastype[x]==T$meastype
        #print(tmpy)
        tmpz<-tmpx & tmpy
        #print(tmpz)
        tmpr<-which(tmpz)
        if(is.null(tmpr))tmpr<-999
        #print(tmpr)
        return(tmpr)
    }
    if(!is.null(mt)) o2<-sapply(1:nrow(T),function(x) tmp(x))
    #if(!is.null(mt)) o2<-sapply(1:10,function(x) tmp(x))
    # 3. Check if the sector_number is an aggregate of any other 
    o3<-sapply(1:nrow(T),function(x) sum(o1[x]<o1[o2[[x]]]))
    o4<-!o3
    return(o4)
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
    if(is.na(y))y<-0
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
        #if(line$party=="ISL") share<-"Island excluded from check"
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

keycategories<-function(){
    keycategories<-read.csv(paste0(issuedir,"../keycategories/EUkeycategoryanalysis_variables_JRC.csv"),stringsAsFactors = FALSE)
    keycategories<-keycategories$variable_UID
    potkeycategories<-alldata[alldata$variableUID%in%keycategories,]
#    if(cursubm == "20190115"){ 
#      gwps2019 <- gwps
#      gwps[6:8] <- 0  
#    }else{
#      stop("Check if the problem with 2.F.1. persists...")
#    }

    
    gasf<-sapply(1:nrow(potkeycategories),function(x) gwps[which(gases==potkeycategories$gas[x])])
    
    if(cursubm == "20190115") gwps <- gwps2019
    
    potkeycategories[,lastyear]<-as.numeric(potkeycategories[,lastyear])*gasf
    
    cols2leave<-paste(names(potkeycategories)[!names(potkeycategories)%in%c(years,"party","notation")],collapse="+")
    arrange<-as.formula(paste(cols2leave,"~ party"))
    potkeycategories<-dcast(potkeycategories,arrange,value.var=lastyear)
    #countries in the list
    cindf<-c(countries3[countries3%in%names(potkeycategories)],"EUC")
    #potkeycategories$EUC<-apply(potkeycategories[,cindf],1,sum,na.rm=TRUE)
    
    rankcategories<-function(x){
        test<-potkeycategories
        test$rank<-frankv(x=abs(potkeycategories[cindf[x]]),order=-1,na.last=TRUE,ties.method="first")
        #test$val<-unlist(potkeycategories[cindf[x]])
        #colnames(test)<-c("variableUID","rank","val")
        test<-test[order(test$rank),]
        test$cumul<-sapply(1:nrow(test),function(y) sum(test[1:y,cindf[x]],na.rm=TRUE))
        test$cumrel<-test$cumul/test$cumul[nrow(test)]
        test[,paste0(cindf[x],"key")]<-test$cumrel<=0.95
        return(test)
    }
    
    for(i in 1:length(cindf)){
        potkeycategories<-rankcategories(i)
    }
    ro<-unlist(lapply(1:length(cindf),function(x) c(cindf[x],paste0(cindf[x],"key"))))
    ro<-c(names(potkeycategories)[!names(potkeycategories)%in%ro],ro)
    potkeycategories<-potkeycategories[order(potkeycategories$sector_number,potkeycategories$category),ro]
    potkeycategories<-potkeycategories[grepl("^3",potkeycategories$sector_number),]
    return(potkeycategories)
}

keycateuc<-function(){
  
  keycategories<-read.csv(paste0(issuedir,"../keycategories/EUkeycategoryanalysis_variables_JRC.csv"),stringsAsFactors = FALSE)
  keycategories<-filter(keycategories,!grepl("Enteric|Farming|Agricultural Soils|Agricultural Residues",name))
  keyemissions<-filter(alldata,variableUID%in%keycategories$variable_UID)
  keyemissions[,years]<-apply(keyemissions[years],2,as.numeric)
  keyeu<-aggregate(x=keyemissions[,years],by=list(keyemissions$variableUID),sum,na.rm=TRUE)
  allkeyvars<-unique(keyemissions[,names(keyemissions)[!names(keyemissions)%in%c("party",years)]])
  keyeu<-merge(allkeyvars,keyeu,by.x="variableUID",by.y="Group.1")
  keyeu$party<-"EUC"
  
  agrikey<-filter(agrimix,meastype=="EM"&gas%in%c("CH4","CO2","N2O"))
  agrikey<-filter(agrikey,!sector_number%in%c("3.A.4","3.B.1.4","3.B.2.4"))
  agridetem<-filter(agridet,meastype=="EM"&gas%in%c("CH4","CO2","N2O"))
  agridetemother<-filter(agridetem,grepl("3.A.4|3.B.[12].4",sector_number))
  agrikey<-rbind(agrikey,agridetemother[,names(agrikey)])
  
  agrikey<-filter(agrikey,party=="EUC")
  agrikey<-agrikey[order(agrikey$sector_number),]
  
  keyeu<-rbind(keyeu[names(agrikey)],agrikey)
  keyeu<-keyeu[order(keyeu$sector_number,keyeu$category),]
  gasf<-sapply(1:nrow(keyeu),function(x) gwps[which(gases==keyeu$gas[x])])
  keyeu[,years]<-(keyeu[,years])*gasf
  keyeu[keyeu$unit=="t CO2 equivalent",years]<-keyeu[keyeu$unit=="t CO2 equivalent",years]/1000
  
  rankcategories<-function(testkey,y){
    testkey[,paste0(y,"rank")]<-frankv(x=abs(testkey[,y]),order=-1,na.last=TRUE,ties.method="first")
    testkey<-testkey[order(testkey[,paste0(y,"rank")]),]
    testkey[,paste0(y,"cumul")]<-sapply(1:nrow(testkey),function(x) sum(testkey[1:x,y],na.rm=TRUE))
    testkey[,paste0(y,"cumrel")]<-testkey[,paste0(y,"cumul")]/testkey[nrow(testkey),paste0(y,"cumul")]
    testkey[,paste0(y,"key")]<-testkey[,paste0(y,"cumrel")]<=0.95
    return(testkey)
  }
  keyeu<-rankcategories(keyeu,"1990")
  keyeu<-rankcategories(keyeu,lastyear)
  keyeuagri<-keyeu
  #keyeuagri<-filter(keyeu,grepl("^3",sector_number))
  
  write.csv(keyeuagri,file=paste0(invloc,"/keycategories/keyeuagri_", cursubm,".csv"))
  write.csv(agrimethods,file=paste0(invloc,"/keycategories/agrimethods_", cursubm,".csv"))
  write.csv(agridetem,file=paste0(invloc,"/keycategories/agridetem_", cursubm,".csv"))
  return(keyeuagri)
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

ipccdefaultexists<-function(EMI){
    # EMI data frame with emissions
    
    ufields<-c(measfields,sectfields,metafields,"variableUID")
    allem<-unique(allagri[allagri$measure=="Emissions",ufields])
    allief<-unique(allagri[allagri$meastype=="IEF",ufields])
    
    mergefields<-ufields[!ufields%in%c("measure","meastype","unit","variableUID")]
    allem<-as.data.frame(apply(allem,2,function(x) as.character(x)))
    allief<-as.data.frame(apply(allief,2,function(x) as.character(x)))
    allemief<-merge(allem,allief,by=mergefields)
    allemief$uidEM<-allemief$variableUID.x
    allemief$uidIEF<-allemief$variableUID.y
    allemief<-allemief[,c("uidEM","uidIEF")]
    
    ipccdefaults<-read.csv(file="ipcc_defaults.csv")
    ipccdefaults<-ipccdefaults[ipccdefaults$variableUID%in%allemief$uidIEF,]
    ipccdefaults<-ipccdefaults[ipccdefaults$IPCC2006!=""|ipccdefaults$IPCC1997!="",]
    ipccdefaults<-ipccdefaults[,c("variableUID","IPCC2006","IPCC1997")]
    
    # Condense defaults if there are multiple
    ipccdefaults<-aggregate(ipccdefaults[,c("IPCC2006","IPCC1997")],by=list(ipccdefaults$variableUID),function(x) paste(x,collapse="-"))
    names(ipccdefaults)[1]<-"variableUID"
    ipccdefaults$IPCC2006[nchar(ipccdefaults$IPCC2006)>15]<-"multiple"
    ipccdefaults$IPCC1997[nchar(ipccdefaults$IPCC1997)>15]<-"multiple"
    
    ipccdefaults<-merge(allemief,ipccdefaults,by.y="variableUID",by.x="uidIEF")
    
    xx<-merge(ipccdefaults,EMI,by.y="variableUID",by.x="uidEM",all.y=TRUE)
    names(xx)[which(names(xx)=="uidEM")]<-"variableUID"
    #xx[is.na(xx)]<-""
    return(xx)
}

multidefaults<-function(D,keep,ignore){
    def<-unique(D[,which(!names(D)%in%ignore)])
    col<-which(names(def)%in%keep[1])
    colr<-which(names(def)%in%keep[2])
    ldefs<-unlist(lapply(c(1:ncol(def)),function(x) length(unique(as.vector(unlist(def[,x]))))))
    #print(def)
    ldefs<-which(ldefs>1)
    
    def<-def[def[,col]!="",]
    #def<-def[!is.na(def[,col]),]
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

# Join with result file from EMRT
joinwithsolved<-function(emrtfile=NULL){
    
    # Reads an 'issue-file' (e.g. country outliers, growth check etc.) at the end of a review cycle
    #       and adds the solved issues to the 'solvedissues' file
    
    if(is.null(emrtfile)){stop("please indicate the file to join with. note that it must be on ",issuedir)}
    
    solvedfile<-read.csv(file=paste0(issuedir,"solvedissues.csv"),stringsAsFactors = FALSE)
    emrtdata<-read.csv(paste0(issuedir,emrtfile),header=TRUE,comment.char="#")
    emrtdata<-emrtdata[emrtdata$Finalization=="resolved",]
    emrtdata<-convert2char(emrtdata)
    emrtdata$explanation[emrtdata$explanation==""]<-emrtdata$Conclusion[emrtdata$explanation==""]
    #Key word 'clarified' if the 'issue' was identified by mistake
    emrtdata<-emrtdata[emrtdata$explanation!="clarified",]
    
    #Adjust the headers to 'normal' ones
    colnames(emrtdata)<-gsub("sec","sector_number",colnames(emrtdata))
    colnames(emrtdata)<-gsub("revyear","year",colnames(emrtdata))
    emrtdata<-filldf(emrtdata,listsolvefields)
    
    #!! Attention - the 'flags' can be used to see which check had been performed, e.g. for recalculations!!
    
    
    #Only first time
    sel<-solvedfile$party==""|solvedfile$party==0
    solvedfile$party[sel]<-solvedfile$issuenr[sel]
    
    emrtdata<-rbind(solvedfile[,listsolvefields],emrtdata[,listsolvefields])
    emrtdata<-emrtdata[order(emrtdata$party,emrtdata$sector_number,emrtdata$category),]
    
    write.csv(emrtdata,file=paste0(issuedir,"solvedissues~",curdate(),".csv"))
    write.csv(emrtdata,file=paste0(issuedir,"solvedissues.csv"))
}

addsolved2check<-function(curcheck,check2ignore=NULL){
    
    # Adds already solved issues to a check so that duplication of issues can be avoided
    # check2ignore are 'checks' (e.g. recalc) which can be ignored
    
#    if(require(readxl)==FALSE){install.packages("readxl", repos = "https://cloud.r-project.org"); library(readxl)
#    } else {library(readxl)}
#    if(require(plyr)==FALSE){install.packages("plyr", repos = "https://cloud.r-project.org"); library(plyr)
#    } else {library(plyr)}
  
    solvedfile<-as.data.frame(read_excel(path=paste0(issuedir,"solvedissues14122017.xlsx"), sheet = 1))
    #solvedfile_kk<-read.csv(file=paste0(issuedir,"solvedissues.csv"),stringsAsFactors = FALSE)
    #add curcheck fields
    #xavi20180131: solvedfile<-solvedfile[,-which(colnames(solvedfile)%in%c("X"))]
    solvedfile1 <- as.data.frame(read_excel(path=paste0(issuedir,"ALL_ISSUES_2018_for solved file.xlsx"), sheet = 1))[, 1:17]
    names(solvedfile1) <- sub("answer", "explanation", names(solvedfile1))
    names(solvedfile1) <- sub("dateansw", "expldate", names(solvedfile1))
    names(solvedfile1) <- sub("comments", "Comments", names(solvedfile1))
    solvedfile1$SOURCE <- "ALL_ISSUES_2018_for solved file.xlsx"
    solvedfile1$year <- 2018

    solvedfile <- rbind.fill(solvedfile1, solvedfile)
    
    solvedfile<-convert2char(solvedfile)
    #curcheck<-convert2char(curcheck)
    #colnames(solvedfile)<-gsub("year","years",colnames(solvedfile))
    colnames(solvedfile)<-gsub("observation","Obs",colnames(solvedfile))
    
    cols2join<-c("party","sector_number","category","meastype","Obs","variableUID","check","unit","gas")
    cols2join<-c("party","sector_number","category","meastype","Obs","check")
    #cols2join<-c("party","sector_number","category","Obs","check")
    if("explanation"%in%colnames(curcheck))cols2join<-c(cols2join,"explanation")
    if("expldate"%in%colnames(curcheck))cols2join<-c(cols2join,"expldate")
    miscolssolve<-cols2join[!cols2join%in%colnames(solvedfile)]
    miscolscheck<-cols2join[!cols2join%in%colnames(curcheck)]
    cols2join<-cols2join[!cols2join%in%miscolscheck]
    
    
    #row.names(curcheck)<-c(1:nrow(curcheck))
    #row.names(solvedfile)<-c((nrow(curcheck)+1):(nrow(curcheck)+nrow(solvedfile)))
    #row.names(curcheck)<-NULL
    #row.names(solvedfile)<-NULL
    
    curcheck<-merge(curcheck,solvedfile,by=cols2join,all=TRUE)
    
    if(!is.null(check2ignore)){
        curcheck<-curcheck[!curcheck$check%in%check2ignore,]
    }
    cols2sort<-c("party","sector_number","category","check","correction","meastype","year","years",
                 "resolved","explanation","Obs","question","Comments","expldate",
                 "issuenr")
    cols2sort<-cols2sort[cols2sort%in%colnames(curcheck)]
    cols2sort<-c(cols2sort,colnames(curcheck)[!colnames(curcheck)%in%cols2sort])
    miscolscheck<-cols2sort[!cols2sort%in%colnames(curcheck)]
    miscolssolve<-cols2sort[!cols2sort%in%colnames(solvedfile)]
    rows2order<-order(curcheck$party,curcheck$check,curcheck$sector_number,curcheck$category)
    curcheck<-curcheck[rows2order,cols2sort]
    curcheck<-convert2char(curcheck)
    curcheck[is.na(curcheck)]<-""
    return(curcheck)
}
export4uba<-function(allagri){
  
    # Export Grrazing shares for CAPRI.
    #  -- added here as this might also be useful for the inventory data (Table 4.D) 
    #     but it is not yet used.
    #alex20191204: source("eugirp_calculateGrazingShares4capri.r")
  
    dtagri<-as.data.table(allagri)
    col2show<-c("party","gas","meastype","source","target","classification","sector_number","category",years,"variableUID")
    t3s1<-dtagri[grepl("EUC|EUA",party)&meastype=="EM"&
                     grepl("3.A|3.B.",sector_number)&
                     category%in%c(livestock,otherlivestock)&
                     gas%in%c("CH4","N2O","NMVOC"),
                 col2show,
                 with=FALSE]
    t3s1<-t3s1[order(party,gas,sector_number,category)]
# alex20200130    write.csv(t3s1,file=paste0(invloc,"/eealocator/table3s1_",cursubm,"~",curdate(),".csv"))
    write.csv(t3s1,file=paste0(invloc,"/tables4eu/table3s1_",cursubm,".csv"))
    write.csv(t3s1,file=paste0(invloc,"/tables4eu/table3s1_",cursubm,"~",curdate(),".csv"))
    
    
    t3as1<-dtagri[grepl("EUC|EUA",party)&
                      meastype%in%c("POP","GEav","YM","IEF","EM")&
                      #meastype%in%c("GE")&
                      grepl("3.A",sector_number)&
                      category%in%c(livestock,otherlivestock)&
                      gas%in%c("CH4","no gas"),
                  col2show,
                  with=FALSE]
    t3as1<-t3as1[order(party,gas,meastype,sector_number,category)]
#alex20200130    write.csv(t3as1,file=paste0(invloc,"/eealocator/tablet3as1_",cursubm,"~",curdate(),".csv"))
    write.csv(t3as1,file=paste0(invloc,"/tables4eu/tablet3as1_",cursubm,".csv"))
    write.csv(t3as1,file=paste0(invloc,"/tables4eu/tablet3as1_",cursubm,"~",curdate(),".csv"))
    View(t3as1)
    
    t3as2<-dtagri[grepl("EUC|EUA",party)&
                      meastype%in%c("WEIGHT","Milk","WORK","PREGNANT","FEEDING","GE","DIGEST")&
                      #meastype%in%c("GE")&
                      grepl("3.A",sector_number)&
                      category%in%c(livestock,otherlivestock)&
                      gas%in%c("CH4","no gas"),
                  col2show,
                  with=FALSE]
    t3as2<-t3as2[order(party,gas,meastype,sector_number,category)]
#alex20200130   write.csv(t3as2,file=paste0(invloc,"/eealocator/tablet3as2_",cursubm,"~",curdate(),".csv"))
    write.csv(t3as2,file=paste0(invloc,"/tables4eu/tablet3as2_",cursubm,".csv"))
    write.csv(t3as2,file=paste0(invloc,"/tables4eu/tablet3as2_",cursubm,"~",curdate(),".csv"))
    View(t3as2)
    
    t3bas1<-dtagri[grepl("EUC|EUA",party)&
                       meastype%in%c("POP","MASS","VSEXC","B0","EM","CLIMA","MCF")&
                       #meastype%in%c("GE")&
                       grepl("3.B.1",sector_number)&
                       category%in%c(livestock,otherlivestock)&
                       gas%in%c("CH4","no gas"),
                   col2show,
                   with=FALSE]
    t3bas1<-t3bas1[order(party,gas,meastype,sector_number,category)]
#alex20200130    write.csv(t3bas1,file=paste0(invloc,"/eealocator/tablett3bas1_",cursubm,"~",curdate(),".csv"))
    write.csv(t3bas1,file=paste0(invloc,"/tables4eu/tablett3bas1_",cursubm,".csv"))
    write.csv(t3bas1,file=paste0(invloc,"/tables4eu/tablett3bas1_",cursubm,"~",curdate(),".csv"))
    View(t3bas1)
    
    t3bb<-dtagri[grepl("EUC|EUA",party)&
                     meastype%in%c("POP","NRATE","NEXC","WEIGHT","EM")&
                     #meastype%in%c("GE")&
                     grepl("3.B.2",sector_number)&
                     category%in%c(livestock,otherlivestock)&
                     gas%in%c("CH4","no gas"),
                 c(col2show),
                 with=FALSE]
    t3bb<-t3bb[order(party,gas,meastype,source,sector_number,category)]
#alex20200130    write.csv(t3bb,file=paste0(invloc,"/eealocator/tablett3bb_",cursubm,"~",curdate(),".csv"))
    write.csv(t3bb,file=paste0(invloc,"/tables4eu/tablett3bb_",cursubm,".csv"))
    write.csv(t3bb,file=paste0(invloc,"/tables4eu/tablett3bb_",cursubm,"~",curdate(),".csv"))
    View(t3bb)
    
#alex20200130    save(t3s1,t3as1,t3as2,t3bas1,t3bb,file=paste0(invloc,"/eealocator/tablett3_",cursubm,"~",curdate(),".RData"))
    save(t3s1,t3as1,t3as2,t3bas1,t3bb,file=paste0(invloc,"/tables4eu/tablett3_",cursubm,".RData"))
    save(t3s1,t3as1,t3as2,t3bas1,t3bb,file=paste0(invloc,"/tables4eu/tablett3_",cursubm,"~",curdate(),".RData"))
    
    # Add Grazing shares
    mms <- t3bb[meastype == "EM"]
    
    
}


savecurstatus<-function(mytext){
    mytext<-gsub(" ","",mytext)
    save(list=savelist,file=gsub(".RData",paste0(mytext,"_",curdate(1),".RData"),rdatallem))
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

plottime<-function(pr=NULL,sc=NULL,mt=NULL,ct=NULL,source=NULL,DF=allagri,export=FALSE){
    
    #if(is.null(pr)){return("no country indicated")}
    graphics.off()
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
    write.csv(time_series,file="time_series.csv")
    
    okplot<-function(){
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
        if(nrow(time_series)>1) legend(x="topleft",ncol=5,legend=timelegend$leg,pch=seq(1,nrow(time_series))%%26)
    }
    if(export==TRUE){
        graphics.off()
        plotformat<-"jpg"
        pwidth=3*2
        pheight=4
        pfont<-1.6*pheight/6
        presolution=1000
        figdir<-paste0(gsub("checks","plots",issuedir),"/trend")
        if (! file.exists(gsub("/$","",figdir))){
            dir.create(file.path(figdir))
            #    setwd(file.path(mainDir, figdate))
        }
        #par(new=TRUE)
        cts<-paste(ct,collaps="-")
        mts<-paste(mt,collaps="-")
        figname<-paste0(plotsdir,"/trend/",pr,"_",sc,cts,"_",mts,".",plotformat,collapse=NULL)
        
        if(plotformat=="pdf") pdf(file=figname,width=pwidth,height=pheight)
        if(plotformat=="png") png(file=gsub("pdf","png",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
        if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
        #Attention: par must be set AFTER device is open, otherwise opens another!
        #Otherwise: figure margins too wide
        par(mar=c(2,4,2,1)) #bot,lef,top,rig
        par(omd=c(0.4,0.4,0,0))
        par(omi=c(0,0,0,0))
        par(oma=c(0,0,0,0))
        par(cex=0.5)
        okplot()
        graphics.off()
    }
    okplot()
    
    
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
        plotformat<-"pdf"
        plotformat<-"png"
        piewidth=3*2
        pieheight=2
        piefont<-1.6*pieheight/6
        pieresolution=1000
        figdir<-gsub("checks","plots",issuedir)
        if (! file.exists(gsub("/$","",figdir))){
            dir.create(file.path(figdir))
            #    setwd(file.path(mainDir, figdate))
        }
        
        # ??? why agrigeneu ???
        piegen<-agrigeneu
        pyears <- t(apply(piegen[,years, with=FALSE],1,"/",apply(piegen[,years, with=FALSE],2,sum)))
        piegen <- cbind(piegen[, -years, with=FALSE], pyears)
        
        
        # Aggregate small sectors 
        sel<-as.logical(piegen[,lastyear, with=FALSE]<0.01)
        piegen <- piegen[sel, sector_number:="Other"]
        piegen <- piegen[, c("sector_number", lastyear), with=FALSE][, value := sum(.SD), by="sector_number", .SDcols=lastyear]
        piegen <- unique(piegen[, .(sector_number, value)])
        

        v<-piegen$sector_number
        n<-nrow(piegen)
        lin<-rep(1,n)
        lin<-rep(0.9,n)
        lin[which(v==piegrep)]<-0.6
        
        figname<-paste0(figdir,"/", cursubm, "/mixplots/",cursubm,"agrimixeu",piegrep,".",plotformat,collapse=NULL)
        if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight)
        if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
        if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
        par(mfrow = c(1,2))
        par(omd=c(0,1,0,1))
        par(mar=c(0,0,0,0)) #bot,lef,top,rig
        par(lwd=0.5)
        pie(piegen[,value],
            init.angle=180,
            radius=pieradius,
            label=paste(piegen$sector_number," ",100*round(piegen[,value],3),"%"),
            clockwise=TRUE,
            col=grey(lin),
            cex=piefont
        )
        #graphics.off()
        
        
        pyears<-t(apply(piedata[,years, with=FALSE],1,"/",apply(piedata[,years, with=FALSE],2,sum)))
        piedata <- cbind(piedata[, -years, with=FALSE], pyears)
        select<-as.logical(piedata[,lastyear, with=FALSE]<0.01)
        piedata <- piedata[select, sector_number:="Other"]
        piedata <- piedata[, c("sector_number", lastyear), with=FALSE][, value := sum(.SD), by="sector_number", .SDcols=lastyear]
        piegen <- unique(piegen[, .(sector_number, value)])
        
        par(lwd=0.5)
        pie(piedata[,value],init.angle=180,
            radius=pieradius,
            label=paste(piedata$sector_number," ",100*round(piedata[,value],3),"%"),
            clockwise=TRUE,col=gray(seq(0.3, 1.0, length = nrow(piedata))),
            cex=piefont
        )
        graphics.off()
    }
    return(1)
}

emissionshareplot<-function(sec,DF=agrimix,eukp=eusubm){
    plotformat <- "jpg"
    pwidth=16
    if (plotformat == "pdf") pwidth=13.5
    pheight=pwidth/1.833
    plotresolution<-plotresolution
    
    acountry<-curcountries[variable==eukp & value==1]$code3
    dfm<-DF[DF$measure=="Emissions"&grepl(sec,DF$sector_number)&DF$party%in%acountry,]
    if(grepl("A|B",sec)){
        #dfm<-agridet[agridet$measure=="Emissions"&grepl(sec,agridet$sector_number)&!grepl(paste0(sec,".4"),agridet$sector_number),]
        #dfm2<-agrimix[agrimix$measure=="Emissions"&grepl(sec,agrimix$sector_number)&!agrimix$sector_number%in%c(paste0(sec,".1")),]
        #dfm<-rbind(dfm,dfm2)
        if(sec=="3.B.2.5"){
            dfm<-allagri[allagri$meastype=="NEXC"&grepl("3.B.2.5",allagri$sector_number)&allagri$source!=""&allagri$party%in%acountry,]
        }
    }
    dfm<-dfm[gas!="NMVOC"]
    dfp<-acountry[as.character(unique(dfm$party))%in%acountry]
    dfc<-sapply(dfp,function(x) country4sub[country4sub$code3==x,"code3"])
    if(sec=="3.B.2.5"){
        dfm<-dcast.data.table(dfm,source +gas ~ party,value.var = lastyear)
        dfm[is.na(dfm)]<-0
        dfl<-dfm$source
        dfo<-order(sapply(dfl,function(x) which(x==manureSystems)))
        print(dfl)
        print(dfo)
        dfl<-dfl[dfo]
        print(dfl)
        dfm<-dfm[dfo,]
    }else if(grepl("A|B",sec)){
        dfm<-dcast.data.table(dfm,category + gas ~ party,value.var = lastyear)
        dfm[is.na(dfm)]<-0
        dfl<-dfm$category
        dfo<-order(sapply(dfl,function(x) which(x==c(livestock,otherlivestock,"Farming"))))
        dfl<-dfl[dfo]
        dfl<-gsub("Farming","Indirect emissions",dfl)
        dfm<-dfm[dfo,]
    }else{
        dfm<-dcast.data.table(dfm,sector_number + gas ~ party,value.var = lastyear)
        dfm[is.na(dfm)]<-0
        dfl<-paste(dfm$sector_number,dfm$gas,sep="-")
    }
    
    dmt<-as.vector(apply(dfm[,3:ncol(dfm)],2,sum))
    
    dfms<-rbind(sapply(1:nrow(dfm),function(x) dfm[x,3:ncol(dfm)]/dmt))
    #row.names(dfms)<-colnames(dfm[3:ncol(dfm)])
    #View(dfms)
    
    figname<-paste0(plotsdir,"/",cursubm, "/mixplots/", cursubm,"emissionshare_",sec,".",plotformat,collapse=NULL)
    
    if(plotformat=="pdf") pdf(file=figname,width=pwidth,height=pheight)
    if(plotformat=="png") png(file=gsub("pdf","png",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
    if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
    
    if(plotformat=="pdf") {
      par(mar=c(4,4,1,20), xpd=TRUE)
    }else{
      par(mar=c(4,4,1,7), xpd=TRUE)
    }
    par(cex=0.7)
    curcols<-grey.colors(length(dfl))
    curcols1<-curcols[as.logical(c(1:length(dfl))%%2)]
    curcols2<-curcols[!as.logical(c(1:length(dfl))%%2)]
    
    #See available palettes with display.brewer.all()
    if(length(dfl)<3){
        #http://colorbrewer2.org/#type=sequential&scheme=BuPu&n=3
        curcols<-c("#9ebcda","#8856a7")
    }else if(length(dfl)>8|sec=="3.B.2.5"){
        curcols2<-rev(brewer.pal(ceiling(length(dfl)/2),"Reds"))
        curcols1<-rev(brewer.pal(length(dfl)-ceiling(length(dfl)/2),"Blues"))
        curcols<-c(curcols1,curcols2)
    }else{
        curcols<-rev(brewer.pal(length(dfl),"Blues"))
    }
    #print(curcols)
    if(plotformat=="pdf"){
      barplot(t(dfms),horiz = FALSE,las=2,cex.axis = 1.5, cex.names = 1.5,offset = 0,col = curcols)
      legend("topright",inset=c(-0.25,0), legend=dfl,fill=curcols,cex=1.5)
    }else{
      barplot(t(dfms),horiz = FALSE,las=2,cex.axis = 0.8,offset = 0,col = curcols)
      legend("topright",inset=c(-0.19,0), legend=dfl,fill=curcols,cex=0.7)
    }
    graphics.off()
}

makegrowthplot<-function(pars,secs,cats="",meastype){
    #figdir<-gsub("checks","plots",issuedir)
    figdir<-paste0(issuedir, "/timeseries")
    
    #All parties for sectors and categories
    #Select sector_number via grepl
    #Predefine vector of categories
    if(paste(cats,collapse="")=="all" | paste(cats,collapse="")==""){
        cats<-unique(unlist(growthcheck$category[grepl(secs,growthcheck$sector_number)&growthcheck$meastype==meastype]))
    }
    
    
    #xavi20180201: t1<-growthcheck[grepl(secs,growthcheck$sector_number)&growthcheck$category%in%cats&growthcheck$meastype==meastype,]
    t1<-growthcheck[grepl(secs,growthcheck$sector_number)&growthcheck$category%in%cats&growthcheck$meastype==meastype&growthcheck$check=="timeseries",]   #xavi20180201: removing row which are not timeseries 
    
    #Do not consider those where an explanation has been given  
    t1<-t1[t1$explanation=="",]
    #Do not consider those where a comment has been given  #xavi20180201: they don't have time series data (they are already resolved)
    t1<-t1[t1$Comments=="",]                               #xavi20180201 
    #cat(secs,cats,meastype)
    nparties<-nrow(t1)
    if(nparties==0){return(1)}
    plotformat<-"pdf"
    #plotformat<-"jpg" #xavi20180130
    pieunit<-5
    omititle<-1
    npiecols<-3
    piewidth<-npiecols*pieunit
    piefont<-pieunit/2
    maxperpage<-4
    pieheight=pieunit*0.5*min(nparties,maxperpage)+omititle
    page=1
    figname<-paste0(figdir,"/",cursubm,"growth",page,"_",gsub("*","",secs),"-",meastype,".",plotformat,collapse=NULL)
    #print(figname)
    pieresolution=300
    if(plotformat=="pdf") pdf(file=figname,width=piewidth,height=pieheight,onefile=TRUE)
    if(plotformat=="png") png(file=gsub("pdf","png",figname),width=piewidth,height=pieheight,unit="in",res=pieresolution)
    if(plotformat=="jpg") jpeg(file=figname,width=piewidth,height=pieheight,unit="in",res=pieresolution)
    #print(paste0("nparties=",nparties,"maxperpage=",maxperpage," npiecols=",npiecols))
    par(mfrow = c(min(nparties,maxperpage),npiecols))
    par(omi=c(0,0.2,omititle,0.2))
    par(cex.main=1.7,cex.axis=1.5,cex.lab=1.5)
    #par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(lwd=0.5)
    
    mcommand<-paste0("makegrowthplot(pars,sec=,\"",secs,"\",cats=c(",paste(cats,collapse=","))
    mcommand<-paste0(mcommand,"),",meastype,")")
    mcommand<-paste0("Growth plots for ",meastype)
    print(mcommand)
    curplot<-0
    pars<-unique(as.vector(unlist(t1$party)))
    for(par in pars){
        #print(paste0("par=",par))  #uncommented Alex
        if(par=="CYP" & meastype == "IEF") next  #xavi20183001: included this because there is an error that needs to be fixed later (the field "gas" is CH4-N2O, in the two rows, so it cannot separate the plots)
        t2<-t1[t1$party==par&grepl(secs,t1$sector_number)&t1$category%in%cats&t1$meastype==meastype,]
        secsl<-unique(as.vector(unlist(t2$sector_number)))
        secsl<-secsl[!secsl=="3.B.2.5 N2O Emissions per MMS"]
        #print(secsl) #uncommented Alex
        for(sec in secsl){
            print(paste0("sec=",sec)) #uncommented Alex
            #if(par=="CYP" & meastype == "AD" & sec == "3.D.1") next  #xavi20183001: included this because there is an error that needs to be fixed later (it has no "classification")
            #if(par=="CYP" & meastype == "IEF" & sec == "3.F.1.2") next  #xavi20180321: included this because there is an error that needs to be fixed later
            #if(par=="DNM" & meastype == "AD" & sec == "3.D.1") next  #xavi20183001: included this because there is an error that needs to be fixed later
            t2<-t1[t1$party==par&t1$sector_number==sec&t1$category%in%cats&t1$meastype==meastype,]
            catsl<-unique(as.vector(unlist(t2$category)))
           # print(paste0("cats=",cats)) #uncommented Alex
            for(cat in catsl){
                #print(paste0("cat=",cat))
                if(cat==0)View(t1)
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
                    cat4header<-cat
                    cat4header<-gsub("Emissions","Em.",cat4header)
                    cat4header<-gsub("Applied to Soils","Appl.",cat4header)
                    cat4header<-gsub("From Managed Soils","",cat4header)
                    cat4header<-gsub("Soil Organic Matter","SOM",cat4header)
                    if(nchar(cat4header)>20)cat4header<-substr(cat4header,1,20)
                    mtext<-paste(par,sec,cat4header,meastype,sep="-")
                    #cat("\n",par,"sec=",sec,"cat=",cat,"cla=",cla,meastype,length(years),length(values))
                    #print("here 6 in eugirp_functions") #Alex
                    
                    ##20200121 - Problem with HU 3.F.1.4 - time series issue for both CH4 and N2O.
                    ##growthcheck needs to be corrected, as it puts 'CH4-N2O' under gas so that the two
                    ##lines can not be distinguished which results in double number of values.
                    ##Need to correct in the function that creates growthcheck
                    ##and add here another loop over the gases.
                    
                    plot(years,values,type="p",main=paste(mtext,": values"),pch=21,col="black",bg="black",cex=2)
                    #print("here 7 in eugirp_functions") #Alex
                    points(years[t4],values[t4],type="p",pch=21,col="red",bg="red",cex=2.5,new=FALSE)
                    if(curplot %% maxperpage == 1) mtext(paste0(mcommand," page ",page), outer = TRUE, cex = 1.5)
                    
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



recalc_uncert <- function(tbl, corrct = FALSE, fctr = 100){
  
  names(tbl) <- paste0("X", seq(1:length(tbl)))
  
  if(corrct == TRUE){
    print(paste0("correcting uncertainties for... ", ct, " by factor = ", fctr))
    tbl[,c(5:6)] <- tbl[,c(5:6)] / fctr 
  }
  
  print(paste0("recalculating other uncertainty estimates for... ", ct))
  tbl$X7 <- sqrt((tbl$X5)^2 + (tbl$X6)^2)
  tbl$X8 <- (tbl$X7*tbl$X4)^2 / sum(tbl$X4, na.rm = T)^2
  tbl$X9 <- ((0.01*tbl$X4) + sum(tbl$X4, na.rm = T) - (0.01*tbl$X3) - sum(tbl$X3, na.rm = T)) / 
    ((( 0.01*tbl$X3 + sum(tbl$X3, na.rm = T))*100) - 
       (((sum(tbl$X4, na.rm = T) - sum(tbl$X3, na.rm = T)) / (sum(tbl$X3, na.rm = T) * 100))))
  tbl$X10 <- abs(tbl$X4 / sum(tbl$X3, na.rm = T))
  tbl$X11 <- tbl$X9 * tbl$X6
  tbl$X12 <- tbl$X10 * tbl$X5 * sqrt(2)
  tbl$X13 <- (tbl$X11)^2 + (tbl$X12)^2
  return(tbl)
  
}


sumerr <- function(relerror, emission, r = 0){   # function to aggrgate categories of Combined Undertainty, etc (e.g. Cattle + Buffalo + Sheep + etc)
                                             # Used for 3A_CH4, 3B_CH4, 3D_N20, 3D_N2O_1, and some subcategories (e.g. Dairy + Non-Dairy Cattle) 
  # relerror: Vector of uncertainties
  # emission: Vector of EM (emissions). relerror and emission must have the same length
  # r <- 0   #check in the old years if it's always 0
  C <- 0
  e <- 0
  
  if (length(relerror) != length(emission)) stop("Please chose arrays of the same length!")
  
  for (un_i in 1:length(relerror)){
    if (!is.numeric(emission[un_i])) emission[un_i] <- 0
    if (is.na(emission[un_i])) emission[un_i] <- 0
    if (is.na(relerror[un_i])) relerror[un_i] <- 0
    
    vari <- ( (relerror[un_i] * emission[un_i]) / 100 )^2
    C <- C + vari
    
    if(relerror[un_i] > 0)  e <- e + abs(emission[un_i])
    
    for (un_j in 1:length(relerror)){
      if (!is.numeric(emission[un_j])) emission[un_j] <- 0
      if (is.na(emission[un_j])) emission[un_j] <- 0
      if (is.na(relerror[un_j])) relerror[un_j] <- 0
      
      varj <- ( (relerror[un_j] * emission[un_j]) / 100 )^2
      C <- C + 2 * r * (vari * varj)^0.5
      
    }# end of for each element of uncertaity vector (j)
  }# end of for each element of uncertaity vector (i)
  
  if(e > 0)  sumerr <- (100 * C^0.5) / e
  if(e == 0)  sumerr <- 0
  
  return(sumerr)
  
}

