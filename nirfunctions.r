# Special characters: use HTML code: 
# https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references

# http://stackoverflow.com/questions/18332463/convert-written-number-to-number-in-r
num2word<-function(num){
    
    tens<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
    hundreds<-c(10,20,30,40,50,60,70,80,90,100)
    wordten<-c("zero","one","two","three","four","five","six","seven","eight","nine","ten",
            "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty")
    wordhun<-c("ten","twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety","hundred")
    
    if(num %in% tens) {w<-wordten[which(tens==num)]}else
        if(num %in% hundreds) {w<-wordhun[which(hundreds==num)]}else{w<-num}
    
    return(w)
    
}
singular<-function(n,words,option=1){
    
    # Convert plural to singular
    # Options:
    # 1: add number (in words if possible) always (ie. one country, two countries)
    # 2: add number only for >2 (ie. the country, the two countries)
    # 0: do not add number (ie the country/the countries)
    num<-num2word(n)
    word<-words
    if(n==1){
        if(words=="countries") word<-"country"
        if(words=="were") word<-"was"
        if(words=="decreases") word<-"decrease"
        if(words=="increases") word<-"increase"
    }
    
    text1<-paste0(num," ",word)
    if(option==0) text1<-word
    if(option==2 & n==1) text1<-word
    
    return(text1)
    
}
firstlow<-function(c){
    nc<-nchar(c)
    cnew<-c
    # Do leave upper-case if there is a space after first character (e.g. N input)
    #                     or if the 2nd character is also upper case (e.g. VS excretion)
    if(nc>1) if((substr(c,2,2)!=" ")&(substr(c,2,2)==tolower(substr(c,2,2)))) 
        cnew<-paste0(tolower(substr(c,1,1)),substr(c,2,nc))
    return(cnew)
}

splittable<-function(D,n=2){
    newnr<-ceiling(nrow(D)/n)
    newcol<-rep("&#09;",newnr)
    newrow<-rep(" ",ncol(D))
    newrow<-rep("&#09;",ncol(D))
    newcol<-rep("&#124;",newnr) #Vertical 
    names(newcol)<-"&#09;"
    newe<-newnr
    Dhead<-names(D)
    newD<-D%>%slice(1:newnr)
    for(i in c(2:n)){
        news<-newe+1
        newe<-newe+newnr
        newD2<-slice(D,news:min(nrow(D),newe))
        if(nrow(newD2)<newnr)newD2[(nrow(newD2)+1):newnr,]<-newrow
        newn<-c(names(newD),"",Dhead)
        newD<-cbind(newD,newcol,newD2)
        #newD<-cbind(newD,newD2)
        names(newD)<-newn
    }
    return(newD)
}

deorincrease<-function(curtr){
    # returns whether curtr is a decrease or an increase
    # attention! negative values mean INCREASE
    if(curtr>0){text<-paste0(" decrease")}else
        if(curtr<0){text<-paste0(" increase");curtr<--curtr}else{
            text<-paste0(" no change")}
    return(text)
}
trendstrength<-function(curtr){
    check<-abs(1-curtr)
    if(check<0.01){str<-" barely"}else 
        if(check<0.05){str<-" slightly"}else
        if(check<0.10){str<-" moderately"}else
        if(check<0.15){str<-" clearly"}else
        if(check<0.25){str<-" considerably"}else
        if(check<0.50){str<-" strongly"}else
        {str<-" very strongly"}
  return(str)  
}
trendtext<-function(curtr,dig=0){
    # input: trend as fraction
    # output: string indicating whether the trend was increasing or decreasing
    #         together with the trend (converted) in %
    ncurtr<-length(curtr)
    
    # curtr>1 means increase ... results in negative curtr
    curtr<-1-mean(as.numeric(curtr))
    curtr<-round(curtr,2+dig)*100
    text<-paste0(deorincrease(curtr),"d")
    if(ncurtr>1){text<-paste0(text, " on average")}
    curtr<-abs(curtr)
    text<-paste0(text," by ",curtr,"%")
    perc<-paste0(curtr,"%")
    return(list(text,perc))
}

absval<-function(curch,d=0){
    curch<-abs(curch)
    
    funit<-paste0(" ",curunit)
    if(curunit=="")funit<-curunit
    if(curch>1000){
        if(curunit=="kt CO2 equivalent"){funit<-" Mt CO2-eq"}
        if(curunit=="1000s"){funit<-" mio heads"}
    }else{
        if(curunit=="kt CO2 equivalent"){funit<-" kt CO2-eq"}
        if(curunit=="1000s"){funit<-" thousand heads"}
    }
    
    if(curch>100000){
        val<-round(curch/1000,0)
        
    }else
    if(curch>1000){
        val<-round(curch/1000,1)
        
    }else{
        val<-round(curch,0)
    }
    if(val==0) val<-round(curch,1)
    if(val==0) val<-as.numeric(format(curch,digits=2))
    if(d>0)val<-as.numeric(format(curch,digits=d))
    val<-paste0(val,funit)
    return(val)
}
laender<-function(pp){
    #pp: vector with country acronyms
    pl<-unlist(lapply(c(1:length(pp)),function(x) countriesl[which(countries2==pp[x])]))
    npl<-length(pp)
    laender<-pl[1]
    if(npl==2){laender<-paste(pl,collapse=" and ")}
    if(npl>2){laender<-paste(paste(pl[1:(npl-1)],collapse=", "),pl[npl],sep=" and ")}
    return(laender)
}
percent<-function(x,d=0){
    #Return as percentage but increase digits for small number <<1
    rd<-abs(min(0-d,unlist(ceiling(log10(x)))))
    p<-paste0(round(100*x,rd),"%")
    return(p)
}
capmeasure<-function(curmeasure,t="short"){
    capmeasure<-paste0(curmeasure," in source category ",curcattext," ")
    if(curmeasure!="Emissions"){capmeasure<-paste0(curmeasure," ")}
    if(curmeasure=="Population"){capmeasure<-paste0(curcat," population ")}
    if(curmea%in%meas2popweight[!meas2popweight%in%c("IEF")]){
        if(t=="long"){
            explain<-paste0(", a parameter used for calculating ",curgas," emissions in source category ",curcattext,", ")
        }else{explain<-paste0(" in source category ",curcattext," ")}
        capmeasure<-paste0("The ",gsub(" $","",firstlow(curmeasure)),explain)
    }
    if(curmea=="IEF"){capmeasure<-paste0("The ",firstlow(capmeasure),"for ",curgas,
                                             " emissions in source category ",curcattext," ")}
    return(capmeasure)
}

# Captions ####
sharefigurecaption<-function(eukp,cursec,lastyear){
    cap<-paste0("&#09;Share of source category ",cursec," ",
                "on total ",eukp," agricultural emissions (left panel) ",
                "and decomposition into its sub-categories (right panel). ",
                "The percentages refer to the emission in the year ",lastyear,".")
    return(cap)
}
msconttablecaption<-function(){
    cap<-paste0("&#09;",curcattext,": Member States&apos; contributions to total GHG and ",paste(curgas,collapse=", ")," emissions")
    return(cap)
}
trendfigurecaption<-function(eukp,sec=cursec,lastyear){
    cap<-paste0("&#09;",sec,": Trend in ",firstlow(curmeasure)," in the ",eukp,
                " and the countries contributing most to ",eukp," values",
                " including their share to ",eukp," emissions in ",lastyear,"")
    return(cap)
}
trendiefcaption<-function(eukp,sec=cursec,lastyear){
    cap<-paste0("&#09;",sec,": Trend in ",firstlow(curmeasure)," in the ",eukp,
                " and range of values reported by countries")
    return(cap)
}
paratablecaption<-function(eukp,sec=cursec,lastyear){
    if(curunit==""){uu<-"-"}else{uu<-curunit}
    cap<-paste0("&#09;",curcattext,": Member States&apos; and ",eukp," ",
                curmeasure," (",uu,")")
    return(cap)
}
# Paragraphs ####
text2mscontr<-function(sec=cursec){
    if(curcat==""){curcat<-curseclong}
    sen1<-paste0("Total GHG and ",curgas," ",tolower(curmeasure)," by Member States from ",
                 sec," ",curseclong," are shown in ",tabs(paste0("tab",sec,"mscontr"),display="cite"),".")
    
    sen2<-paste0(" Between 1990 and ",lastyear,", ",curgas," emission from ",
                 curcat," ",trendtext(curtrend)[[1]]," or ",absval(curtrendabs),". ")
    
    if(alltrend$party[decrease[1]]==alltrend$party[decreaseabs[1]]){and<-" and also"}else{
        and<-paste0(" and in ",laender(alltrend$party[decreaseabs[1]]))
    }
    sen3<-paste0(" The decrease was largest in ",laender(alltrend$party[decrease[1]]),
                 " in relative terms (",trendtext(alltrend$trend[decrease[1]])[[2]],")",
                 and," in absolute terms (",trendtext(alltrend$trend[decreaseabs[1]])[[2]]," or ",
                 absval(alltrend$diff[decreaseabs[1]]),").")
    
    sen4<-paste0(" From ",lastyear2," to ",lastyear," emissions ",trendtext(lasttrend,1)[[1]],".")
    
    return(paste0(sen1,sen2,sen3,sen4))
}

text2trend<-function(fig="",option=0){
    

    capgas<-paste0(" ",curgas)
    if(curgas=="no gas"){capgas<-""}
    
    if(option==1){
        sent1<-""
    }else{
        sent1<-paste0(capmeasure(curmeasure),deorincrease(1-curtrend),"d ",trendstrength(curtrend),
                      " in ",eukp," by ",
                      percent(abs(1-curtrend))," or ",absval(curtrendabs),". ")
    }
    
    sent2a<-paste0(fig," shows the trend of ")
    if(curmeasure=="Emissions"){sent2b<-"emissions"}else{sent2b<-firstlow(capmeasure(curmeasure))}
    sent2c<-paste0(" indicating the countries contributing most to ",eukp," total. ")
    sent2<-paste0(sent2a,sent2b,sent2c)
    
    curcatgas<-paste0(firstlow(curcat))
    if(capgas!="")curcatgas<-paste0(curcatgas,capgas)
    if(grepl("Indirect Emissions",curcat)){curcatgas<-paste0("indirect ",capgas)}
    if(grepl("Direct N2O Emissions",curcat)){curcatgas<-paste0("direct ",capgas," emissions")}
    if(grepl("Indirect N2O Emissions",curcat)){curcatgas<-paste0("indirect ",capgas," emissions")}
    if(grepl("Direct N2O Emissions",curcat)&curmea=="EM"){curcatgas<-paste0("direct ",capgas)}
    if(grepl("Indirect N2O Emissions",curcat)&curmea=="EM"){curcatgas<-paste0("indirect ",capgas)}
    if(curmea%in%c("POP","AD"))curcatgas<-curcat
    sent3<-paste0("The ",singular(10,"countries",2)," with highest ",firstlow(curmeasure),
                  " together accounted for ",
                  percent(sum(allshare$share[1:nshare]),d=1)," of ",curcatgas,
                  " ",firstlow(curmeasure),". ") 
    
    if(decreasen>0 & increasen>0){
        sent4<-paste0(curmeasure," decreased in ",decreasen," countries and increased in ",singular(increasen,"countries",1),". ")}
    if(decreasen==0 & increasen>0){
        sent4<-paste0(curmeasure," increased in all ",singular(increasen,"countries",2),". ")}
    if(decreasen>0 & increasen==0){
        sent4<-paste0(curmeasure," decreased in all ",singular(decreasen,"countries",2),". ")}
                 
    
    if(decreasen>0){
        if(decreasen>decreasencnt){
            if(decreasencnt>2){
                temp<-paste0("The ",singular(decreasencnt,"countries",2)," with the largest decreases ",singular(decreasencnt,"were",0)," ")
            }else{
                temp<-"Largest decreases occurred in "
            }
        }else{temp<-paste0(curmeasure," decreased in ")}
        sent5<-paste0(temp,laender(decreasecnt)," with a total absolute decrease of ",absval(decreasescnt),". ")
    }else{sent5<-""}
    
    if(increasen>0){
        if(increasen>increasencnt){
            if(increasencnt>2){
                temp<-paste0("The ",singular(increasencnt,"countries",2)," with the largest increases ",singular(decreasencnt,"were",0)," ")
            }else{
                temp<-"Largest increases occurred in "
            }
        }else{temp<-paste0(curmeasure," increased in ")}
        sent6<-paste0(temp,laender(increasecnt)," with a total absolute increase of ",absval(increasescnt),".")
    }else{sent6<-""}
    
    return(paste0(sent1,sent2,sent3,sent4,sent5,sent6))
}


text2ief<-function(fig="",tab=""){

    capgas<-paste0(" ",curgas)
    if(curgas=="no gas"){capgas<-""}
    what<-curmeasure
    
    # The implied emission factor for CH4 emissions in source category 3.A.1 - Cattle 
    # increased slightly in EU28 by 3.2% or 2.17 kg/head/year. 
    if(is.na(curtrend)){
        sent1<-paste0(capmeasure(curmeasure,"long"),"could not be evaluated at ",eukp," level. ")
        sent2<-""
    }else{
        sent1<-paste0(capmeasure(curmeasure,"long"),deorincrease(1-curtrend),"d "," in ",eukp,trendstrength(curtrend),
                      " by ",percent(abs(1-curtrend),d = 1)," or ",absval(curtrendabs,d=3),
                      " between ",firstyear," and ",lastyear,". ")
        
        # Figure 1.1 shows the trend of the IEF in EU28 indicating also the range of 
        # values used by the countries contributing between 1990 and 2013. 
        sent2a<-paste0(fig," shows the trend of the ",what," in ",eukp)
        sent2c<-paste0(" indicating also the range of values used by the countries. ")
        sent2<-paste0(sent2a,sent2c)
    }
    sent3<-paste0(tab," shows ",firstlow(capmeasure(curmeasure)),
                  " for the years ",firstyear," and ",lastyear,
                  " for all Member States and ",eukp,". ")

    # The IEF decreased in 5 countries and increased in 22 countries. 
    if(decreasen>0 & increasen>0){
        if(curmea%in%c("IEF")){sent4<-paste0("The ",what}else{sent4<-what}
        sent4<-paste0(sent4," decreased in ",singular(decreasen,"countries",1)," and increased in ",singular(increasen,"countries",1),". ")
        if(stablen>0){sent4<-paste0(sent4,"It was in ",lastyear," at the level of ",firstyear," in ",singular(stablen,"countries",1),". ")}
        if(missing>0){sent4<-paste0(sent4,"No data were available for ",singular(missing,"countries",1),". ")}
    }else{
        if(stablen>0){
            sent4<-paste0("The reported ",tolower(what)," in ",lastyear," was at the level of ",firstyear," in ",singular(stablen,"countries",1)," and ")
            addother<-"other "
        }else{
            sent4<-""
            addother<-what
        }
        if(decreasen==0 & increasen>0){
            sent4<-paste0(sent4," increased in all ",addother,singular(increasen,"countries",2),". ")}
        if(decreasen>0 & increasen==0){
            sent4<-paste0(sent4," decreased in all ",addother,singular(decreasen,"countries",2),". ")}
    }

    
    # Largest decreases occurred in Croatia with a mean absolute decrease of 50 kg/head/year. 
    if(decreasen>0){
        if(decreasencnt==1){amean<-" an"}else{amean<-" a mean"}
        if(decreasen>decreasencnt){
            if(decreasencnt>2){
                temp<-paste0("The ",singular(decreasencnt,"countries",2)," with the largest decreases ",singular(decreasencnt,"were",0)," ")
            }else{
                temp<-paste0("The largest ",singular(decreasencnt,"decreases",0)," occurred in ")
            }
        }else{temp<-paste0("Decreases occurred in ")}
        sent5<-paste0(temp,laender(decreasecnt)," with",amean," absolute decrease of ",absval(decreasemcnt),". ")
    }else{sent5<-""}
    
    # The three countries with the largest increases were Slovakia, Estonia and Czech Republic 
    # with a mean absolute increase of 18 kg/head/year.
    if(increasen>0){
        if(increasencnt==1){amean<-" an"}else{amean<-" a mean"}
        if(increasen>increasencnt){
            if(increasencnt>2){
                temp<-paste0("The ",singular(increasencnt,"countries",2)," with the largest increases ",singular(increasencnt,"were",0)," ")
            }else{
                temp<-paste0("The largest ",singular(increasencnt,"increases",0)," occurred in ")
            }
        }else{temp<-paste0("Increases in ");if(increasencnt==1){temp<-paste0("There was an increase in ")}}
        sent6<-paste0(temp,laender(increasecnt)," with",amean," absolute increase of ",absval(increasemcnt),". ")
    }else{sent6<-""}

    
    return(paste0(sent1,sent2,sent3,sent4,sent5,sent6))
}