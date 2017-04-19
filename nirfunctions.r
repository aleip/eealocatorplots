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
vector2words<-function(curvector){
    
    n<-length(curvector)
    w<-paste(curvector[1:(n-1)],collapse=", ")
    w<-paste0(w,", and ",curvector[n])
    return(w)
    
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
        if(curunit=="1000s"){funit<-" million heads"}
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
    pp<-as.character(pp)
    pl<-unlist(lapply(c(1:length(pp)),function(x) country4sub$thename[which(country4sub$code2==pp[x])]))
    npl<-length(pp)
    laender<-pl[1]
    if(npl==2){laender<-paste(pl,collapse=" and ")}
    if(npl>2){laender<-paste(paste(pl[1:(npl-1)],collapse=", "),pl[npl],sep=" and ")}
    return(laender)
}
percent<-function(x,d=0){
    #Return as percentage but increase digits for small number <<1
    rd<-abs(min(0-d,unlist(ceiling(log10(abs(x))))))
    p<-paste0(round(sign(x)*100*abs(x),rd),"%")
    return(p)
}

curcatnew<-function(curcat){
    # curcat: category if category if 'Farming' then it will be curseclong
    if(curcat=="Farming"){curcat<-curseclong}
    if(curcat=="Direct N2O Emissions from Managed Soils") curcat<-"Direct N2O emissions from managed soils"
    return(curcat)
}
seccatsou<-function(sec=cursec,cat=curcat,cla=curcla,tar="",typ=""){
    
    seccatsou<-paste0(sec," - ",gsub("Farming","",cat),gsub("Agricultural Soils","",cla),tar,typ)
    return(seccatsou)
    
}
curcatlong<-function(curcat,cursec=cursec){
    # curcattext: combination of sector and category (e.g. 3.A.1 Cattle)
    curcattext<-paste0(cursec," - ",curcat)
    if(grepl("3.D.1",cursec)){curcattext<-paste0(cursec," - ",curcat," ",curseclong)}
    if(curmeasure=="Atmospheric deposition"){curcattext<-paste0(cursec," - ",curmeasure," from ",curcat)}
    if(cursec=="3.D.1.1" | cursec=="3.D.1.2"){curcattext<-gsub("Managed Soils",curseclong,curcattext)}
    if(cursec=="3.D.2.1" | cursec=="3.D.2.2"){curcattext<-paste0(cursec," - Indirect N2O Emissions from ",curseclong)}
    if(cursec=="3.D.1.3"){curcattext<-paste0(cursec," - ",curcat)}
    if(grepl("3.D.AI.1",cursec)){curcattext<-"3.D.2.1 - Indirect emissions from Atmospheric Deposition"}
    if(grepl("3.D.AI.2",cursec)){curcattext<-"3.D.2.2 - Indirect emissions from Atmospheric Deposition"}
    if(grepl("3.D.AI.3",cursec)){curcattext<-"3.D.2.2 - Indirect emissions from Nitrogen Leaching and Run-off"}
    if(cursec=="3.B.2.5" & curmeasure=="leaching"){curcattext<-"3.B.2.5 - Indirect N2O emissions from leaching from manure management"}
    if(cursec=="3.B.2.5" & curmeasure=="Atmospheric"){curcattext<-"3.B.2.5 - Indirect N2O emissions from manure management"}
    curcattext<-paste0("*",gsub(" $","",curcattext),"*")
    return(curcattext)
}
curmeasurenew<-function(curmeasure){
    if(curmeasure=="Atmospheric deposition"){if(curmea=="IEF")curmeasure<-"Implied emission factor"}
    if(curmeasure=="Nitrogen leaching and run-off"){if(curmea=="IEF")curmeasure<-"Implied emission factor"}
    curmeasure<-gsub(" to cropland and grassland","",curmeasure)
    if(grepl("from application of ",curmeasure)){curmeasure<-firstup(gsub("N input from ","",curmeasure))}
    if(grepl("N input from ",curmeasure)){curmeasure<-gsub("input from","from applied",curmeasure)}
    curmeasure<-gsub(" \\(average\\)","",curmeasure)
    curmeasure<-gsub("inputs of N","N inputs",curmeasure)
    curmeasure<-gsub("nox|Nox","NOx",curmeasure)
    curmeasure<-gsub("Nh3|nh3","NH3",curmeasure)
    curmeasure<-gsub(" n "," N ",curmeasure)
    return(curmeasure)
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
    if(cursec=="3.B.2.5"){capmeasure<-paste0(capmeasure,"- *Indirect N2O emissions* ")}
    return(capmeasure)
}

# Captions ####
sharefigurecaption<-function(eusubml,cursec,lastyear){
    cap<-paste0("&#09;Share of source category ",cursec," ",
                "on total ",eusubml," agricultural emissions (left panel) ",
                "and decomposition into its sub-categories (right panel). ",
                "The percentages refer to the emissions in the year ",lastyear,".",
                if(cursec=="3.B.2"){"3.B.2.1-3.B.3.4: emissions by animal types (cattle, sheep, swine, other livestock); 3.B.2.5:Indirect emissions from manure management."}else
                if(cursec=="3.D.1"){paste0(" Categories 3.D.1.1-3.D.1.5: direct N2O emissions by N source ",
                                           "(inorganic fertilizers, organic fertilizers, urine and dung deposited by grazing animals, ",
                                            "crop residues and mineralization of soil organic matter); ",
                                            "category 3.D.1.6: cultivation of histosols.")}
                )
    return(cap)
}
sharemsfigurecaption<-function(eusubml,sec=cursec,lastyear){
    cap<-paste0("&#09;Decomposition of emissions in source category ",curcattext," ",
                "into its sub-categories by Member State in the year ",lastyear,". ",
                if(grepl("D.1",sec)){
                    paste0("3.D.1.1 inorganic N fertilisers, ",
                           "3.D.1.2 organic N fertilisers, ",
                           "3.D.1.3 urine and dung deposited by grazing animals, ",
                           "3.D.1.4 crop residues incorporated in the soil, ",
                           "3.D.1.5 mineralisation/immobilisation associated with loss/gain of soil organic matter, and ",
                           "3.D.1.6 cultivation of organic soils (histosols).")
                }else if(grepl("D.2",sec)){
                    paste0("3.D.2.1 Atmospheric Deposition and 3.D.2.2 Nitrogen Leaching and Run-off.")
                }else{""}
    )
    if(sec=="3.B.2.5")cap<-paste0("&#09;Decomposition of manure nitrogen handled in source category ",curcattext," ",
                             "into the different manure management systems by Member State in the year ",lastyear,". ")
    print(cap)
    return(cap)
}

msconttablecaption<-function(){
    cap<-paste0("&#09;",curcattext,": Member States&apos; contributions to total GHG and ",paste(curgas,collapse=", ")," emissions")
    return(cap)
}
trendfigurecaption<-function(eusubml,sec=cursec,lastyear){
    cap<-paste0("&#09;",sec,": Trend in ",
                if(curmea=="POP"){paste0(firstlow(curcat)," ")},
                firstlow(curmeasure)," in the ",eusubml,
                " and the countries contributing most to ",eusubml," values",
                " including their share to ",eusubml," emissions in ",lastyear,"")
    return(cap)
}
trendiefcaption<-function(eusubml,sec=cursec,lastyear){
    cap<-paste0("&#09;",sec,": Trend in ",firstlow(curmeasure)," in the ",eusubml,
                " and range of values reported by countries")
    return(cap)
}
paratablecaption<-function(eusubml,sec=cursec,lastyear){
    if(curunit==""){uu<-"-"}else{uu<-curunit}
    uvprep<-gsub("\\*","",gsub("\\\"","",paste(curtable["Member State"],collapse="")))
    if(grepl("EU28",uvprep)){uv<-paste0("and ",eusubml," ")}else{uv<-""}
    cap<-paste0("&#09;",curcattext,": Member States&apos; ",uv                ,
                firstlow(curmeasure)," (",uu,")")
    return(cap)
}
compademplots<-function(eusubml,sec=cursec,cat=curcat){
    cap<-paste0("&#09;",sec,": Comparison of ",cat," ",firstlow(curmeasure)," in the ",eusubml,
                " and range of values reported by countries in the ",multiref[1]," and the ",multiref[2],".")
    return(cap)
}
compareplots<-function(eusubml,sec=cursec,cat=curcat){
    cap<-paste0("&#09;",sec,": (a) Average ",cat," ",firstlow(curmeasure)," in the ",eusubml,
                " in the ",multiref[1]," and the  ",multiref[2],
                ", (b) Importance of range of difference in the databases for total ",eusubml," value",
                " and (c) Relative difference of mean values by country.")
    return(cap)
}
# Paragraphs ####
text2mscontr<-function(sec=cursec){
    if(curcat==""){curcat<-curseclong}
    sen1<-paste0("Total GHG and ",curgas," ",tolower(curmeasure)," by Member State from ",
                 sec," *",curseclong,
                 #if(curcat%in%c("Other Livestock","Cattle","Swine","Sheep")){paste0(" - ",tolower(curcat))},
                 "* are shown in ",tabs(paste0("tab",sec,"mscontr"),display="cite"),
                 " by Member State ",if("IS" %in% allcountries){"plus Iceland, "},
                 "and the total EU-28",if("IS" %in% allcountries){" and EU-28+ISL"},
                 " for the first and the last year of the inventory (",firstyear," and ",lastyear,").",
                 " Values are given in kt CO2-eq. ",
                 if(cursec=="3.A"){"In this category GHG and CH4 columns have the same values, as no other greenhouse gases are produced in the enteric fermentation process."})
    
    sen2<-paste0(" Between 1990 and ",lastyear,", ",curgas," emission in this source category ",
                 trendtext(curtrend)[[1]]," or ",absval(curtrendabs),". ")
    
    if(alltrend$party[decrease[1]]==alltrend$party[decreaseabs[1]]){and<-" and also"}else{
        and<-paste0(" and in ",laender(alltrend$party[decreaseabs[1]]))
    }
    sen3<-paste0(" The decrease was largest in ",laender(alltrend$party[decrease[1]]),
                 " in relative terms (",trendtext(alltrend$trend[decrease[1]])[[2]],")",
                 and," in absolute terms (",
                 #trendtext(alltrend$trend[decreaseabs[1]])[[2]]," or ",
                 absval(alltrend$diff[decreaseabs[1]]),").")
    
    sen4<-paste0(" From ",lastyear2," to ",lastyear," emissions in the current category ",trendtext(lasttrend,1)[[1]],".")
    
    return(paste0(sen1,sen2,sen3,sen4))
}
text2shareeu<-function(){
    sent1<-paste0(curgas," ",firstlow(capmeasure(curmeasure))," are ",
                  percent(eushareghg,1)," of total ",eukp," GHG emissions and ",
                  percent(eusharencgg)," of total ",eukp," ",curgas," emissions. They make ",
                  percent(eushareagrighg,1)," of total agricultural emissions and ",
                  percent(eushareagrincgg)," of total agricultural ",curgas," emissions.")
    return(sent1)
}

text2sharems<-function(sec=cursec,curcolor='grey'){
 
    text<-paste0("Regarding the origin of emissions in the different Member States, ",
                 figs(paste0("fig",cursec,"sharems"),display="cite"),
                 " shows the distribution of ",
                 if(grepl("D.1",cursec)){"direct "}else if(grepl("D.2",cursec)){"indirect "},
                 curgas," emissions from ",
                 if(grepl("D",cursec)){"managed soils"}else{gsub("n2o","N2O",tolower(curseclong))},
                 " by ", 
                 if(grepl("A|B",cursec)){"livestock category"}else{"emission source"},
                 " in all Member States and in the ",eusubml,".",
                 " Each bar represents the total emissions of a country in the current emission category, ",
                 "where different shades of ",curcolor," correspond to the emitting ",
                 if(grepl("A|B",cursec)){"animal types"}else{"sub-categories"},".")
    return(text)
    
    
}

text2trend<-function(fig="",option=0){
    

    capgas<-paste0(" ",curgas)
    if(curgas=="no gas"){capgas<-""}
    
    if(option==1){
        sent1<-""
    }else{
        sent1<-paste0(capmeasure(curmeasure),deorincrease(1-curtrend),"d ",trendstrength(curtrend),
                      " in ",eusubml," by ",
                      percent(abs(1-curtrend))," or ",absval(curtrendabs),
                      " in the period ",firstyear," to ",lastyear,". ")
    }
    
    sent2a<-paste0(fig," shows the trend of ")
    if(curmeasure=="Emissions"){sent2b<-"emissions"}else{sent2b<-firstlow(capmeasure(curmeasure))}
    sent2c<-paste0(" indicating the countries contributing most to ",eusubml," total. ")
    
    sent2d<-paste0("The figure represents the trend in ",curgas," ",firstlow(curmeasure),
                   if(curmea!="POP" & cursec!="3.D.2.2"){paste0(" from ",gsub(" n "," N ",tolower(curseclong)))},
                   " for the different Member States along the inventory period. ")
    sent2<-paste0(sent2a,sent2b,sent2c,sent2d)
    if(cursec=="3.A.1"&curmea=="EM"&curcat=="Dairy Cattle"){
        sent2<-paste0(sent2,"Each bar shows the emissions in kt accumulated by the different Member States in a specific year. Every Member State is represented by a different pattern. Only the first ten Member States with the highest emission shares are shown separately, while the emissions corresponding to the remaining countries are represented under ‘other’ label. In red points, we see the total emissions of the category for the ",eusubml,". The legend on the right shows the Member States corresponding to each pattern and the share of their emissions over the EU-28 total. ")
    }
    if(fig==""){sent2<-""}
    
    curcatgas<-paste0(tolower(curcat))
    if(capgas!="" & !curmea%in%c("AD","POP"))curcatgas<-paste0(curcatgas,capgas)
    if(grepl("Indirect Emissions",curcat)){curcatgas<-paste0("indirect ",capgas)}
    if(grepl("Direct N2O Emissions",curcat)){curcatgas<-paste0("direct ",capgas," emissions")}
    if(grepl("Indirect N2O Emissions",curcat)){curcatgas<-paste0("indirect ",capgas," emissions")}
    if(grepl("Direct N2O Emissions",curcat)&curmea=="EM"){curcatgas<-paste0("direct ",capgas)}
    if(grepl("Indirect N2O Emissions",curcat)&curmea=="EM"){curcatgas<-paste0("indirect ",capgas)}
    #if(curmea%in%c("AD","POP"))curcatgas<-curcat
    sent3<-paste0("The ",singular(10,"countries",2)," with the highest ",firstlow(curmeasure),
                  " accounted together for ",
                  percent(sum(allshare$share[1:nshare]),d=1),
                  " of the total. ")
                  #" of ",curcatgas," ",firstlow(curmeasure),". ") 
    
    if(decreasen>0 & increasen>0){
        sent4<-paste0(firstup(curmeasure)," decreased in ",singular(decreasen,"countries",1)," and increased in ",singular(increasen,"countries",1),". ")}
    if(decreasen==0 & increasen>0){
        sent4<-paste0(firstup(curmeasure)," increased in all ",singular(increasen,"countries",2),". ")}
    if(decreasen>0 & increasen==0){
        sent4<-paste0(firstup(curmeasure)," decreased in all ",singular(decreasen,"countries",2),". ")}
                 
    
    if(decreasen>0){
        if(decreasen>decreasencnt){
            if(decreasencnt>2){
                temp<-paste0("The ",singular(decreasencnt,"countries",2)," with the largest decreases ",singular(decreasencnt,"were",0)," ")
            }else{
                temp<-"The largest decreases occurred in "
            }
        }else{temp<-paste0(firstup(curmeasure)," decreased in ")}
        sent5<-paste0(temp,laender(decreasecnt)," with a total absolute decrease of ",absval(decreasescnt),". ")
    }else{sent5<-""}
    
    if(increasen>0){
        if(increasen>increasencnt){
            if(increasencnt>2){
                temp<-paste0("The ",singular(increasencnt,"countries",2)," with the largest increases ",singular(increasencnt,"were",0)," ")
            }else{
                temp<-"Largest increases occurred in "
            }
        }else{temp<-paste0(firstup(curmeasure)," increased in ")}
        sent6<-paste0(temp,laender(increasecnt),", with a total absolute increase of ",absval(increasescnt),".")
    }else{sent6<-""}
    
    return(paste0(sent1,sent2,sent3,sent4,sent5,sent6))
}


text2ief<-function(fig="",tab=""){

    capgas<-paste0(" ",curgas)
    if(curgas=="no gas"){capgas<-""}
    what<-firstlow(curmeasure)
    #what<-gsub("nh3","NH3",what)
    #what<-gsub("nox","NOx",what)
    
    # The implied emission factor for CH4 emissions in source category 3.A.1 - Cattle 
    # increased slightly in EU28 by 3.2% or 2.17 kg/head/year. 
    if(is.na(curtrend)){
        sent1<-paste0(capmeasure(curmeasure,"long"),"could not be evaluated at ",eusubml," level. ")
        sent2<-""
    }else{
        sent1<-paste0(capmeasure(curmeasure,"long"),deorincrease(1-curtrend),"d "," in ",eusubml,
                      trendstrength(curtrend)," between ",firstyear," and ",lastyear,
                      " by ",percent(abs(1-curtrend),d = 1)," or ",absval(curtrendabs,d=3),". ")
        
        # Figure 1.1 shows the trend of the IEF in EU28 indicating also the range of 
        # values used by the countries contributing between 1990 and 2013. 
        sent2a<-paste0(fig," shows the trend of the ",what," in ",eusubml)
        sent2c<-paste0(" indicating also the range of values used by the countries. ")
        sent2<-paste0(sent2a,sent2c)
    }
    sent3<-paste0(tab," shows ",firstlow(capmeasure(curmeasure)),
                  " for the years ",firstyear," and ",lastyear,
                  " for all Member States and ",eusubml,". ")
    if(fig=="") sent2<-""

    # The IEF decreased in 5 countries and increased in 22 countries. 
    if(decreasen>0 & increasen>0){
        if(curmea%in%c("IEF","FracGASF","FracGASM")){sent4<-paste0("The ",what)}else{sent4<-firstup(what)}
        if(grepl("VS",what))sent4<-what
        sent4<-paste0(sent4," decreased in ",singular(decreasen,"countries",1)," and increased in ",singular(increasen,"countries",1),". ")
        if(stablen>0){sent4<-paste0(sent4,"It was in ",lastyear," at the level of ",firstyear," in ",singular(stablen,"countries",1),". ")}
        if(missing>0){
            if(missing<3){fewmissing<-1}else{fewmissing<-0}
            print(fewmissing)
            sent4<-paste0(sent4,"No data were available for ",
                          if(fewmissing==0){paste0(singular(missing,"countries",1)," (")},
                          laender(missingcountries),
                          if(fewmissing==0){")"},". ")
        }
    }else{
        if(stablen>0){
            sent4<-paste0("The reported ",tolower(what)," in ",lastyear," was at the level of ",firstyear," in ",singular(stablen,"countries",1)," and ")
            addother<-"other "
        }else{
            sent4<-paste0("The reported ",tolower(what))
            addother<-""
        }
        if(decreasen==0 & increasen>0){
            sent4<-paste0(sent4," increased in all reporting ",addother,singular(increasen,"countries",2),". ")}
        if(decreasen>0 & increasen==0){
            sent4<-paste0(sent4," decreased in all reporting ",addother,singular(decreasen,"countries",2),". ")}
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
        }else if(decreasen==1){temp<-"A decrease occurred in "}else{temp<-paste0("Decreases occurred in ")}
        sent5<-paste0(temp,laender(decreasecnt)," with",amean," absolute value of ",absval(decreasemcnt),". ")
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
        }else{temp<-paste0("Increases occurred in ");if(increasencnt==1){temp<-paste0("There was an increase in ")}}
        sent6<-paste0(temp,laender(increasecnt)," with",amean," absolute value of ",absval(increasemcnt),". ")
    }else{sent6<-""}
    #print(paste0(sent1,sent2,sent3,sent4,sent5,sent6))
    
    
    return(paste0(sent1,sent2,sent3,sent4,sent5,sent6))
}