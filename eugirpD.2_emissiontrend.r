doplots<-2
doplotsv<-2
docateg<-"all"

plotdata<-alltrend
plotmeas<-unique(subset(plotdata,select=allfields[!allfields %in% c("notation","party",years)]))
plotmeas<-plotmeas[plotmeas$meastype %in% meas2sum,]

# Criterion 1: Do not plot without sector_number
plotselect <- plotmeas$sector_number!=""
plotmeas<-plotmeas[plotselect,]

# Criterion 2: Do not plot sector_numbers 
#      Exceptions: do all plots in Sector 3
plotselect<-unlist(lapply(c(1:nrow(plotmeas)),function(x) 
    if(grepl("^3",plotmeas$sector_number[x])){TRUE}else{
    gcCount(plotmeas$sector_number[x],".")<3}))
plotmeas<-plotmeas[plotselect,]

# Criterion 3: Plot only sector 3
plotselect <- grepl("^3",plotmeas$sector_number)
plotmeas<-plotmeas[plotselect,]


if(restrictsector!=""){
    select <- grepl(restrictsector,plotmeas$sector_number)
    plotmeas<-plotmeas[select,]
}
if(restrictcategory!=""){
    select <- grepl(restrictcategory,plotmeas$category)
    plotmeas<-plotmeas[select,]
}

plotdata<-plotdata[plotdata$variableUID %in% plotmeas$variableUID,]
plotdata<-plotdata[order(plotdata$sector_number,plotdata$category),]
plotmeas<-plotmeas[order(plotmeas$sector_number,plotmeas$category),]

source("eugirpD.2_emissionshares.r")
gases<-c("CH4","CO2","N2O","Aggregate GHGs")
sharemeas<-plotmeas[plotmeas$meastype=="EM" & plotmeas$gas %in% gases,]
sharemeas$IEFuid<-unlist(lapply(c(1:nrow(sharemeas)),function(x)
    selectiefuid("ief",sharemeas,measures2wei,x)))

sharemeas$ADuid<-unlist(lapply(c(1:nrow(sharemeas)),function(x)
    if(sharemeas$IEFuid[x]==0){"no IEF"}else
        if(grepl("^[2-9],",sharemeas$IEFuid[x])){"multiple IEF"}else{
            assignad2par$aduids[assignad2par$variableUID==sharemeas$IEFuid[x]]
        }))

select<-sharemeas$IEFuid==0 | grepl("^[2-9],",sharemeas$IEFuid)
sharemeasnoief<-sharemeas[select,]
sharemeasnoief$ADuid<-unlist(lapply(c(1:nrow(sharemeasnoief)),function(x)
    if(sharemeasnoief$ADuid[x]==0){selectiefuid("ad",sharemeas,measures2wei,x)}else{
        sharemeasnoief$ADuid[x]}))
sharemeas<-sharemeas[!select,]

select<-sharemeas$ADuid=="" | grepl("^[2-9] ",sharemeas$ADuid) | nchar(sharemeas$ADuid)>36
sharemeasnoad<-sharemeas[select,]
sharemeas<-sharemeas[!select,]



#
curmeasu<-"EM"


#for(imeas in c(10:10)){
#for(imeas in c(1438:nrow(plotmeas))){
for(imeas in c(1:nrow(plotmeas))){
    
    #    plotmatr<-unique(plotdata[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries),c("party",years)])
    #    eu28<-unique(plotdata[plotdata[,"variableUID"]==curuid & (plotdata$party %in% eucountries),c("party",years)])
    
    sharesexist<-0
    curuid<-plotmeas$variableUID[imeas]
    plotcoun<-as.vector(unlist((plotdata$party[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries)])))
    plotmatr<-as.data.frame(extractuiddata(DF = plotdata[!plotdata$party %in% eucountries,],uid = curuid,c = countries,narm = FALSE))
    eu28<-colSums(extractuiddata(DF = plotdata[plotdata$party=="EU28",],uid = curuid,c = countries,narm = FALSE),na.rm=TRUE)
    eu28["1990"]<-NA
    
    temp<-plotmatr
    temp[temp<=0]<-NA
    eu28pos<-colSums(temp,na.rm=T)
    eu28pos[eu28pos==0]<-NA
    
    temp<-plotmatr
    temp[temp>=0]<-NA
    eu28neg<-colSums(temp,na.rm=T)
    eu28neg[eu28neg==0]<-NA
    
    # Relative ABSOLUTE value in country compared to EU value for all years
    # This is different from value-plots!!!
    #rel<-(plotmatr[,years])
    rel<-abs(plotmatr[,years])
    
    # Relative ABSOLUTE value in country compared to EU value averaged over all years
    relav<-rowMeans(rel,na.rm=TRUE)
    relav<-relav[!is.na(relav)]
    relav<-relav[!relav==0]
    
    plotmatr$party<-allcountries
    topn<-min(10,length(relav))
    topno<-max(0,length(relav)-topn)
    
    # Select the top countries with highest mean value over all years
    topneu28<-row.names(as.matrix(head(relav[order(relav,decreasing=T,na.last=T)],topn)))
    topother<-row.names(as.matrix(tail(relav[order(relav,decreasing=T,na.last=T)],topno)))
    
    eu28main<-plotmatr[row.names(plotmatr) %in% topneu28,]
    
    #Sort according to contribution to absolute inter-annual changes
    #eu28main<-eu28main[order(rowSums(abs(eu28main[,years]),na.rm=TRUE),decreasing=FALSE),]
    #Sort according to contribution to total inter-annual changes
    eu28main<-eu28main[order(rowSums(eu28main[,years],na.rm=TRUE),decreasing=TRUE),]
    Other<-as.data.frame(t(colSums(plotmatr[row.names(plotmatr) %in% topother,years],na.rm=TRUE)))
    Other$party<-"Other"
    topnnames<-eu28main$party
    
    if(length(relav)>length(topneu28)){eu28fin<-rbind(eu28main,Other)}else{eu28fin<-eu28main}
    ncountries<-nrow(eu28fin)
    finnames<-eu28fin$party
    eu28fin<-as.matrix(eu28fin[,years])
    
    finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28,na.rm=TRUE)*100
    finshares[is.na(finshares)]<-0
    
    if(curuid %in% sharemeas$variableUID){
        sharesexist<-1
        shares<-emissiontrendshares(DF = alldata,
                                    emuid = curuid,
                                    iefuid = sharemeas$IEFuid[sharemeas$variableUID==curuid],
                                    aduid = sharemeas$ADuid[sharemeas$variableUID==curuid],
                                    nyears = length(years),countries = countries )
        #return(as.list(act_pcteu,ief_pcteu,act_pctcountry,ief_pctcountry,act_pctcouneu,ief_pctcouneu))
        act_pcteu<-shares[[1]][match(finnames,curcount)]
        ief_pcteu<-shares[[2]][match(finnames,curcount)]
        
        act_pctcountry<-shares[[3]][match(finnames,curcount)]
        ief_pctcountry<-shares[[4]][match(finnames,curcount)]
        
        act_pctcouneu<-shares[[5]]
        ief_pctcouneu<-shares[[6]]
        if(length(relav)>length(topneu28)){
            nother<-length(finnames)
            act_pcteu[nother]<-sum(shares[[1]][match(curcount[!curcount%in%finnames],curcount)],na.rm=TRUE)
            ief_pcteu[nother]<-sum(shares[[2]][match(curcount[!curcount%in%finnames],curcount)],na.rm=TRUE)
            act_pctcountry[nother]<-mean(shares[[3]][match(curcount[!curcount%in%finnames],curcount)],na.rm=TRUE)
            ief_pctcountry[nother]<-mean(shares[[4]][match(curcount[!curcount%in%finnames],curcount)],na.rm=TRUE)
        }
    }
    
    
    textorderadem1<-paste0("Countries are selected by the magnitude of their (absolute) inter-annual changes over the year ",min(years),"-",max(years),". ")
    if(topno>0) {
        textorderadem2<-paste0("The top ",topn," countries are displayed. ")
        textorderadem3<-paste0("The other ",topno," reporting countries with data are lumped to 'other'.")
    }else{
        textorderadem2<-paste0("The ",topn," reporting countries are displayed. ")
        textorderadem3<-paste0("")
    }
    textorderadem4<-paste0("Countries are sorted by their contribution to total changes over ",min(years),"-",max(years),". ")
    textorder<-paste0(textorderadem1,textorderadem2,textorderadem3,textorderadem4)
    
    rundata<-"adem"
    runfocus<-"trend"
    
    # runcateg to be used for the plot title
    rungas<-plotmeas$gas[imeas]
    curunit<-plotmeas$unit[imeas]
    curuid<-plotmeas$variableUID[imeas]
    runcateg<-paste(plotmeas$sector_number[imeas],plotmeas$category[imeas],sep=" ")
    runmethod<-paste(sapply(metafields,function(x) if(plotmeas[imeas,x]==""){""}else{paste(plotmeas[imeas,x],sep=" ")}),collapse="")
    runpar<-plotmeas$meastype[imeas]
    runmeasure<-plotmeas$measure[imeas]
    runmatrix<-eu28fin
    curmatrix<-eu28fin
    
    if(!(sum(eu28fin,na.rm=TRUE)==0)) {
        source("eugirp_nirplots.r")
        #nirplotsdone<-nirplots(eu28fin,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }
}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.csv(plotmeas,file=paste0(figdate,"/emissiontrends~",figdate,".csv",collapse=NULL))
