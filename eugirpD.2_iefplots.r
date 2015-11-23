plottype<-"iefplots"
doplots<-2
doplotsv<-2
if(plottype=="iefplots"}) docateg<-"^3"
if(plottype=="emiplots"}) docateg<-"all"
sharesexist<-0
if(plottype=="emiplots"}) plotparamcheck<-0

#plotdata$option==0 should not occur ... remove here
alldata$option[alldata$option==0]<-""
alldata<-unique(alldata)

if(plottype=="emiplots"}) plotdata<-alldata
if(plottype=="iefplots"}) plotdata<-allagri
#remove also remove redundant options ABC which is not needed for plots (redundant for CATTLE parents)...
plotdata$option<-""
plotdata<-unique(plotdata)

plotmeas<-unique(subset(plotdata,select=allfields[!allfields %in% c("notation","party",years)]))
if(plottype=="emiplots"}) plotmeas<-plotmeas[plotmeas$meastype %in% meas2sum,]
if(plottype=="iefplots"}) plotmeas<-plotmeas[plotmeas$meastype %in% c(meas2popweight,meas2mcf,meas2clima),]

# Criterion 1: Do not plot without sector_number
plotselect <- plotmeas$sector_number!=""
plotmeas<-plotmeas[plotselect,]

# Criterion 2: Do not plot sector_numbers below  three levels
#      Exceptions: do all plots in Sector 3
#plotselect<-unlist(lapply(c(1:nrow(plotmeas)),function(x) 
#    if(grepl("^3",plotmeas$sector_number[x])){TRUE}else{
#    gcCount(plotmeas$sector_number[x],".")<3}))
#plotmeas<-plotmeas[plotselect,]

if(restrictsector!=""){
    select <- grepl(restrictsector,plotmeas$sector_number)
    plotmeas<-plotmeas[select,]
}
if(restrictcategory!=""){
    select <- grepl(restrictcategory,plotmeas$category)
    plotmeas<-plotmeas[select,]
}
if(plottype=="emiplots"}){
    sectorplots<-read.table("plots_sec1.txt")
    sectorplots<-as.vector(sectorplots$V1)
    select<-!grepl("^1",plotmeas$sector_number) | plotmeas$variableUID%in%sectorplots
    plotmeas<-plotmeas[select,]
    
    sectorplots<-read.table("plots_sec2.txt")
    sectorplots<-as.vector(sectorplots$V1)
    select<-!grepl("^2",plotmeas$sector_number) | plotmeas$variableUID%in%sectorplots
    plotmeas<-plotmeas[select,]
    
    sectorplots<-read.table("plots_sec4.txt")
    sectorplots<-as.vector(sectorplots$V1)
    select<-!grepl("^4",plotmeas$sector_number) | plotmeas$variableUID%in%sectorplots
    plotmeas<-plotmeas[select,]
    
    sectorplots<-read.table("plots_sec5.txt")
    sectorplots<-as.vector(sectorplots$V1)
    select<-!grepl("^5",plotmeas$sector_number) | plotmeas$variableUID%in%sectorplots
    plotmeas<-plotmeas[select,]
}
agriuids<-unique(c(as.character(agridet$variableUID),as.character(agrimix$variableUID),as.character(agrigen$variableUID)))
select<-!grepl("^3",plotmeas$sector_number) | plotmeas$variableUID%in%agriuids
plotmeas<-plotmeas[select,]
plotmeas<-plotmeas[order(plotmeas$sector_number,plotmeas$category),]
if(plotparamcheck==1){plotmeas<-paramcheck}

plotdata<-plotdata[plotdata$variableUID %in% plotmeas$variableUID,]
plotdata<-plotdata[order(plotdata$sector_number,plotdata$category),]
if(plottype=="emiplots"}) plotdata<-eu28sums(A = plotdata)



#Eliminate IS
plotdata<-plotdata[plotdata$party!="IS",]
#for(imeas in c(13)){
#for(imeas in c(1449:nrow(plotmeas))){


#for(imeas in c(28:28)){
#for(imeas in c(77:nrow(plotmeas))){
for(imeas in c(1:nrow(plotmeas))){
    
    #    plotmatr<-unique(plotdata[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries),c("party",years)])
    #    eu28<-unique(plotdata[plotdata[,"variableUID"]==curuid & (plotdata$party %in% eucountries),c("party",years)])
    figname<-""
    curuid<-plotmeas$variableUID[imeas]
    plotcoun<-as.vector(unlist((plotdata$party[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries)])))
    plotmatr<-as.data.frame(extractuiddata(DF = plotdata,uid = curuid,c = countries,narm = FALSE))
    eu28<-plotmatr[nrow(plotmatr),]
    
    eu28pos<-max(plotmatr,na.rm=T)
    eu28neg<-min(plotmatr,na.rm=T)
    
    plotmatr<-plotmatr[1:(nrow(plotmatr)-1),]

    rel<-Reduce(rbind,apply(plotmatr,1,"/",eu28))
    #Use absolute relative deviation as sorting criterium
    #Store the maximum absolute relative deviation
    relabs<-abs(1-rel)
    #relav<-rowMeans(relabs,na.rm=T)
    relav<-apply(relabs,1,mean,na.rm=T)
    relav[is.infinite(relav)]<-NA
    relav[is.nan(relav)]<-NA

    # If there is no EU-value go on with relative value
    if(is.na(sum(relav))){ 
        eu28<-apply(plotmatr,2,mean,na.rm=T)
        relav<-apply(plotmatr,1,mean,na.rm=T)
        eukp<-paste0(eukp,"*")
    }
    #Keep only those values to plot which are not overlaying the boxplot
    #Attention: the endrange might make 'disappear' points that would be expected...
    endrange<-0.1
    lowerend<-apply(plotmatr,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
    upperend<-apply(plotmatr,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
    lowerok<-(plotmatr<lowerend)
    upperok<-(plotmatr>upperend)
    relna<-lowerok+upperok
    relplot=plotmatr*relna
    relplot[relplot==0]<-NA
    
    
    #plotmatr$party<-allcountries[!allcountries%in%eucountries]
    plotmatr$party<-allcountries[!allcountries%in%eucountries]
    plotmatr$relav<-relav
    plotmatr<-plotmatr[!is.na(relav),]
    plotmatr<-plotmatr[order(plotmatr$relav,decreasing=T),]
    #relav<-relav[!is.na(relav)]
    nexist<-length(relav[!is.na(relav)])
    topn<-min(10,nexist)
    topno<-max(0,nexist-topn)
    
    # Select the top countries with highest mean value over all years
    eu28main<-plotmatr[1:topn,c(years,"party")]
    topnnames<-eu28main$party
    if(topno>0){
        Other<-as.data.frame(t(unlist(apply(plotmatr[(topn+1):(topn+topno),years],2,mean))))
        Other$party<-"Other"
        eu28fin<-rbind(eu28main,Other)
    }else{
        eu28fin<-eu28main
    }
    ncountries<-nrow(eu28fin)
    finnames<-eu28fin$party
    eu28fin<-as.matrix(eu28fin[,years])
    
    #finshares<-rowMeans(eu28fin,na.rm=T)/apply(eu28,1,mean)*100
    finshares<-eu28fin[,length(years)]
    finshares[is.na(finshares)]<-0
    
    rundata<-"ief"
    runfocus<-"value"
    if(runfocus=="value"){
        textiefval1<-paste0(eukp," value is obtained from a weighted average of country-values. ")
        textiefval2<-"The relative distance from MS/EU28 value is calculated for each year (e.g. 10% smaller). "
        textiefval3<-"Countries are sorted by average absolute relative distance calculated over the whole time period. "
    }
    if(topno>0){
        textiefval4<-paste0("The top ",topn," countries are displayed. ")
        textiefval5<-paste0("The other ",topno," countries with data are averaged to 'other'.")
    }else{
        textiefval4<-paste0("The ",topn," reporting countries are displayed. ")
        textiefval5<-paste0("")
    }
    textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
    
    
    # runcateg to be used for the plot title
    rungas<-plotmeas$gas[imeas]
    curunit<-plotmeas$unit[imeas]
    curuid<-plotmeas$variableUID[imeas]
    
    runsect<-data.frame(lapply(plotmeas[imeas,sectfields],as.character))
    runmeta<-data.frame(lapply(plotmeas[imeas,metafields],as.character))
    runmeas<-data.frame(lapply(plotmeas[imeas,measfields],as.character))
    
    runmatrix<-eu28fin
    curmatrix<-eu28fin
    
    if(!(sum(eu28fin,na.rm=TRUE)==0)) {
        runid<-formatC(imeas,width=ceiling(log10(nrow(plotmeas))),flag="0")
        plotnow(curuid,plotdata,eu28fin,finnames,finshares,eu28,eu28pos,eu28neg,
                runfocus="value",rundata="ief",eukp,
                plotparamcheck,sharesexist,textorder,runid)
        #source("eugirp_nirplots.r")
        if(plotparamcheck==1){
            fignames[imeas]<-figname
        }
        #nirplotsdone<-nirplots(eu28fin,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }
}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.csv(plotmeas,file=paste0(figdir,"/iefplots~",figdate,".csv",collapse=NULL))
