doplots<-2
doplotsv<-2
if(rundata=="ief") docateg<-"^3"
if(rundata=="adem") docateg<-"all"
sharesexist<-0
if(rundata=="adem") plotparamcheck<-0

#plotdata$option==0 should not occur ... remove here
alldata$option[alldata$option==0]<-""
alldata<-unique(alldata)

if(rundata=="adem") plotdata<-alldata
if(rundata=="ief") plotdata<-allagri
#remove also remove redundant options ABC which is not needed for plots (redundant for CATTLE parents)...
plotdata$option<-""
plotdata<-unique(plotdata)

plotmeas<-unique(subset(plotdata,select=allfields[!allfields %in% c("notation","party",years)]))
if(rundata=="adem") plotmeas<-plotmeas[plotmeas$meastype %in% meas2sum,]
if(rundata=="ief") plotmeas<-plotmeas[plotmeas$meastype %in% c(meas2popweight,meas2mcf,meas2clima),]

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
if(rundata=="adem"){
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
if(rundata=="adem") plotdata<-eu28sums(A = plotdata)

runfunction<-1

save(plotmeas,plotdata,file=gsub(".RData",paste0("_plotmeas",rundata,".RData"),rdatallem))


#    plotmatr<-unique(plotdata[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries),c("party",years)])
#    eu28<-unique(plotdata[plotdata[,"variableUID"]==curuid & (plotdata$party %in% eucountries),c("party",years)])

if(runfunction==1){

    #resti<-plotmeas$imeas[plotmeas$category=="Dairy Cattle" | plotmeas$category=="Non-Dairy Cattle"]
    #for(imeas in resti){   
    #for(imeas in c(113,119,120)){
    #for(imeas in c(358:nrow(plotmeas))){
    for(imeas in c(1:nrow(plotmeas))){
        figname<-""
        selection<-TRUE
        #selection<-grepl("^3",plotmeas$sector_number[imeas])
        selection<-plotmeas$meastype[imeas]%in%c("GEav","Milk","MASS","VSEXC","NRATE","FracGASF","FracGASM","FracLEACH")
        selection<-plotmeas$meastype[imeas]%in%c("VSEXC")
        if(plotmeas$meastype[imeas]=="Milk") plotdata[plotdata$party=="LU",years]<-NA
        if(plotmeas$meastype[imeas]=="VSEXC") plotdata[plotdata$party=="SE",years]<-NA
        #rundata<-"iefs"
        if(selection){
            curuid<-plotmeas$variableUID[imeas]
            #runfoc<-paste0(runfoc,)
            runid<-formatC(imeas,width=ceiling(log10(nrow(plotmeas))),flag="0")
            cat(runid,"/",nrow(plotmeas))
            makeemplot(curuid,plotdata,"value",rundata,"EU28",plotparamcheck=0,runid)
        }
    }
}else{
    
    for(imeas in c(1:nrow(plotmeas))){
        
        plotcoun<-as.vector(unlist((plotdata$party[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eucountries)])))
        plotmatr<-as.data.frame(extractuiddata(DF = plotdata[!plotdata$party %in% eucountries,],uid = curuid,c = countries,narm = FALSE))
        eu28<-colSums(extractuiddata(DF = plotdata[plotdata$party=="EU28",],uid = curuid,c = countries,narm = FALSE),na.rm=TRUE)
        
        temp<-plotmatr
        temp[temp<=0]<-NA
        eu28pos<-colSums(temp,na.rm=T)
        eu28pos[eu28pos==0]<-NA
        
        temp<-plotmatr
        temp[temp>=0]<-NA
        eu28neg<-colSums(temp,na.rm=T)
        eu28neg[eu28neg==0]<-NA
        
        # Relative value in country compared to EU value for all years
        # This is different from trend-plots!!!
        rel<-abs(plotmatr[,years])
        # Relative value in country compared to EU value averaged over all years
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
        eu28main<-eu28main[order(rowSums(eu28main[,years],na.rm=TRUE),decreasing=FALSE),]
        Other<-as.data.frame(t(colSums(plotmatr[row.names(plotmatr) %in% topother,years],na.rm=TRUE)))
        Other$party<-"Other"
        topnnames<-eu28main$party
        
        if(length(relav)>length(topneu28)){eu28fin<-rbind(eu28main,Other)}else{eu28fin<-eu28main}
        ncountries<-nrow(eu28fin)
        finnames<-eu28fin$party
        eu28fin<-as.matrix(eu28fin[,years])
        
        finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28)*100
        finshares<-eu28fin[,years[length(years)]]/eu28[years[length(years)]]*100
        finshares[is.na(finshares)]<-0
        
        textorderadem1<-paste0("Countries are sorted by the average contribution to the sum of ",eukp," value over the the whole time period. ")
        if(topno>0) {
            textorderadem2<-paste0("The top ",topn," countries are displayed. ")
            textorderadem3<-paste0("The other ",topno," reporting countries with data are lumped to 'other'.")
        }else{
            textorderadem2<-paste0("The ",topn," reporting countries are displayed. ")
            textorderadem3<-paste0("")
        }
        textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
        
        rundata<-"adem"
        runfocus<-"value"
        
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
            source("eugirp_nirplots.r")
            #nirplotsdone<-nirplots(eu28fin,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
        }
    }
}
plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
write.csv(plotmeas,file=paste0(plotsdir,"/",rundata,"plots~",figdate,".csv",collapse=NULL))
