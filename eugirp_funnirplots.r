#print("start eugirp_funnirplots.R")
#print(objects())


generateplotdata<-function(rundata="adem",datasource=c("nir"),subcountries="EUC"){
    if(rundata=="ief") docateg<-"^3"
    if(rundata=="adem") docateg<-"all"
    sharesexist<-0
    if(rundata=="adem") plotparamcheck<-0
    if(rundata=="ief") plotparamcheck<-0
    
    #Exlcudeparty is required because there are two submissions from UK: UK and GB (incl some islands...)
    if(length(datasource)>1){
        plotdata<-allagri
    }else{
        if(rundata=="adem") plotdata<-alldata
        if(rundata=="ief") plotdata<-allagri
    }

    plotdata$datasource<-"nir"
    plotdata<-convert2char(plotdata)
    #Remove years that are not in capinv data
    if("capri"%in%datasource & "nir"%in%datasource)plotdata<-plotdata[,names(capinv)]
    if("capri"%in%datasource & "nir"%in%datasource)plotdata<-rbind(plotdata,capinv)
    if("capri"%in%datasource & !("nir"%in%datasource))plotdata<-capinv
    if("fao"%in%datasource & "nir"%in%datasource)plotdata<-rbind(plotdata,faodata)
    if("fao"%in%datasource & !("nir"%in%datasource))plotdata<-faodata
    #years2remove<-years[!years%in%years2keep]
    #plotdata<-plotdata[,-which(names(plotdata)%in%years2remove)]
    #remove also remove redundant options ABC which is not needed for plots (redundant for CATTLE parents)...
    plotdata$option<-""
    plotdata<-unique(plotdata)
    plotdata<-plotdata[!plotdata$party%in%excludeparty,]
    #View(plotdata)
    
    plotmeas<-unique(subset(plotdata,select=allfields[!allfields %in% c("notation","party",years)]))
    #plotmeas<-unique(plotdata[,allfields[!allfields %in% c("notation","party",years)]])
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
    
    if(length(restrictsector)>1){
        select <- plotmeas$sector_number%in%restrictsector
        plotmeas<-plotmeas[select,]
    }else if(restrictsector!=""){
        # Restrict to sector and sub-sectors
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
    # ADEM plots
    # Keep all uids which are defined in agrigen,agrimix and agridet
    # ... also stored in *RData file, defines appropriate level of detail
    # ... NOTE: for CAPRI this might not be sufficient, as Swine is not further sub-divided.
    # IEF plots 
    # The plots are needed also at the highest disaggregated level, as outliers might be
    # generated or removed if aggregated due to AD share changes.
    if(rundata=="adem"){
        agriuids<-unique(c(as.character(agridet$variableUID),as.character(agrimix$variableUID),as.character(agrigen$variableUID)))
        select<-!grepl("^3",plotmeas$sector_number) | plotmeas$variableUID%in%agriuids
        plotmeas<-plotmeas[select,]
    }
    plotmeas<-plotmeas[order(plotmeas$sector_number,plotmeas$category),]
    if(plotparamcheck==1){plotmeas<-paramcheck}
    plotdata<-plotdata[plotdata$variableUID %in% plotmeas$variableUID,]
    plotdata<-plotdata[order(plotdata$sector_number,plotdata$category),]
    
    # Remove rows where countries report zero (or NA) for the whole time period
    years<-as.character(years2keep)
    plotdata[,years]<-apply(plotdata[,years],2,function(x) as.numeric(x))
    plotdata<-plotdata[apply(plotdata[,years],1,sum,na.rm=TRUE)!=0,]
    years2keep<-as.character(years2keep)
    if(rundata=="adem"){
        acountry<-as.character(country4sub[country4sub[,subcountries]==1,"code2"])
        acountry<-acountry[!acountry%in%eu]
        plotdata<-plotdata[plotdata$party%in%acountry,]
        plotdata<-eu28sums(A = plotdata,aeu = subcountries,years=years2keep)
    }
        
    
    #if(rundata=="adem") plotdata<-eu28sums(A = plotdata)
    adddefault<-0
    if(length(datasource)==1){
        if(rundata=="ief" & datasource=="nir" & runfocus!="range"){
            adddefault<-1
            plotmeas<-loadipccdefaults(plotmeas,1,nrow(plotmeas))
        }}
    
    
    # For multiple plots select only those variables which are available for both
    muds<-length(as.character(unique(plotdata$datasource)))
    selv<-unique(plotdata[,c("variableUID","datasource")])
    selv$ok<-unlist(lapply(1:nrow(selv),function(x) sum(selv$variableUID==selv$variableUID[x])))
    selv$ok<-unlist(lapply(1:nrow(selv),function(x) sum(selv$variableUID==selv$variableUID[x])==muds))
    measOK<-merge(plotmeas,unique(selv[,c("variableUID","ok")]),by="variableUID",all.x=TRUE,sort = FALSE)
    measOK<-measOK[!is.na(measOK$ok),]
    plotmeas<-measOK[measOK$ok,]
    #plotmeas<-plotmeas[measOK$ok,]
    save(plotmeas,plotdata,rundata,sharesexist,adddefault,datasource,file=gsub(".RData",paste0("_plotmeas",rundata,paste(datasource,collapse="-"),".RData"),rdatallem))
    return(list(plotdata,plotmeas,adddefault,sharesexist))
}
loopoverplots<-function(imeas,runfocus="value",eusubm="EUC"){
    if(plotmeas$meastype[imeas]=="Milk") plotdata[plotdata$party=="LU",years]<-NA
    curuid<-plotmeas$variableUID[imeas]
    multisource<-unique(plotdata$datasource[plotdata$variableUID==curuid])
    plotted<-prepareplot(imeas,plotmeas,plotdata,runfocus,rundata,eusubm,plotparamcheck,multisource,adddefault)        
    if(!is.null(plotted[[1]])) plotlegend(curuid,plotdata,runfocus,rundata,eusubm,dsource,plotted)
    #if(!is.null(plotted[[1]])) plotlegend(curuid,plotdata,runfocus,rundata,eusubm,multisource,plotted)
    graphics.off()
}
plotname<-function(dsource,plotsdir,issuedir,imeas,runsect,runmeta,runmeas,runfoc,
                   figdate,plotformat,rundata,cursubm,plotparamcheck){
    
    runmeasure<-as.vector(unlist(runmeas[4]))
    runmeastype<-as.vector(unlist(runmeas[1]))
    runsector<-as.vector(unlist(runsect[1]))
    mtexttitle0<-mtexttit(runsect,runmeta,runmeas)
    #print(mtexttitle0)
    fnammethod<-gsub(" ","",mtexttitle0)
    fnammethod<-gsub("[/: .]","",fnammethod)
    fnammethod<-paste0(fnammethod,runmeastype)
    #print(fnammethod)
    figdir<-paste0(plotsdir,"/",cursubm,"/sec",substr(runsector,start = 1,stop = 1))
    if (! file.exists(paste0(plotsdir,"/",cursubm))){dir.create(file.path(paste0(plotsdir,"/",cursubm)),showWarnings = FALSE )}
    #cat("\nfigdir1: ",figdir)
    if(substr(runsector,start = 1,stop = 1)=="3") {
        if (! file.exists(figdir)){dir.create(file.path(paste0(plotsdir,"/",cursubm)),showWarnings = FALSE )}
        figdir<-paste0(plotsdir,"/",cursubm,"/",runfocus,rundata)
    }
    
    #if(grepl("fao",dsource)) figdir<-paste0(invloc,"/fao/plots")
    if(rundata=="ief") plotformat<-"jpg"
    #if(plotparamcheck==1) plotformat<-"jpg"
    if(plotparamcheck==1) figdir<-paste0(issuedir,"countryoutliers")
    if (! file.exists(figdir)){dir.create(file.path(figdir),showWarnings = FALSE )}
    
    if(plotparamcheck!=1) {
        #runfoc<-paste0(runfoc,formatC(imeas,width=ceiling(log10(nrow(pmeas))),flag="0"))
        #print(paste0(imeas,"/",nrow(pmeas),": ",figname))
    }
    runid<-formatC(imeas,width=ceiling(log10((500))),flag="0")
    figname<-paste0(figdir,"/",fnammethod,"-",dsource,runfoc,imeas,"~",figdate,".",plotformat,collapse=NULL)
    if(rundata=="ief")figname<-figname<-paste0(figdir,"/",fnammethod,"-",dsource,runfoc,rundata,"_",runid,".",plotformat,collapse=NULL)
    if(rundata=="ief")figname<-figname<-paste0(figdir,"/",fnammethod,"-",dsource,runfoc,rundata,".",plotformat,collapse=NULL)
    figname<-figname<-paste0(figdir,"/",fnammethod,"-",dsource,runfoc,".",plotformat,collapse=NULL)
    #print(figname)
    return(figname)
}

iniplot<-function(figname,nplots){
    graphics.off()
    # Define metrics of the plot ####
    # Note: default values have been set for a plot of the size 27.94 cm x 15.24 cm
    # Note: default values have been set for a plot of the size 11 in x 6 in
    pwidth=27.94
    pwidth=16
    pheight=pwidth/1.833
    if(runfocus=="compare"){heightmult<-1.3}else{heightmult<-1}
    pheight=pheight*heightmult
    pconv<-pwidth/27.94
    plotresolution<-plotresolution
    bspace=0.1
    # Plotting barplot, first the bars in shading, then the left-and righthand pattern
    #df.bar<-barplot(eu28fin,yaxp=c(0,8000,2000),col=mycols)
    
    #cat("\nfigname: ",figname)
    if(plotformat=="pdf") pdf(file=figname,width=pwidth,height=pheight)
    if(plotformat=="png") png(file=gsub("pdf","png",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
    if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=pwidth,height=pheight,unit="cm",res=plotresolution)
    cat(gsub(plotsdir,"",figname),": ")
    # Parameters must be set afte defining graphic (?)
    par(mfrow = c(nplots,1))
    if(runfocus=="compare") par(mfrow=c(1,3))
    par(xpd=FALSE)
    
    #outer margin area
    #see http://rgraphics.limnology.wisc.edu/rmargins_sf.php
    if(runfocus=="range"){haslegend<-0}else{haslegend<-1}
    hasfootnote<-1
    hastitle<-1
    xstt=0.15
    if(haslegend==1){xleg=0.7}else{xleg=0.95}
    if (hasfootnote==1){ystt=0.10}else{ystt=0.05}
    if (hastitle==1){yhea=1-0.1/heightmult} else {yhea=1.0}
    
    # omd: outer margin as fraction of device region (in contrast: oma in lines of text)
    paromd<-c(xstt,xleg,ystt,yhea)
    if(runfocus=="compare") paromd<-c(0.05,1,0.15,yhea)
    par(omd=paromd)
    return(list(hastitle,haslegend,hasfootnote,pconv,paromd))
}

getyaxis<-function(teval,tevalpos,tevalneg){
    #if(grepl("ief",rundata)){teval<-curmatrix}
    
    # tevalsmall and tevallarge give the min and max eu-sum, including negative values over the years
    # --
    #View(teval); View(tevalneg); 
    tevalsmall<-unlist(lapply(c(1:length(tevalneg)),function(x) 
        if((!is.na(tevalpos[x]))|(!is.na(tevalneg[x]))){min(tevalpos[x],tevalneg[x],na.rm=T)}else{NA}))
    tevallarge<-unlist(lapply(c(1:length(tevalneg)),function(x) 
        if((!is.na(tevalpos[x]))|(!is.na(tevalneg[x]))){max(tevalpos[x],tevalneg[x],na.rm=T)}else{NA}))
    tdifmax<-max(tevallarge,na.rm=T)
    tdifmin<-min(tevalsmall,na.rm=T)
    
    # Calculate magnitude of ticks from the max abs value
    # ... add 5% margin if the highest value is just at the limit
    tmag<-10^floor(log10(1.05*max(abs(tdifmax),abs(tdifmin),na.rm=TRUE)))/4
    if(rundata=="ief" && runfocus=="trend"){tmag=tmag/2.5}
    #print(paste(tdifmax,tdifmin,tmag))
    
    tmax<-tmag*ceiling(tdifmax/tmag)
    tmin<-tmag*floor(tdifmin/tmag)
    #print(paste(tmin,tmax,tmag))
    if(!grepl("ief",rundata)){tmin<-min(0,tmin)}
    if(is.nan(tmin)){tmin<-0.5*min(teval,na.rm=T)}
    if(is.nan(tmax)){tmax<-1.5*max(teval,na.rm=T)}
    if(tmin==0 & tmax==0){tmin<--0.5;tmax<-0.5}
    if(tmin==tmax){tmin<-0.9*tmin;tmax<-1.1*tmin}
    if(tmag==Inf)stop()
    #print(paste(tmin,tmax,tmag))
    return(list(tmin,tmax,tmag))
}

gettdis<-function(tmin,tmax,tmag){
    # Number of ticks should be between tnlow and tnhig
    tpos<-c(40,20,10,5,2.5,2,1)
    tnlow=5
    tnhig=10
    if(tmag==0){tdis=5}else{
        for (i in tpos){
            tcur<-(tmax-tmin)/(i*tmag)
            if(tcur<=tnhig){tdis<-tcur}
        }}
    return(tdis)    
}

#prepareplot<-function(imeas,plotmeas,plotdata,runfocus="value",rundata="adem",eusubm,plotparamcheck=1,dsource,multisource,adddefault=0){
prepareplot<-function(imeas,plotmeas,plotdata,runfocus="value",rundata="adem",eusubm,plotparamcheck=1,multisource,adddefault=0){
    
    #print("prepareplot")
    curuid<-plotmeas$variableUID[imeas]
    years2keep<-as.character(years2keep)
    #runfoc<-paste0(runfoc,)
    #print(runfocus)
    runid<-formatC(imeas,width=ceiling(log10(nrow(plotmeas))),flag="0")
    figname<-plotname(paste0(unique(plotdata$datasource),collapse=""),plotsdir,issuedir,runid,plotmeas[imeas,sectfields],plotmeas[imeas,metafields],plotmeas[imeas,measfields],
                      runfocus,figdate,plotformat,rundata,cursubm,plotparamcheck=0)
    nplots<-length(unique(plotdata$datasource))
    #par(mfrow = c(1,length(unique(plotdata$datasource))))
    #multisource<-unique(plotdata$datasource)
    multisource<-unique(plotdata$datasource[plotdata$variableUID==curuid])
    cat("\n")
    plotinitialized<-NULL
    plotted<-NULL
    ploteuvals<-NULL
    cntrshars<-NULL
    eu28years<-NULL
    relavs<-NULL
    nmain<-NULL
    nothers<-NULL
    acountry<-as.character(country4sub[country4sub[,eusubm]==1,"code2"])
    acountryminus<-acountry[!acountry%in%eu]
    #eukp<-eunames[,eusubm]
    
    tmin<-NULL
    tmax<-NULL
    tmag<-NULL
    extrayears<-""
    if("fao" %in% multisource) extrayears<-years[apply(plotdata[plotdata$datasource=="fao",years],2,sum,na.rm=TRUE)==0]
    for(dsource in multisource){
        # Determine y-axis for ADEM plots
        isource<-which(dsource==multisource)
        plotdatacur<-plotdata[plotdata$variableUID==curuid&plotdata$datasource==dsource,]
        plotmatr<-as.data.frame(extractuiddata(DF = plotdatacur,uid = curuid,c = acountry,narm = FALSE))
        eu28<-plotmatr[nrow(plotmatr),]
        plotmatr<-plotmatr[1:(nrow(plotmatr)-1),]
        if(sum(plotmatr,na.rm=TRUE)==0){return(list(plotted,ploteuvals,plotinitialized,multisource))}
        if(rundata=="adem"){
            temp<-plotmatr
            temp[temp<=0]<-NA
            eu28pos<-colSums(temp,na.rm=T)
            eu28pos[eu28pos==0]<-NA
            euquant<-as.data.frame(matrix(rep(0,length(years)*3),ncol=length(years),nrow=3))
            row.names(euquant)<-quantfields
            names(euquant)<-years
            
            temp<-plotmatr
            temp[temp>=0]<-NA
            eu28neg<-colSums(temp,na.rm=T)
            eu28neg[eu28neg==0]<-NA
            
            ticksyaxis<-getyaxis(eu28,eu28pos,eu28neg)
            tmin<-min(tmin,ticksyaxis[[1]])
            tmax<-max(tmax,ticksyaxis[[2]])
            tmag<-max(tmag,ticksyaxis[[3]])
            #print(paste(tmin,tmax,tmag))

        }else if(grepl("ief",rundata)){
            eu28pos<-max(plotmatr,na.rm=T)
            eu28neg<-min(plotmatr,na.rm=T)
        }
    }
    
    for(dsource in multisource){
        isource<-which(dsource==multisource)
        #print(paste0("isource=",isource,dsource,"-",paste(multisource,collapse=",")))
        #save(list=objects(),file="temp.rdata")
        plotdatacur<-plotdata[plotdata$variableUID==curuid&plotdata$datasource==dsource,]
        # The first time this runs autocorr column is all NA -- no checks yet made
        autocorr<-acountryminus[acountryminus%in%plotdatacur$party[!is.na(plotdatacur$autocorr)]]
        autocorr<-unlist(lapply(autocorr,function(x) paste(x," (",plotdatacur$autocorr[plotdatacur$party==x],")",sep="")))
        autocorr<-paste(autocorr,collapse=", ")
        if(autocorr!="") autocorr<-paste0("Data correction: ",autocorr)
        serious<-which(acountryminus%in%plotdatacur$party[plotdatacur$correction==0])
        if(dsource=="nir")plotdatacur<-plotdatacur[plotdatacur$correction!=0|plotdatacur$party%in%eu,]
        if(length(serious)==0)serious=""
        if(dsource!="nir") serious<-""
        if(nrow(plotdatacur)>0){
            # Initialize if not yet done
            if(is.null(plotinitialized)) plotinitialized<-iniplot(figname,nplots)
            #        if(dsource=="capri"){
            #print("Attention - test delete conversion DE to CY => REMOVED")
            #            if("DE"%in%unique(plotdatacur$party))plotdatacur[plotdatacur$party=="CY",years]<-plotdatacur[plotdatacur$party=="DE",years]
            #        }
            # Extract data: re-calculate sums for ADEM, use available EU-28 data for IEF ####
            #if(rundata=="adem"){
            #    plotmatr<-as.data.frame(extractuiddata(DF = plotdatacur[!plotdatacur$party %in% eucountries,],uid = curuid,c = countries,narm = FALSE))
            #    eu28<-colSums(extractuiddata(DF = plotdatacur[plotdatacur$party=="EU28",],uid = curuid,c = countries,narm = FALSE),na.rm=TRUE)
            #}else if(grepl("ief",rundata)){
            save(plotdatacur,curuid,acountry,file="temp.rdata")
            plotmatr<-as.data.frame(extractuiddata(DF = plotdatacur,uid = curuid,c = acountry,narm = FALSE))
            #View(plotdatacur,dsource)
            eu28<-plotmatr[nrow(plotmatr),]
            plotmatr<-plotmatr[1:(nrow(plotmatr)-1),]
            #}
            #Make plot when any value is different from zero or NA:
            if(isource==1) cat(runid,"/",nrow(plotmeas),sep = "")
            cat("-",dsource,sep="")
            #View(plotmatr,dsource)
            if(sum(plotmatr,na.rm=TRUE)!=0){
                #return(list(plotted,eu28fin,euquant,finnames,finshares,eu28,eu28pos,eu28neg,sharesexist=0,textorder))
                #plotmade<-makeemplot(curuid,plotdata,plotmatr,"value",rundata,"EU28",plotparamcheck=0,runid,dsource,multisource)
                
                plotcoun<-unique(as.vector(unlist((plotdata$party[plotdata[,"variableUID"]==curuid & !(plotdata$party %in% eu)]))))
                
                #print(curuid)
                #print(plotcoun)
                #View(fdata)
                
                # Negative and positive values: note this has different meaning for ADEM plots vs. IEF plots ####
                #  - ADEM: time series of sum of MS with positive/negative data
                #  - IEF: maximum/minimum values over the time series
                if(rundata=="adem"){
                    #eu28pos and eu28neg now calculated earlier
                    #temp<-plotmatr
                    #temp[temp<=0]<-NA
                    #eu28pos<-colSums(temp,na.rm=T)
                    #eu28pos[eu28pos==0]<-NA
                    euquant<-as.data.frame(matrix(rep(0,length(years)*3),ncol=length(years),nrow=3))
                    row.names(euquant)<-quantfields
                    names(euquant)<-years
                    
                    #temp<-plotmatr
                    #temp[temp>=0]<-NA
                    #eu28neg<-colSums(temp,na.rm=T)
                    #eu28neg[eu28neg==0]<-NA
                    
                    # Relative value in country compared to EU value for all years
                    # This is different from trend-plots!!!
                    rel<-abs(plotmatr[,years2keep])
                    
                    #print("# Relative value in country compared to EU value averaged over all years")
                    relav<-rowMeans(rel,na.rm=TRUE)
                    relav[is.nan(relav)]<-NA
                    
                    relavx<-relav[!is.na(relav)]
                    relavx<-relavx[!relavx==0]
                    
                    #print("plotmatr1")
                    save(rel,eu,acountry,plotmatr,file="rel.rdata")
                    plotmatr$party<-acountry[!acountry%in%eu]
                    rel$party<-acountry[!acountry%in%eu]   #xavi20180126
                    #print("plotmatr2")
                    topn<-min(10,length(relavx))
                    topno<-max(0,length(relavx)-topn)
                    
                    #print("# Select the top countries with highest mean value over all years")
                    #xavi20180126: topneu28<-row.names(as.matrix(head(relavx[order(relavx,decreasing=T,na.last=T)],topn)))
                    #xavi20180126: topother<-row.names(as.matrix(tail(relavx[order(relavx,decreasing=T,na.last=T)],topno)))
                    #xavi20180126   #selecting top countries with higuest values for the last year
                    #xavi20180129: topneu28_1<-row.names(as.matrix(head(relavx[order(relavx,decreasing=T,na.last=T)],topn)))
                    #xavi20180129: topother_1<-row.names(as.matrix(tail(relavx[order(relavx,decreasing=T,na.last=T)],topno)))
                    
                    kk <- rel[, c(ncol(rel)-1, ncol(rel))]
                    kk1 <- kk[order(kk[,1], decreasing = TRUE),]
                    kk2 <- kk1[!is.na(kk1[,1]),]
                    topneu28<-row.names(head(kk2,topn))
                    topneu28_1 <- apply(plotmatr[years2keep], 1, sum, na.rm = TRUE)
                    topneu28_1 <- names(topneu28_1)[topneu28_1 != 0]
                    if (length(topneu28)<10){
                      topneu28 <- union(topneu28, topneu28_1)
                      topneu28 <- head(topneu28, 10)
                    } 
                    #kk: topother<-setdiff(row.names(kk2), topneu28)
                    topother<-setdiff(topneu28_1, topneu28)
                      
                    #yearWdata <- apply(rel[-c(ncol(rel))], 2, sum, na.rm = TRUE)       #xavi20180126
                    #maxYrwData <- as.character(max(as.numeric(names(yearWdata[yearWdata!=0]))))    #xavi20180126
                    #topneu28_2<-row.names(as.matrix(head(relavx[order(as.vector(na.omit(rel[[maxYrwData]])),decreasing=T,na.last=T)],topn))) #xavi20180126
                    #topneu28<-as.vector(na.omit(union(topneu28_2, topneu28_1)[1:10]))
                    #topother<-row.names(as.matrix(tail(relavx[order(as.vector(na.omit(rel[[maxYrwData]])),decreasing=T,na.last=T)],topno))) #xavi20180126
                    #topother<-as.vector(union(topother, topother_1))
                    #topother<-as.vector(union(topother, topneu28_1))
                    #topother<-as.vector(union(topother, topneu28_2))
                    #topother<-as.vector(setdiff(topother, topneu28))

                    #xavi20180129: eu28main<-plotmatr[row.names(plotmatr) %in% topneu28,]
                    #xavi20180126: eu28main<-eu28main[order(rowSums(eu28main[,years2keep],na.rm=TRUE),decreasing=FALSE),]
                    #kk: eu28main<-plotmatr[row.names(plotmatr) %in% topneu28, c(ncol(plotmatr)-1, ncol(plotmatr))]
                    eu28main<-plotmatr[row.names(plotmatr) %in% topneu28,]
                    eu28main<-eu28main[order(eu28main[lastyear], decreasing = FALSE), c(years2keep,"party")]
                    
                    Other<-as.data.frame(t(colSums(plotmatr[row.names(plotmatr) %in% topother,years2keep],na.rm=TRUE)))
                    Other$party<-"Other"
                    topnnames<-eu28main$party

                    if(length(relavx)>length(topneu28)){eu28fin<-rbind(eu28main,Other)}else{eu28fin<-eu28main}
                    finnames<-eu28fin$party
                    eu28fin<-as.matrix(eu28fin[,years2keep])
                    eu28fin[is.na(eu28fin)]<-0
                    
                    #xavi20180129: finshares<-rowMeans(eu28fin,na.rm=T)/mean(as.matrix(eu28))*100
                    #finshares<-eu28fin[,years[length(years)]]/as.matrix(eu28[years[length(years)]])*100
                    #print("# General determination of the last available year")
                    lastyear<-max(which(apply(eu28fin,2,sum,na.rm=TRUE)!=0))
                    finshares<-eu28fin[,years[lastyear]]/as.numeric(eu28[years[lastyear]])*100
                    ###Necessary addition as long as capri only has values up to 2010 
                    #print("Remove capri manipulation for calculation of country shares when there are values for final year")
                    #if(dsource=="capri"){
                    #    finshares<-apply(eu28fin,1,function(x) tail(na.omit(x),1)) / apply(eu28,1, function(x) tail(na.omit(x),1)) * 100
                        #stop()
                    #}
                    
                    finshares[is.na(finshares)]<-0
                    runfocus<-"value"
                    #if(isource==2)stop("funnirplots 283")
                }else if(grepl("ief",rundata)){
                    eu28pos<-max(plotmatr,na.rm=T)
                    eu28neg<-min(plotmatr,na.rm=T)
                    if(trendoutlmethod==2){
                        plotquant<-t(plotmatr)
                        euquant<-as.data.frame(t(Reduce(cbind,lapply(c(1:nrow(plotquant)),function(x) selquantiles(as.matrix(plotquant[x,]))[2:4]))))
                        names(euquant)<-quantfields
                        euquant<-as.data.frame(t(euquant))
                        names(euquant)<-years
                        euquant1<-euquant
                        euquant[1,]<-euquant[2,]+(1+bxplf)*(euquant[1,]-euquant[2,])
                        euquant[3,]<-euquant[2,]+(1+bxplf)*(euquant[3,]-euquant[2,])
                        
                    }
                    if(trendoutlmethod==3){
                        plotquant<-t(plotmatr)
                        euquant<-as.data.frame(t(Reduce(cbind,lapply(c(1:nrow(plotquant)),function(x) selmeansd(as.matrix(plotquant[x,]))))))
                        euquant<-as.data.frame(t(euquant))
                        names(euquant)<-years
                        euquant1<-euquant
                        euquant[2,]<-euquant1[1,]
                        euquant[1,]<-euquant1[1,]-bxplf*euquant1[2,]
                        euquant[3,]<-euquant1[1,]+bxplf*euquant1[2,]
                    }
                    #Box-whisker method: 0.953 times the difference to the mean from 25 and 75 percentiles
                    #                    to determine the upper and lower whisker
                    if(sum(eu28,na.rm=TRUE)==0){ 
                        eu28<-apply(plotmatr,2,mean,na.rm=T)
                        eukp<-paste0(eukp,"*")
                    }else{
                    }
                    dividebycol<-function(vec,val){
                        newvec<-vec/unlist(val)
                        return(newvec)
                    }
                    #rel<-Reduce(rbind,apply(plotmatr,2,"/",eu28))
                    rel<-Reduce(cbind,lapply(c(1:length(eu28)),function(x) dividebycol(plotmatr[,x],eu28[x])))
                    #Use absolute relative deviation as sorting criterium
                    #Store the maximum absolute relative deviation
                    relabs<-abs(1-rel)
                    #relav<-rowMeans(relabs,na.rm=T)
                    relav<-apply(relabs,1,mean,na.rm=T)
                    relav[is.infinite(relav)]<-NA
                    relav[is.nan(relav)]<-NA
                    # If there is no EU-value go on with relative value
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
                    plotmatr$party<-acountry[!acountry%in%eu]
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
                    
                    if(isource==1){
                        ticksyaxis<-getyaxis(eu28fin,eu28pos,eu28neg)
                        tmin<-ticksyaxis[[1]]
                        tmax<-ticksyaxis[[2]]
                        tmag<-ticksyaxis[[3]]
                    }
                    #stop()
                    
                }
                #print(adddefault)
                if(adddefault==1){
                    defaults<-plotmeas[imeas,ipccfields]
                }else{
                    defaults<-NULL
                }
                #print(eu28fin)
                #xavi20180129: if(!(sum(eu28fin,na.rm=TRUE)==0)) {
                if(!(sum(eu28fin,na.rm=TRUE)==0)) {
                    #eugirp_funnirplots.r
                    #return(list(tmin,tmax))
                    #plotted<-plotnow(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus,rundata,dsource,multisource)
                    if("fao" %in% multisource){
                        eu28fin[,extrayears]<-0
                        eu28pos[extrayears]<-0
                        eu28neg[extrayears]<-0
                        eu28[extrayears]<-0
                    }
                  #xavi20180214: plotted<-plotnow(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus,rundata,dsource,multisource,tmin,tmax,tmag,defaults,serious)
                  if ( tmin > 0.01){
                    plotted<-plotnow(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus,rundata,dsource,multisource,tmin=floor(tmin-(tmin*0.1)),tmax=ceiling(tmax*1.1),tmag,defaults,serious)
                  }else{
                    plotted<-plotnow(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus,rundata,dsource,multisource,tmin,tmax,tmag,defaults,serious)
                  }
                }
                #print(finshares)
                tmp<-as.data.frame(finnames)
                tmp$val<-finshares
                tmp[,years2keep]<-eu28fin
                names(tmp)<-c("party",isource,paste0(years2keep,".",isource))
                #print(tmp)
                if(isource==1){
                    autocorrs<-autocorr
                    seriouss<-serious
                    cntrshars<-tmp
                    eu28years<-list(eu28)
                    relavs<-list(relav)
                    nmain<-list(topn)
                    nothers<-list(topno)
                    #cntryears<-eu28fin
                }else{
                    autocorrs[[isource]]<-autocorr
                    seriouss[[isource]]<-serious
                    cntrshars<-merge(cntrshars,tmp,by="party",all=TRUE)
                    eu28years[[isource]]<-eu28
                    relavs[[isource]]<-relav
                    nmain[[isource]]<-topn
                    nothers[[isource]]<-topno
                    #cntryears<-list(cntryears,eu28fin)
                    
                }
                ploteuvals<-list(cntrshars,eu28years,nmain,nothers,relavs,autocorrs,seriouss)
            }else{
                cat(" nothing to plot\n")
            }
        }
    }
    return(list(plotted,ploteuvals,plotinitialized,multisource))
}

#plotnow<-function(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus="value",rundata="adem",dsource,multisource){
#plotnow<-function(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus="value",rundata="adem",dsource,multisource,defaults,serious){
plotnow<-function(curuid,eu28fin,euquant,finnames,eu28,eu28pos,eu28neg,runfocus="value",rundata="adem",dsource,multisource,tmin,tmax,tmag,defaults,serious){
    # This function creates the various plots that can be used for the NIR,
    # in particular: value-plots, trend-plots, and country-plots.
    #capinv graphics.off()
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #### GENERAL PLOT INFORMATION #################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #source("c:/adrian/models/capri/dndc/results/20110722/nitrogen/figures/plotdefaults.r")
    
    #pwidth set in function iniplot ... needs to be retrieved from current device
    pwidth=16
    pconv<-pwidth/27.94
    # runcateg to be used for the plot title
    #rungas<-unique(pmeas$gas)
    
    runmatrix<-eu28fin
    curmatrix<-eu28fin
    ncountries<-nrow(eu28fin)
    plotyears<-names(eu28)
    
    if (runfocus=="value"){runfoc<-"1VAL"}
    if (runfocus=="trend"){runfoc<-"2TRD"}
    if (runfocus=="countries"){runfoc<-"3CNT"}
    
    if(length(curuid)>1){stop("More than one UID selected for graph!!")}
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # IDENTIFYING ATTRIBUTES ######################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    schraffierung<-1
    formatfinnames<-finnames
    attributes<-as.matrix(read.table("eugirp_attributes.txt",header=T,row.names=1,check.names=F))
    mydens<-(as.numeric(attributes[formatfinnames,"dens"]))
    mycols<-(attributes[formatfinnames,"color"])
    mycoll<-(attributes[formatfinnames,"coll"])
    myang1<-(as.numeric(attributes[formatfinnames,"ang1"]))
    myang2<-(as.numeric(attributes[formatfinnames,"ang2"]))
    
    myticks<-(as.numeric(attributes[formatfinnames,"symbol"]))
    mytcol1<-(attributes[formatfinnames,"col1t"])
    mytcol2<-(attributes[formatfinnames,"col2t"])
    mytcol3<-mytcol2
    mytcol3[mytcol1=="black" & mytcol2=="black"]<-"white"
    mytcol3[mytcol1!="black" | mytcol2!="black"]<-"black"
    
    
    #For ief plots
    mydensall<-(as.numeric(attributes[finnames,"dens"]))
    mycolsall<-(attributes[finnames,"color"])
    mycollall<-(attributes[finnames,"coll"])
    myang1all<-(as.numeric(attributes[finnames,"ang1"]))
    myang2all<-(as.numeric(attributes[finnames,"ang2"]))
    
    myticksall<-(as.numeric(attributes[finnames,"symbol"]))
    mytcol1all<-(attributes[finnames,"col1t"])
    mytcol2all<-(attributes[finnames,"col2t"])
    mytcol3all<-mytcol2
    mytcol3all[mytcol1=="black" & mytcol2=="black"]<-"white"
    mytcol3all[mytcol1!="black" | mytcol2!="black"]<-"black"
    #mydens<-as.vector(as.numeric(attributes[rownames(eu28fin),"dens"]))
    
    largeticks<-2*pconv
    smallticks<-largeticks/2
    
    #inner margins
    marbot<-3.3 * pconv
    marlef<-1 *pconv
    martop<-1.5 *pconv
    marrig<-3 *pconv
    marrig<-0.1 *pconv
    mars<-c(marbot,marlef,martop,marrig)
    
    par(mar=c(marbot,marlef,martop,marrig)
        #    ,lab=c(10,1,10)
    )
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # TICKS #######################################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    tdis<-gettdis(tmin,tmax,tmag)

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #print("Start plotting")
    if(!grepl("ief",rundata)){
        eu28finpos<-(eu28fin>0)*eu28fin
        eu28finneg<-(eu28fin<0)*eu28fin
        #print(paste(tmin,tmax,tdis))
        df.bar<-barplot(eu28finpos,ylim=c(tmin,tmax),yaxp=c(tmin,tmax,tdis),
                        col=mycols,xpd=F,axes=F,las=2,cex=1*pconv,cex.axis=1*pconv,
                        cex.names=1*pconv,xaxt="n",   #Do not plot x-axis values (years)
                        tck=0.1)
        #df.bar<-barplot(eu28finpos,ylim=c(tmin,tmax),yaxp=c(tmin,tmax,tdis),col=mycols,xpd=F,axes=F,las=2)
        barplot(eu28finneg,add=T,col=mycols,axes=F,axisnames=F,las=2)
        ypos<-0
        xpos<-0
        
        # Add pattern to barplot
        barplot(eu28finpos,add=T,dens=mydens,angle=45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finneg,add=T,dens=mydens,angle=45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finpos,add=T,dens=mydens,angle=-45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finneg,add=T,dens=mydens,angle=-45,col=mycoll,axes=F,axisnames=F,las=2)
        
        par(new=T)
        eu28[eu28==0]<-NA
        lines(x=df.bar,y=eu28,lty=1,lwd=2)
        #see point types here: http://www.endmemo.com/program/R/pchsymbols.php
        points(x=df.bar,y=eu28,pch=21,bg="black",col="red",cex=1.5*pconv,lwd=2,cex.axis=1*pconv)
    }else{
        if(runfocus=="countries"){
            countrymatrix<-t(curmatrix)
            cmformin<-curmatrix
            cmformax<-curmatrix
            cmformin[is.na(cmformin)]<-999
            cmformax[is.na(cmformax)]<--999
            cmmin<-apply(cmformin,1,min)
            cmmax<-apply(cmformin,1,max)
            cmmean<-rowMeans(cmformin,na.rm=T)
            cmsum<-rowSums(cmformin,na.rm=T)
            
            cm<-cbind(cmmin,cmmax,cmmean,cmsum,curmatrix)
            #Required a data frame
            cmt<-as.data.frame(t(cm))
            curcountries<-names(cmt)
            #Convert row names into column
            cmt<-cbind(rownames(cmt),cmt)
            #names(cmt)[1]<-"variable"
            
            #positions need to be saved during plotting
            # see http://stackoverflow.com/questions/15331472/x-axis-does-not-match-barplot
            # and http://www.inside-r.org/packages/cran/TeachingDemos/docs/updateusr
            barpos<-barplot(cmmax-cmmin,offset=cmmin,
                            space=0.5,
                            col=mycolsall,
                            ylim=c(tmin,tmax),
                            xaxt="n",
                            xpd=TRUE,
                            xlab="x")
            barpos<-barplot(add=T,cmmax-cmmin,offset=cmmin,
                            space=0.5,
                            dens=mydensall,col=mycollall,angle=45,
                            ylim=c(tmin,tmax),xaxt="n",xpd=TRUE,xlab="x")
            barpos<-barplot(add=T,cmmax-cmmin,offset=cmmin,
                            space=0.5,
                            dens=mydensall,col=mycollall,angle=-45,
                            ylim=c(tmin,tmax),xaxt="n",xpd=TRUE,xlab="x")
            
            axis(1,at=barpos,adj=1, tick=TRUE, 
                 line=NA, lty=1, lwd=1,
                 labels=rep("",length(curcountries))
            )
            text(x=barpos,
                 y=par()$usr[3]-0.04*(par()$usr[4]-par()$usr[3]),
                 labels=curcountries, 
                 srt=90, adj=1, 
                 xpd=TRUE)
        }
        else if(runfocus=="range"){
            df.bar<-plot(matrix(eu28),xlim=c(1,length(years)),ylim=c(tmin,tmax),xaxt="n",xpd=TRUE,xlab="",ylab="",yaxt="n",
                         pch=21,bg="black",col="red",cex=1.5*pconv,lwd=2)
            #polygon(c(1:length(years),length(years):1),c(apply(eu28fin,2,min),apply(eu28fin,2,max)),density = 10,col="grey")
            #plot(matrix(eu28),ylim=c(tmin,tmax),xaxt="n",yaxt="n",pch=21,bg="black",col="red",cex=1.5*pconv,lwd=2,new=FALSE)
            text(c(1:length(years)),as.numeric(matrix(eu28))+(tmax-tmin)/20,
                 labels=format(as.numeric(matrix(eu28)),digits = 3),
                 cex=0.75*pconv,srt=90,adj=0)
            
            lines<-eu28fin
            if(nrow(lines)>1){
                lines[lines==0]<-NA
                linesok<-which(apply(lines,2,sum,na.rm=T)==0)
                if(length(linesok)>0){
                    linemin<-rep(NA,nyears)
                    linemax<-rep(NA,nyears)
                    linemin[-linesok]<-apply(lines[,-linesok],2,min,na.rm=TRUE)
                    linemax[-linesok]<-apply(lines[,-linesok],2,max,na.rm=TRUE)
                }else{
                    linemin<-apply(lines,2,min,na.rm=TRUE)
                    linemax<-apply(lines,2,max,na.rm=TRUE)
                }
                lines(c(1:length(years)),linemax,ylim=c(tmin,tmax),col="grey30",new=FALSE)
                lines(c(1:length(years)),linemin,ylim=c(tmin,tmax),col="grey30",new=FALSE)
            }
            #points(matrix(eu28),pch=21,bg="black",col="red",cex=5.5,lwd=5)
            ypos<- 0.1
            xpos<--0.5
            df.bar<-1+yearsnum-yearsnum[1]
        }
        else if(rundata=="ief"){
            euquant[euquant==0]<-NA
            
            plot(0, xlim=c(0, length(years)+1), 
                 ylim=c(tmin,tmax),
                 axes=F, xlab="", ylab="", type="n")
            ypos<--0.5
            xpos<--0.5
            lines(c(1:length(years)),euquant[1,],ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=3,new=FALSE)
            lines(c(1:length(years)),euquant[2,],ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=1,new=FALSE)
            lines(c(1:length(years)),euquant[3,],ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=3,new=FALSE)
            
            #lines(c(length(years):(length(years)+0.2)),rep(euquant[1,length(years)],2),ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=3,new=FALSE)
            #lines(c(length(years):(length(years)+0.2)),rep(euquant[2,length(years)],2),ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=1,new=FALSE)
            #lines(c(length(years):(length(years)+0.2)),rep(euquant[3,length(years)],2),ylim=c(tmin,tmax),col="grey20",lwd=1.5,lty=3,new=FALSE)
            
            if(trendoutlmethod==2){medtext<-"med"}
            if(trendoutlmethod==3){medtext<-"mean"}
            text(length(years)+0.3,euquant[1,length(years)],labels="low",col="grey20",cex=0.75*pconv,srt=0,adj=0)
            text(length(years)+0.3,euquant[2,length(years)],labels=medtext,col="grey20",cex=0.75*pconv,srt=0,adj=0)
            text(length(years)+0.3,euquant[3,length(years)],labels="upp",col="grey20",cex=0.75*pconv,srt=0,adj=0)
            
            
            
            for (i in c(1: min(topn+1,nrow(eu28fin)))){
                #print(paste(i,eu28fin[i,1],myticks[i],mytcol1[i],mytcol2[i],mytcol3[i]))
                points(eu28fin[i,],pch=myticks[i],col="black"   ,bg=mytcol1[i],lwd=1*pconv,cex=1.5*pconv)
                points(eu28fin[i,],pch=myticks[i],col=mytcol3[i],bg=mytcol2[i],lwd=1*pconv,cex=0.75*pconv)
            }
            points(matrix(eu28),pch=21,bg="black",col="red",cex=1.5*pconv,lwd=2)
            #points(matrix(eu28),pch=21,bg="black",col="red",cex=5.5,lwd=5)
            df.bar<-1+yearsnum-yearsnum[1]
        }
    }
    if(runfocus!="countries"){
        # So far only points and box. Add here all additional lines, ticks, and axes texts
        
        # Add ticks without labels
        #stop()
        axis(1,at=df.bar,labels=FALSE,line=0,tck=-0.01,lwd=largeticks,las=2,cex.axis=1*pconv)
        if(rundata!="ief"){
            axis(1,at=c(0,df.bar,ceiling(max(df.bar))),labels=FALSE,line=0,tck=0,lwd=largeticks,las=2,cex.axis=1*pconv)
        }else{
            axis(1,at=c(-0.5,df.bar,ceiling(max(df.bar))),labels=FALSE,line=0,tck=0,lwd=largeticks,las=2,cex.axis=1*pconv)
        }
        
        # Add labels (shift higher) (see http://stackoverflow.com/questions/28606339/distance-between-axis-tick-mark-and-corresponding-labels-in-r)
        axis(1,at=df.bar,labels=years2keep,line=-0.5,tck=-0.01,lwd=0,las=2,cex.axis=1*pconv)
        
        # Add small grey ticks on the y-axis
        axis(2,at=seq(tmin,tmax,(tmax-tmin)/tdis/5),pos=c(ypos,0),lwd=smallticks,labels=F,col.ticks="grey")
        
        # Calculate distances for large ticks and draw them
        tnum<-(tmax-tmin)/tdis
        tnum<-tdis
        tdis<-(tmax-tmin)/tnum
        if(tnum<1){ndig<-abs(floor(log10(abs(tmax))))}else{ndig<-0}
        tseq<-round(seq(tmin,tmax,tdis),ndig)
        # ..... if the number of ticks is inconsistent with the rounding, calculate without rounding
        #print(paste("tseq:",tseq[1],"-",tseq[2]))
        #print(paste("tminetc",tmin,tmax,tdis))
        if(sum(tseq[2],-tseq[1],na.rm=TRUE)*tdis!=(tmax-tmin)) tseq<-seq(tmin,tmax,tdis)
        axis(2,at=tseq,pos=c(ypos,0),lwd=largeticks,las=1,labels=FALSE)
        # Now add lables (separately, to position precisley)
        axis(2,at=tseq,pos=c(ypos+0.5,0),lwd=0,las=1,cex.axis=1*pconv)
        
        # Draw line so 'connect' the y-axis with the x-axis
        #if(rundata!="ief") 
        if(tmin/tmax<0) lines(x=c(-1,length(years)*1.2),y=c(0,0),lwd=largeticks*1.2)
        abline(v=ypos,lwd=largeticks)
        #stop()
    }
    
    if(length(multisource)>1){
        if(dsource=="capri") sourctitlong<-"CAPRI"
        if(dsource=="fao") sourctitlong<-"FAO"
        if(dsource=="nir") sourctitlong<-"National GHG inventories"
        title(sourctitlong,cex.main=0.5,adj=0.95)
    }
    
    if(!is.null(defaults)){
        ipccmin<-(defaults[1])
        ipccmax<-(defaults[2])
        if(ipccmin!="" & ipccmax!=""){
            ipcctxt<-paste0("IPCC 2006 default: ",as.character(unlist(defaults[3])))
            ipccmin<-suppressWarnings(as.numeric(as.character(unlist(defaults[1]))))
            ipccmax<-suppressWarnings(as.numeric(as.character(unlist(defaults[2]))))
            if(!is.na(ipccmin)){
                lines(c(-1,0.5),rep(ipccmin,2),pch=21,col="blue",cex=1.5*pconv,lwd=2)
                lines(c(-1,0.5),rep(ipccmax,2),pch=21,col="blue",lwd=2)
                if((ipccmax-ipccmin)/tmag>0.01) arrows(x0=0,y0=ipccmin,x1=0,y1=ipccmax,code=3,length=0.08,angle=25,col="blue",lwd=1)
            }
        }else{ipcctxt<-""}
    }else{ipcctxt<-""}
    
    return(list(tmin,tmax,ipcctxt))
}

plotlegend<-function(curuid,fdata,runfocus,rundata="adem",eusubm="EUC",dsource,plotted){
    #print(paste("plotlegend. ",par("usr")))
    uabs<-par("usr")[3]
    oabs<-par("usr")[4]
    xstt<-par()$omd[1]
    xleg<-par()$omd[2]
    ystt<-par()$omd[3]
    yhea<-par()$omd[4]
    
    #First in prepareplots return list
    tmin<-plotted[[1]][[1]]
    tmax<-plotted[[1]][[2]]
    ipcctxt<-plotted[[1]][[3]]
    
    #Second in prepareplots return list
    allfinshares<-plotted[[2]][[1]]
    
    #Third in prepareplots return list
    hastitle<-plotted[[3]][1]
    haslegend<-plotted[[3]][2]
    hasfootnote<-plotted[[3]][3]
    
    #Fourth in prepareplots return list
    multisource<-plotted[[4]]
    pconv<-as.vector(unlist(plotted[[3]][4]))
    
    
    
    ###Here the shares are all 0 in CAPRI
    #print(allfinshares)
    selcols<-grepl("[1-9]",names(allfinshares)) & nchar(names(allfinshares))>4
    selcols[1]<-TRUE
    eu28fins<-allfinshares[,selcols]
    selcols<-names(allfinshares)%in%c("party",c(1:9))
    allfinshares<-allfinshares[,selcols]
    ###Here the shares are all 0 in CAPRI
    #print(allfinshares)
    finnames<-eu28fins$party
    ncountries<-length(finnames)
    nplots<-sum(match(gsub("[1-9]","x",names(allfinshares)),"x"),na.rm=TRUE)
    #Separate 'other' out to prepare for ordering
    #eu28finso<-eu28fins[eu28fins$party=="Other",]
    #eu28fins<-eu28fins[eu28fins$party!="Other",]
    #allfinshareso<-allfinshares[allfinshares$party=="Other",]
    #allfinshares<-allfinshares[allfinshares$party!="Other",]
    if("Other"%in%finnames){hasother<-"Other"}else{hasother<-""}
    
    # Get number of countries 'in' and 'out'
    topns<-unlist(plotted[[2]][[3]])
    topnos<-unlist(plotted[[2]][[4]])
    
    # Retrieve ordering info
    acountry<-as.character(country4sub[country4sub[,eusubm]==1,"code2"])
    acountry<-acountry[!acountry%in%eu]
    #eukp<-eunames[,eusubm]
    
    relavs<-plotted[[2]][[5]]
    relavs2<-plotted[[2]][[1]] #xavi20180126 This is just to keep the correct order
    relav<-as.data.frame(matrix(rep(NA,length(acountry)*(nplots+1)),ncol=(nplots+1),nrow=length(acountry)))
    names(relav)<-c("party",c(1:(nplots)))
    relav$party<-acountry
    for(iplot in c(1:nplots)){
        relav[,iplot+1]<-relavs[[iplot]]
    }
    #xavi20180126: relav<-merge(as.data.frame(finnames),relav,by.x="finnames",by.y="party",sort = FALSE)
    relav<-merge(as.data.frame(finnames),relavs2[,c(1,ncol(relavs2))],by.x="finnames",by.y="party",sort = FALSE)
    autocorr<-plotted[[2]][[6]]
    serious<-plotted[[2]][[7]]
    
    #Determine order 
    relav$order<-apply(as.matrix(relav[,-1]),1,mean,na.rm=TRUE)
    #xavi20180126: relav<-relav[order(relav$order,decreasing=FALSE),]
    
    if(hasother=="Other"){
        relav<-relav[order(relavs2[relavs2$party!="Other",ncol(relavs2)],decreasing=FALSE),] #xavi20180126
        #xavi20180125: finnames<-as.data.frame(c(as.character(relav$finnames),hasother))
        finnames<-as.data.frame(c(hasother, as.character(relav$finnames)))
    }else{
        relav<-relav[order(relavs2[,ncol(relavs2)],decreasing=FALSE),] #xavi20180126 
        finnames<-as.data.frame(c(as.character(relav$finnames)))
    }
    names(finnames)<-"party"
    
    #eu28fins<-rbind(eu28fins[order(shareorder,decreasing = FALSE),],eu28finso)
    #allfinshares<-rbind(allfinshares[order(shareorder,decreasing = FALSE),],allfinshareso)
    eu28fins<-merge(finnames,eu28fins,by="party",sort=FALSE)
    allfinshares<-merge(finnames,allfinshares,by="party",sort=FALSE)
    finnames<-as.vector(finnames$party)
    alleu28<-plotted[[2]][[2]]
    
    #Third in prepareplots return list
    hastitle<-plotted[[3]][1]
    haslegend<-plotted[[3]][2]
    hasfootnote<-plotted[[3]][3]
    
    #Fourth in prepareplots return list
    multisource<-plotted[[4]]
    pconv<-as.vector(unlist(plotted[[3]][4]))
    
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # LABLES Y-AXIS #####################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #print("Plot Labels y-axis")
    #mytext1<-"CH"[4] ~~ "enteric fermentation"
    ##mytext2<-substitute(A ~ " - " ~ B ~ "Tg N" ~~ yr^{-1},list(A=colnames(b)[j],B=emis[j]))
    #mytext2<-"[ Tg CH"[4] ~~ yr^{-1} ~"]"
    #if(rundata=="ief"){mytext2<-"[ Tg CH"[4] ~~ head^{-1} ~~ yr^{-1} ~"]"}
    #source("text_elements.txt")
    mars<-par()$mar
    marbot<-mars[1]
    marlef<-mars[2]
    martop<-mars[3]
    marrig<-mars[4]
    pmeas<-unique(fdata[fdata$variableUID==curuid,-which(names(fdata)%in%c("party",years,"datasource","autocorr","correction"))])
    runsect<-data.frame(lapply(unique(pmeas[,sectfields]),as.character))
    runmeta<-data.frame(lapply(unique(pmeas[,metafields]),as.character))
    runmeas<-data.frame(lapply(unique(pmeas[,measfields]),as.character))
    runmeastype<-as.vector(unlist(runmeas[1]))
    runmeasure<-as.vector(unlist(runmeas[4]))
    mtexttitle0<-mtexttit(runsect,runmeta,runmeas)
    #print(paste0("\nmtexttitle0",mtexttitle0," runsect=",runsect," runmeta=",runmeta," runmeas=",runmeas))
    curunit<-unique(pmeas$unit)
    source("eugirp_attributes.r")   
    
    par(omd=c(0,xstt,ystt,yhea))
    par(mar=c(marbot,marlef,martop,marrig))
    par(fig=c(0,1,0,1),new=T)
    #box("figure",col="red",lwd=5)
    #box("plot",col="blue",lwd=4)
    #box("inner",col="green",lty="dotted",lwd=3)
    #box("outer",col="black",lwd=2)
    
    plot(0, xlim=c(0, 1), 
         ylim=c(uabs+0.04*oabs,0.96*oabs), 
         #ylim=c(0,1),
         axes=F,xlab="",ylab="",type="n")
    
    
    textunit<-unitforplot(curunit)
    if(rundata=="ief" & runfocus=="trend"){
        textunit<-"Interannual" ~" growth"
    }            
    #text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=min(1.5,1.5*30/nchar(textpar)),textpar,las=3,srt=90,font=2)
    writetext<-multilines(runmeasure,60)
    
    if(length(writetext)>1){newcex<-0.9*pconv}else{newcex<-min(1.5*pconv,1.0*40/nchar(writetext[1]))}
    text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=newcex,writetext[1],las=3,srt=90,font=1)
    if(length(writetext)>1){
        text(0.22,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=newcex,writetext[2],las=3,srt=90,font=1)
    }
    text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3*pconv,textunit,las=3,srt=90,font=1)
    #print(paste(curunit,textunit))
    #text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3,curunit,las=3,srt=90,font=2)
    
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # IDENTIFYING ATTRIBUTES ######################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    schraffierung<-1
    formatfinnames<-as.vector(finnames)
    #print(formatfinnames)
    attributes<-as.matrix(read.table("eugirp_attributes.txt",header=T,row.names=1,check.names=F))
    mydens<-(as.numeric(attributes[formatfinnames,"dens"]))
    mycols<-(attributes[formatfinnames,"color"])
    mycoll<-(attributes[formatfinnames,"coll"])
    myang1<-(as.numeric(attributes[formatfinnames,"ang1"]))
    myang2<-(as.numeric(attributes[formatfinnames,"ang2"]))
    
    myticks<-(as.numeric(attributes[formatfinnames,"symbol"]))
    mytcol1<-(attributes[formatfinnames,"col1t"])
    mytcol2<-(attributes[formatfinnames,"col2t"])
    mytcol3<-mytcol2
    mytcol3[mytcol1=="black" & mytcol2=="black"]<-"white"
    mytcol3[mytcol1!="black" | mytcol2!="black"]<-"black"
    
    
    #For ief plots
    mydensall<-(as.numeric(attributes[finnames,"dens"]))
    mycolsall<-(attributes[finnames,"color"])
    mycollall<-(attributes[finnames,"coll"])
    myang1all<-(as.numeric(attributes[finnames,"ang1"]))
    myang2all<-(as.numeric(attributes[finnames,"ang2"]))
    
    myticksall<-(as.numeric(attributes[finnames,"symbol"]))
    mytcol1all<-(attributes[finnames,"col1t"])
    mytcol2all<-(attributes[finnames,"col2t"])
    mytcol3all<-mytcol2
    mytcol3all[mytcol1=="black" & mytcol2=="black"]<-"white"
    mytcol3all[mytcol1!="black" | mytcol2!="black"]<-"black"
    #mydens<-as.vector(as.numeric(attributes[rownames(eu28fin),"dens"]))
    #stop("Stop after y-axis")
    # box("figure",col="red",lwd=5)
    # box("plot",col="blue",lwd=4)
    # box("inner",col="green",lty="dotted",lwd=3)
    # box("outer",col="black",lwd=2)
    
    for(iplot in (c(1:nplots))){
        
        finshares<-as.matrix(subset(allfinshares,select=which(names(allfinshares)%in%c(iplot))))
        eu28<-alleu28[[iplot]]
        eu28fin<-as.matrix(subset(eu28fins,select=which(names(eu28fins)%in%c(paste0(years,".",iplot)))))
        topn<-unlist(topns[[iplot]])
        topno<-unlist(topnos[[iplot]])
        #print(iplot)
        sourctitshort<-toupper(multisource[iplot])
        
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        # LEGEND ###########################################################
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        #print("Plot legend")
        # Define plotting region for legend
        #vomd<-c(omdx2,1-omdx1,omdy1,omdy2)  # horizontal min,max - vertical min,max
        if(haslegend==1){
            par(omd=c(xleg,1,ystt,yhea))
            #par(mar=c(1.0,1.0,5.0,1.)) #bot,lef,top,rig
            par(mar=c(0,marlef,0,marrig))
            
            par(fig=c(0,1,0,1),new=T)
            par(lty=1,col="black")
            plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
            
            #if(rundata=="ief" & runfocus!="countries"){recdist=0.9}else{recdist=1.2}
            
            # Font size and digits
            legcex<-1.1*pconv
            maxfins<-max(abs(finshares),na.rm=TRUE)
            minfins<-min(abs(finshares),na.rm=TRUE)
            if(maxfins>1000) legcex<-1.0*pconv
            if(rundata=="ief") legcex<-0.9*pconv
            nzeros<-max(0,3-ceiling(log10(maxfins)))
            if(is.infinite(nzeros))nzeros<-1
            addzeros<-paste(rep("0",nzeros),collapse="")
            if(rundata=="ief" & runfocus!="countries"){iefconv<-0.9/1.1}else{iefconv<-1}
            
            # recside defines the space for columns in the legend (box, country-name, numbers)
            # recdist defines the space between columns in the legend
            # xplace: values in adem-plots (right adjustment!)
            # avshare: valuse in ief-plots (right adjustment!)
            recside<-0.2*iefconv
            recdist=0.05*iefconv
            xplace<-(1+iplot)*(recside+recdist)+recside
            avshare<-1.5*recside+recdist + (iplot*recside)
            avshare<-xplace
            
            
            # Vertical spacing
            # - minlow: bottom of country list, defined by region of current element and bottom margin
            # - maxhig: 
            minlow<-marbot*par("cxy")[2]
            maxhig<-0.9
            eu28ispos<-0.87
            eu28ispos<-0.92
            nplotspos<-0.87
            if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){eu28ispos<-0.91}
            omitcountry<-0
            
            #Include all finshares (also those with zero)
            nomit<-length(finshares[finshares==0])
            nomit<-0
            
            if(ncountries>0){
                for (i in c(1:ncountries)){
                    
                    # Conditions always true - change if finshares==0 should be omitted
                    #if(!is.na(finshares[i])){
                    #if(finshares[i]!=0 | finshares[i]==0){
                    #print(paste(i,ncountries,finnames[i],finshares[i],sep="-"))
                    mytextavc <- finnames[i]
                    if(finnames[i]!="Other")mytextavc <- country4sub[country4sub$code2==finnames[i],"code3"]
                    mytexteu<-eukp
                    mytexteua<-paste0(eukp)
                    if(is.nan(finshares[i])) finshares[i]<-NA
                    if(!is.na(finshares[i])){
                        checkdig<-round(finshares[i],nzeros)
                        if(round(checkdig,0)==checkdig & addzeros!=""){checkdig<-paste0(checkdig,".",addzeros)}
                        mytextavv <- paste0(checkdig,"%",sep="")
                        if(runfocus=="range")mytextavv<-checkdig
                        mytextran <-""
                    }else{
                        mytextavv<-finshares[i]
                        #shareAD1<-""
                        #shareAD2<-""
                        mytextran <-""
                    }      
                    if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
                        #Give average (min-max)
                        
                        myround<-max(1,-floor(log10(max(eu28fin,na.rm=TRUE)))+2)
                        mytexteub<-paste0(round(mean(t(eu28),na.rm=T),myround))
                        mytexteuc<-paste0("(",
                                          round(min(eu28,na.rm=T),myround),"-",
                                          round(max(eu28,na.rm=T),myround),")")
                        mytextavc<-finnames[i]
                        mytextavv<-mean(eu28fin[i,],na.rm=T)
                        if(is.nan(mytextavv)){
                            mytextavv<-" - "
                        }else{
                            mytextavv<-paste0(round(mean(eu28fin[i,],na.rm=T),myround))
                        }
                        if(sum(eu28fin[i,],na.rm=TRUE)>0) {
                            mytextran<-paste0("(",round(min(eu28fin[i,],na.rm=T),myround),"-",
                                              round(max(eu28fin[i,],na.rm=T),myround),")")
                        }else{
                            mytextran<=""
                        }
                        
                    }
                    if (sharesexist){
                        #if(!is.na(act_pctcountry)) 
                        shareAD1 <- paste0(round(act_pctcountry[i],0),"%")
                        #if(!is.na(act_pcteu)) 
                        shareAD2 <- paste0("(",round(act_pcteu[i],0),"%/",round(ief_pcteu[i],0),"%)")
                        shareADeu <- paste0("(",round(act_pctcouneu,0),"%)")
                    }
                    
                    # Define vertical spacing for countries between minlow and maxhig
                    hig=minlow+(i+nomit-omitcountry+0)*(maxhig-minlow)/(ncountries+1)
                    mid=minlow+(i+nomit-omitcountry-0.5)*(maxhig-minlow)/(ncountries+1)
                    low=minlow+(i+nomit-omitcountry-1)*(maxhig-minlow)/(ncountries+1)
                    
                    if(rundata=="adem" | runfocus=="countries"){
                        #print(paste(i,low,mid,hig))
                        rect(0,low,recside,hig,col=mycols[i]) 
                        if (schraffierung == 1){
                            rect(0,low,recside,hig,col=mycoll[i],dens=mydens[i],angle=myang1[i]) 
                            rect(0,low,recside,hig,col=mycoll[i],dens=mydens[i],angle=myang2[i]) 
                            rect(0,low,recside,hig) 
                        }
                    }else{
                        #print(paste(i,low,mid,hig,mytcol1[i],mytcol2[i],mytcol3[i]))
                        #if(finshares[i]!=0){
                        points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol1[i],col="black",lwd=1*pconv,cex=1.5*pconv)
                        points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol2[i],col=mytcol3[i],lwd=1*pconv,cex=0.75*pconv)
                        #}
                    }
                    #chartr substitutes dots with space
                    if (sharesexist) {
                        text(avshare+0.03,eu28ispos,shareADeu,cex=legcex-0.1*pconv,adj=0,font=2)
                        if(iplot==1)text(recside+recdist,mid,mytextavc,cex=legcex-0.25*pconv,adj=0,family="Courier")
                        text(2.2*recside*recdist,mid,mytextavv,cex=legcex-0.25*pconv,adj=1)
                        text(avshare+0.11,mid+0.000,shareAD1,cex=legcex-0.25*pconv,adj=1)
                        text(avshare+0.12,mid-0.000,shareAD2,cex=legcex-0.25*pconv,adj=0)
                    }else{
                        if(rundata=="ief"){
                            if(iplot==1) text(recside*0.75+recdist,mid,mytextavc,cex=legcex,adj=0)
                            text(avshare+0.00,mid,mytextavv,cex=legcex,adj=1)
                            if(nplots==1)text(avshare+0.40,mid,mytextran,cex=legcex,adj=1)
                        }else{
                            # Right adjustment: 
                            if(iplot==1) text(recside+recdist,mid,mytextavc,cex=legcex,adj=0)
                            text(xplace,mid,mytextavv,cex=legcex,adj=1)
                            text(xplace+recdist,mid,mytextran,cex=legcex,adj=1)
                        }
                    }
                    #}
                    #else{
                    #    omitcountry<-omitcountry+1        
                    #}
                    #}
                }
            }
            
            #if (rundata!="ief" ) text(0,1.00,"Average share of country",cex=legcex,adj=0,font=2)
            if (rundata!="ief" ) {
                if(nplots==1) text(0,1.00,paste0("Share in year t-2 (",years[length(years)],")"),cex=legcex,adj=0,font=2)
                if(nplots>1) {
                    text(0,1.00,paste0("Share in year "),cex=legcex,adj=0,font=2)
                    lastyear<-max(which(apply(eu28fin,2,sum,na.rm=TRUE)>0))
                    text(xplace,1.00,paste0(years[lastyear]," (",toupper(multisource[iplot]),") "),cex=legcex-0.25,adj=1,font=1)
                }
            }
            if (rundata=="ief" && runfocus=="value") {
                text(0,1.00,paste0("Average ",runmeastype," of country "),cex=legcex,adj=0,font=2)
                if(nplots==1) text(0.60,1.00,paste0("(min-max)"),cex=legcex,adj=0,font=2)
            }
            if (rundata=="ief" && runfocus=="countries") text(0,1.00,"Range of values over the period",cex=legcex,adj=0,font=2)
            
            if (rundata=="ief" && runfocus=="trend") text(0,1.00,"Average trend of country",cex=legcex,adj=0,font=2)
            if (runmeastype=="EM" && runfocus=="trend") text(0,0.96,"- Contribution AD to country trend",cex=legcex-0.3,adj=0,font=2)
            if (runmeastype=="EM" && runfocus=="trend") text(0,0.93,"- Contribution AD/EF to EU trend",cex=legcex-0.3,adj=0,font=2)
            
            #if (rundata=="ief" && runfocus=="value") text(0,0.95,"% from EU28+IS average",cex=legcex,adj=0,font=3)
            #if (rundata=="ief" && runfocus=="trend") text(0,0.95,"interannual change > 3%",cex=legcex,adj=0,font=2)
            
            mid=minlow+(ncountries+1-0.5)*(maxhig-minlow)/(ncountries+1)
            #print(paste(i,low,mid,hig))
            
            distance<-par()$cin[1]/par()$fin[1]*0.8
            
            #text(recside*recdist,mid+distance,mytexteua,cex=legcex,adj=0,font=2)
            if(iplot==1) {
                points(x=recside/2,y=eu28ispos,pch=21,bg="black",col="red",cex=1.5*pconv,lwd=2)
                text(recside+recdist,eu28ispos,mytexteua,cex=legcex,adj=0,font=2)
            }
            if(nplots>1) text(xplace,nplotspos,sourctitshort,cex=legcex*0.8,adj=1,font=2)
            if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
                #text(recside*recdist,mid,mytexteub,cex=legcex,adj=1,font=2)
                #text(recside*recdist,mid,mytexteuc,cex=legcex,adj=1,font=2)
                text(avshare+0.00,eu28ispos,mytexteub,cex=legcex,adj=1,font=2)
                if(nplots==1)text(avshare+0.40,eu28ispos,mytexteuc,cex=legcex,adj=1,font=2)
            }
            
            #text(avshare,mid,"100%",cex=legcex,adj=1)
            
        }
    }
    
    if(haslegend==1){
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        # Box with explanation country-selection ####
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
        for(iplot in c(1:nplots)){
            if(iplot==1) {
                topntext<-topns[iplot]
                topnotext<-topnos[iplot]
            }
            if(topns[iplot]!=topns[1]){
                if(iplot==2) topntext<-paste(topntext," (",toupper(multisource[iplot-1]),")",sep="")
                topntext<-paste(topntext,"/",topns[iplot]," (",toupper(multisource[iplot]),")",sep="")
            }
            if(topnos[iplot]!=topnos[1]){
                if(iplot==2) topnotext<-paste(topnotext," (",toupper(multisource[iplot-1]),")",sep="")
                topnotext<-paste(topnotext,"/",topnos[iplot]," (",toupper(multisource[iplot]),")",sep="")
            }
        }
        
        if(rundata=="adem"){
            textorderadem1<-paste0("Countries are sorted by their contribution to the sum of the ",eukp," value of the last year. ")
            if(topno>0) {
                textorderadem2<-paste0("The top ",topntext," countries are displayed. ")
                textorderadem3<-paste0("The other ",topnotext," reporting countries with data are lumped to 'other'.")
            }else{
                textorderadem2<-paste0("The ",topntext," reporting countries are displayed. ")
                textorderadem3<-paste0("")
            }
            textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
        }else if(grepl("ief",rundata)){
            if(runfocus%in%c("value","range")){
                textiefval1<-paste0("The ",eukp," value is obtained from a weighted average of country-values. ")
                textiefval2<-"The relative distance from the MS/EU28 value is calculated for each year (e.g. 10% smaller). "
                textiefval3<-"Countries are sorted by average absolute relative distance calculated over the whole time period. "
            }
            if(topno>0){
                textiefval4<-paste0("The top ",topntext," countries are displayed. ")
                textiefval5<-paste0("The other ",topnotext," countries with data are averaged to 'other'.")
            }else{
                textiefval4<-paste0("The ",topntext," reporting countries are displayed. ")
                textiefval5<-paste0("")
            }
            textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
        }
        
        if(length(textorder)>0){
            curmax=46
            curcex=0.65
            text2write<-multilines(textorder,curmax)
            if(length(text2write)>6){
                curmax=58
                curcex<-0.6
                text2write<-multilines(textorder,curmax)
            }
            
            par(omd=c(xleg,1,ystt/2,ystt+0.1))
            #par(mar=c(1.0,1.0,5.0,1.)) #bot,lef,top,rig
            par(mar=c(0,marlef,0,marrig))
            par(fig=c(0,1,0,1),new=T)
            par(lty=1,col="black")
            
            plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
            rect(0.0,0.0,1,1)
            
            for(i in c(1:length(text2write))){
                text(0.03,0.95-0.2*curcex*i,text2write[i],adj=0,cex=curcex*pconv)    
                
            }
        }
    }
    #   box("figure",col="red",lwd=5)
    #   box("plot",col="blue",lwd=4)
    #   box("inner",col="green",lty="dotted",lwd=3)
    #   box("outer",col="black",lwd=2)
    
    #myunit<-expression("Unit: Tg N " ~~ yr^{-1})
    #mtext(myunit,1,line=0,outer=F,adj=1,cex=1.6)
    #stop("Stop after legend")
    
    #stop("Stop after title")
    # 
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # X-AXIS TITLE #####################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #print("Plot x-axis title")
    
    mtextxaxis<-"Year"
    
    par(omd=c(xstt,xleg,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    text(0.5,1-1.1*par("cin")[2],adj=0.5,mtextxaxis,cex=1.2*pconv,font=1)
    
    
    par(omd=c(0,xleg,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    #text(0,par("cin")[2],adj=0,mfooter1,cex=0.8)
    #text(0,0,adj=0,mfooter2,cex=0.8)
    #print(mtexttitle0)
    if(grepl("Other Fossil Fuels", mtexttitle0, ignore.case = TRUE)) mtexttitle0<-gsub("Other Fossil Fuels", "Other Fuels", mtexttitle0, ignore.case = TRUE)
    if(grepl("Other Fossil Fuel Other Fuels", mtexttitle0, ignore.case = TRUE)) mtexttitle0<-gsub("Other Fossil Fuel Other Fuels", "Other Fuels", mtexttitle0, ignore.case = TRUE)
    if(grepl("no classification", mtexttitle0, ignore.case = TRUE)) mtexttitle0<-gsub("no classification", "", mtexttitle0, ignore.case = TRUE)
    plottitle(mtexttitle0,plotted,multisource)
    
    #stop("Stop after x-axis")
    
    #  box("figure",col="red",lwd=5)
    #  box("plot",col="blue",lwd=4)
    #  box("inner",col="green",lty="dotted",lwd=3)
    #  box("outer",col="black",lwd=2)
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #  FOOTNOTE #########################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #print("Plot footnote")
    par(omd=c(0,1,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    
    foottextleft<-paste0("EU-GIRP.v",eugirp.version," (",eugirp.fullname,") (c) EC-JRC/AL ",eugirp.web)
    foottextrigt<-paste0(figdate," - UID: ",curuid, ". Submission from ",cursubm)
    
    autocorrtext<-autocorr
    if(length(serious)!=0){serioustext<-paste0("Data not plotted (serious outlier): ",paste(countriesnoeu[serious],collapse=", "))
    }else{serioustext<-NULL}
    if(length(autocorrtext)!=0) serioustext<-paste(autocorrtext,serioustext)
    if(grepl("fao|capri",paste(multisource,collapse="")))serioustext<-NULL
    if(runfocus%in%c("range"))serioustext<-NULL
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    if(rundata!="adem")if(length(serioustext)!=0)text(0.005,0.4+0.2*(nchar(ipcctxt)>1),adj=0,serioustext,cex=0.5*pconv,font=1,col="red")
    text(0.005,0.4,adj=0,ipcctxt,cex=0.5*pconv,font=1,col="blue")
    text(0.005,0.1,adj=0,foottextleft,cex=0.5*pconv,font=1)
    text(0.995,0.1,adj=1,foottextrigt,cex=0.5*pconv,font=1)
    mtext(ipcctxt,side=1,at=c(0.012,2),cex=0.35,outer=TRUE,col="blue")
    
    #   graphics.off()
    
}

plottitle<-function(mtexttitle0="x",plotted,multisource){
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #  TITLE ############################################################
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    #print("Plot title")
    if (rundata=="adem" && runfocus=="value") {mtexttitle<-paste0("Trend in the ",eukp)}
    if (rundata=="adem" && runfocus=="trend") {mtexttitle<-paste0("Annual changes in the ",eukp)}
    if (grepl("ief",rundata) && runfocus=="value") {mtexttitle<-paste0("Range of values in the ",eukp)}
    if (rundata=="ief" && runfocus=="trend") {mtexttitle<-paste0("Range in annual changes across the ",eukp)}
    if (rundata=="ief" && runfocus%in%c("countries","range")) {mtexttitle<-"Range values over time"}
    if(runfocus=="compare") {mtexttitle<-paste0("Comparison of estimates in the ",eukp,": ",paste(toupper(multisource),collapse=" vs. "))}
    
    mtexttitle0<-gsub(" to cropland and grassland","",mtexttitle0)
    mtexttitle0<-gsub("Managed Soils - Agricultural Soils","Agricultural Soils",mtexttitle0)
    mtexttitle0<-gsub("Organic N Fertilizers - N input","N input",mtexttitle0)
    mtexttitle0<-gsub("Inorganic N Fertilizers - N input","N input",mtexttitle0)
    maxnchar<-60
    mvect<-strsplit(mtexttitle0," ")[[1]]
    maxwords<-min(length(mvect),
                  match(TRUE,lapply(1:length(mvect),function(x) sum(nchar(mvect[1:x]))>maxnchar))-1,
                  na.rm=TRUE)
    mtexttitle1<-gsub(",","",toString(mvect[1:maxwords]))
    if(maxwords<length(mvect)){
        #    mtexttitle2<-paste(gsub(",","",toString(mvect[(maxwords+1):length(mvect)])),mtexttitle)
        mtexttitle2<-paste(gsub(",","",toString(mvect[(maxwords+1):length(mvect)])),"- ",mtexttitle)
    }else{
        mtexttitle2<-mtexttitle
    }
    #Larger for comparisonplots
    if(runfocus=="compare"){addconv<-1.5}else{addconv<-1}
    
    # Return from iniplot stored in position [[3]] of plotted!
    # pconv saved as fourth element
    # par("omd") saved as fifth element
    pconv<-as.vector(unlist(plotted[[3]][4]))
    yhea<-as.vector(unlist(plotted[[3]][5]))[4]
    par(omd=c(0,1,yhea,1))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    text(0.5,0.65,adj=0.5,mtexttitle1,cex=1.3*pconv*addconv)
    text(0.5,0.15,adj=0.5,mtexttitle2,cex=1.25*pconv*addconv)
    
    
}

plotcomparison<-function(imeas,plotmeas=plotmeas,plotdata=plotdata,lyear=2013){
    curuid<-plotmeas$variableUID[imeas]
    multisource<-unique(plotdata$datasource[plotdata$variableUID==curuid])
    pmeas<-plotmeas[imeas,c(sectfields,metafields,measfields)]
    
    runid<-formatC(imeas,width=ceiling(log10(nrow(plotmeas))),flag="0")
    figname<-plotname(paste0(multisource,collapse=""),plotsdir,issuedir,runid,
                      plotmeas[imeas,sectfields],plotmeas[imeas,metafields],plotmeas[imeas,measfields],
                      runfocus,curdate(),plotformat,rundata,cursubm,plotparamcheck=0)
    #cat(figname)
    plotinitialized<-iniplot(figname,1)
    #cat("plot initialized")
    # Requied for plottitle only plotted[[3]] (yhea and pconv)
    plotted<-list(curuid,figname,plotinitialized,multisource)
    
    test<-plotdata[plotdata$variableUID==curuid,]
    test$toplot<-apply(test[,as.character(yearsnum[yearsnum<=lyear])],1,mean,na.rm=TRUE)
    
    test<-test[,c("party","toplot","datasource")]
    test<-unique(test)
    test<-dcast(test,party ~ datasource,value.var="toplot")
    otherdatasource<-names(test)[!names(test)%in%c("party","nir")]
    test$reldiff<-(test[,otherdatasource]/test$nir-1)*100
    test$relimpr<-abs((test[,otherdatasource]-test$nir)/test$nir[test$party=="EUC"])*100
    
    test<-test[test$party!="EU28",]
    test<-test[test$party!="EUC",]
    test<-test[test$party!="IC",]
    test<-test[test$party!="IS",]
    #test<-test[test$party!="CY",]
    #test<-test[test$party!="GB",]
    
    test<-test[order(test$relimpr,na.last=FALSE),]
    
    testc<-sapply(test$party,function(x) countries4plot[which(countries2==x)])
    
    datasource<-names(test)[!names(test)=="party"]
    bspace=1.3
    bspace1=c(0,rep(bspace,nrow(test)-1))
    bspace2=c(-1,rep(bspace,nrow(test)-1))
    cols<-c("grey40","grey70","black","grey")
    
    par(mfrow=c(1,3))
    # Number of lines around plot bottom,left,top,right
    par(mar=c(0,4,0,3))
    
    #
    barplot(test[,multisource[1]],horiz=TRUE,space=bspace1,beside=TRUE,col=cols[1],
            axes=T,
            names.arg=sapply(test[,"party"],function(x) countries4plot[which(countries2==x)]),
            las=1,cex.names=0.8)
    barplot(test[,multisource[2]],horiz=TRUE,space=bspace2,beside=TRUE,col=cols[2],add=T)
    
    barplot(test[,"reldiff"],horiz=TRUE,space=bspace2,beside=TRUE,col=cols[4],
            axes=T,
            names.arg=sapply(test[,"party"],function(x) countries4plot[which(countries2==x)]),
            las=1,cex.names=0.8)
    
    barplot(test[,"relimpr"],horiz=TRUE,space=bspace1,beside=TRUE,col=cols[3],
            axes=T,
            names.arg=sapply(test[,"party"],function(x) countries4plot[which(countries2==x)]),
            las=1,cex.names=0.8)
    # Title
    runsect<-data.frame(lapply(unique(pmeas[,sectfields]),as.character))
    runmeta<-data.frame(lapply(unique(pmeas[,metafields]),as.character))
    runmeas<-data.frame(lapply(unique(pmeas[,measfields]),as.character))
    runmeastype<-as.vector(unlist(runmeas[1]))
    runmeasure<-as.vector(unlist(runmeas[4]))
    mtexttitle0<-paste0(mtexttit(runsect,runmeta,runmeas)," - ",runmeasure)
    mtexttitle0<-gsub(" and NMVOC","",mtexttitle0)
    curunit<-unique(pmeas$unit)
    textunit<-unitforplot(curunit)
    
    xstt<-par()$omd[1]
    xleg<-par()$omd[2]
    ystt<-par()$omd[3]
    yhea<-par()$omd[4]
    pconv<-plotinitialized[[4]]

    plottitle(mtexttitle0,plotted,multisource)
    #print(multisource)
    
    abbcex<-1.4*pconv
    tline<-0.25
    ra<-0.005
    rb<-0.338
    rc<-0.663
    rp<-0.01
    recside<-0.12
    #boxxl<-0.001
    #boxyb<-0.90
    #boxxr<-0.11
    #boxyt<-0.05
    #cat("\n",boxxl,boxyb,boxxr,boxyt)
    #rect(boxxl,boxyb,boxxr,boxyt,col="white") 
    rect(ra+rp,0.9-2*tline-recside,ra+rp+recside/2,0.9-2*tline+recside,col=cols[1]) 
    rect(ra+rp,0.9-3*tline-recside,ra+rp+recside/2,0.9-3*tline+recside,col=cols[2]) 
    text(2*ra+rp+recside/2,0.9-2*tline,adj=0,toupper(multisource[1]),cex=abbcex)
    text(2*ra+rp+recside/2,0.9-3*tline,adj=0,toupper(multisource[2]),cex=abbcex)
    
    # Legend
    uabs<-par("usr")[3]
    oabs<-par("usr")[4]
    
    
    par(omd=c(0,1,0,ystt))
    par(mar=c(0,0,2,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    foottextleft<-paste0("EU-GIRP.v",eugirp.version," (",eugirp.fullname,") (c) EC-JRC/AL ",eugirp.web)
    foottextrigt<-paste0(figdate," - UID: ",curuid, ". Submission from ",cursubm)
    text(0.005,0.1,adj=0,foottextleft,cex=1.5*0.5*pconv,font=1)
    text(0.995,0.1,adj=1,foottextrigt,cex=1.5*0.5*pconv,font=1)
    
    abbcex<-1.4*pconv
    tline<-0.28
    ra<-0.005
    rb<-0.338
    rc<-0.663
    rp<-0.03
    recside<-0.05
    
    
    text(ra,0.9,adj=0," a)",cex=abbcex)
    text(ra+rp,0.9,adj=0,paste0("Mean value (",years[1],"-",lyear,")"),cex=abbcex)
    text(ra+rp,0.9-1*tline,adj=0,textunit,cex=abbcex)
    
    #rect(ra+rp,0.9-2*tline-recside,ra+recside,0.9-2*tline+recside,col=cols[1]) 
    #rect(ra+rp,0.9-3*tline-recside,ra+recside,0.9-3*tline+recside,col=cols[2]) 
    #text(2*ra+rp+recside,0.9-2*tline,adj=0,toupper(multisource[1]),cex=abbcex)
    #text(2*ra+rp+recside,0.9-3*tline,adj=0,toupper(multisource[2]),cex=abbcex)
    
    text(rc,0.9,adj=0,"c)",cex=abbcex)
    text(rc+rp,0.9,adj=0,"Importance of difference between ",cex=abbcex)
    text(rc+rp,0.9-1*tline,adj=0,paste0("mean ",toupper(multisource[1])," and ",toupper(multisource[2])," data "),cex=abbcex)
    text(rc+rp,0.9-2*tline,adj=0,paste0("relative to ",eukp," value in ",toupper(multisource[1])," [%]"),cex=abbcex)
    
    text(rb,0.9,adj=0,"b)",cex=abbcex)
    text(rb+rp,0.9,adj=0,"Relative difference between mean ",cex=abbcex)
    text(rb+rp,0.9-1*tline,adj=0,paste0(toupper(multisource[1])," and ",toupper(multisource[2])," data [%]"),cex=abbcex)
    
    
    #box("figure",col="red",lwd=5)
    #box("plot",col="blue",lwd=4)
    #box("inner",col="green",lty="dotted",lwd=3)
    #box("outer",col="black",lwd=2)
    graphics.off()
}
