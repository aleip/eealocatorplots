### Functions that can be used in interactive data crunching ########

## plottime --> Extract (multiple time series) ####
plottime<-function(pr=NULL,sc=NULL,mt=NULL,ct=NULL,source=NULL,DF=allagri,export=FALSE){
    
    ## This functions extracts single or multiple time series and does
    ## - plot into a figure - this can be in the Plots panel or exported (option export=TRUE/FALSE)
    ## - View the timeseries
    ## - Save an excel file with the time series
    ## 
    ## Select party (pr), sector_number (sc), meastype (mt), category (ct) or source (source)
    ## - for the non-selected parameters all available data will be used.
    ## 
    ## Columns that are have only identical values (e.g. same target/party/...) will be removed 
    
    #if(is.null(pr)){return("no country indicated")}
    graphics.off()
    
    sel<-rep(1,nrow(DF))
    if(!is.null(pr))sel<-sel & DF$party%in%pr
    if(!is.null(sc))sel<-sel & grepl(sc,DF$sector_number)
    if(!is.null(source))sel<-sel & grepl(source,DF$source)
    if(!is.null(mt))sel<-sel & DF$meastype%in%mt
    if(!is.null(ct))sel<-sel & DF$category%in%ct
    #if(is.null(mt)){return("no meastype")}
    time_series<-DF[sel,]
    timelabel<-paste0(time_series[1,measure]," - ",time_series[1,sector_number]," ",
                      time_series[1,category]," [",time_series[1,unit],"]")
    timemin<-1*min(time_series[,years, with=FALSE],na.rm=TRUE)
    timemax<-max(time_series[,years, with=FALSE],na.rm=TRUE)
    timemax<-timemax+0.22*(timemax-timemin)
    timelegend<-subset(time_series,select=c("party","sector_number","category"))
    timelegend$leg<-""
    if(length(unique(timelegend$party))>1)timelegend$leg<-paste0(timelegend$party)
    if(length(unique(timelegend$category))>1){
        timelegend$leg<-paste0(timelegend$leg,timelegend$category)
    }
    
    # Check unique cols
    ucols <- unlist(lapply(1:length(names(time_series)), function(x) 
        nrow(unique(time_series[, names(time_series)[x], with=FALSE]))))
    time_series <- time_series[, names(time_series)[ucols>1], with=FALSE]
    
    View(time_series)
    write.xlsx(time_series,file="time_series.xlsx")
    
    okplot<-function(){
        if(nrow(time_series)>1){
            plot(years,time_series[1,years, with=FALSE],
                 ylim=c(timemin,timemax),ylab=timelabel)
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

