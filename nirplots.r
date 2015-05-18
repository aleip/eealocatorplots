

#nirplots<-function(curmatrix,eu28fin,eu28,rundata,runfocus,runcateg,runpar,runfoc){

# This function creates the various plots that can be used for the NIR,
# in particular: value-plots, trend-plots, and country-plots.
graphics.off()

#### GENERAL PLOT INFORMATION
#source("c:/adrian/models/capri/dndc/results/20110722/nitrogen/figures/plotdefaults.r")
xstt=0.15
xleg=0.7
hasfootnote<-1
hastitle<-1
if (hasfootnote==1){ystt=0.10}else{ystt=0.05}
if (hastitle==1){yhea=0.90} else {yhea=1.0}
bspace=0.1
par(xpd=F)
schraffierung<-1

if (runfocus=="value"){runfoc<-"1VAL"}
if (runfocus=="trend"){runfoc<-"2TRD"}
if (runfocus=="countries"){runfoc<-"3CNT"}

runmethod<-gsub(" ","",runmethod)
runmethod<-gsub("/","",runmethod)

runcategmetgas<-paste0(gsub(" ","",runcateg),runmethod,".")
if(rungas!="no gas"){
    runcategmetgas<-paste0(runcategmetgas,gsub(" ","",rungas))
}

#print("Think about text-elements later")
#print(curunit)
# See text expressions 
# http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
if(curunit=="kg/head/yr"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"}
if(curunit=="kg/head/year"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"}
if(curunit=="MJ/head/day"){textunit<-"[ MJ " ~~ head^{-1}  ~~ day^{-1} ~"]"}
#if(curunit=="kg N2O-N/kg N"){textunit<-"[ kg " ~~ N _[2] ~ "O (kg N" ~ ")"^{-1} ~"]"}
if(curunit=="kg N2O-N/kg N"){textunit<-expression("[kg N"[2]*"O (kg N)"^'-1'*']')}
if(curunit=="kg/t dm"){textunit<-expression("[kg (t dm)"^'-1'*']')}
if(curunit=="Gg"){textunit<-"[Gg" ~"]"}
if(curunit=="kg"){textunit<-"[kg" ~"]"}
if(curunit=="kt"){textunit<-"[kt" ~"]"}
if(curunit=="kt CO2 equivalent"){textunit<-"[kt" ~ CO_2 ~ "eq]"}
if(curunit=="1000s"){textunit<-"[1000s" ~"]"}
if(curunit=="%"){textunit<-"[%" ~"]"}
textsou<-runmethod
#   ---> If the category name includes the "source" (eg animal type) - remove
textcat<-gsub(textsou,"",runcateg)
textpar<-gsub(" ","_",measname[measname[,"variableUID"]==runuid,"measure"])
textuid<-as.vector(unique(runmatrix[,"variableUID"]))
if(length(textuid)>1){stop("More than one UID selected for graph!!")}
#textsou<-gsub("Dairy CATT","Dairy Cattle",textsou)

figdate<-format(Sys.time(), "%Y%m%d")

if (! file.exists(figdate)){
    dir.create(file.path(figdate))
#    setwd(file.path(mainDir, figdate))
}
figname<-paste0(figdate,"/",runcategmetgas,"-",textpar,"-",runfoc,"~",figdate,".pdf",collapse=NULL)
#postscript(figname)
pdf(file=figname,width=11,height=6)

#mydens<-as.vector(as.numeric(attributes[rownames(eu28fin),"dens"]))
attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
mydens<-(as.numeric(attributes[finnames,"dens"]))
mycols<-(attributes[finnames,"color"])
mycoll<-(attributes[finnames,"coll"])
myang1<-(as.numeric(attributes[finnames,"ang1"]))
myang2<-(as.numeric(attributes[finnames,"ang2"]))

myticks<-(as.numeric(attributes[finnames,"symbol"]))
mytcol1<-(attributes[finnames,"col1t"])
mytcol2<-(attributes[finnames,"col2t"])
mytcol3<-mytcol2
mytcol3[mytcol1=="black" & mytcol2=="black"]<-"white"
mytcol3[mytcol1!="black" | mytcol2!="black"]<-"black"

# Plotting barplot, first the bars in shading, then the left-and righthand pattern
#df.bar<-barplot(eu28fin,yaxp=c(0,8000,2000),col=mycols)

marbot<-3.3
marlef<-1
martop<-1.5
marrig<-0.1
#outer margin area
#see http://rgraphics.limnology.wisc.edu/rmargins_sf.php
par(omd=c(xstt,xleg,ystt,yhea))
#inner margins
par(mar=c(marbot,marlef,martop,marrig)
    #    ,lab=c(10,1,10)
)

# Ticks

# Getting order of magnitude of ticks
teval<-eu28
tevalpos<-eu28pos
tevalneg<-eu28neg
tevalsmall<-unlist(lapply(c(1:length(tevalneg)),function(x) min(tevalpos[x],tevalneg[x],na.rm=T)))
tevallarge<-unlist(lapply(c(1:length(tevalneg)),function(x) max(tevalpos[x],tevalneg[x],na.rm=T)))
if(rundata=="ief"){teval<-curmatrix}

tpos<-c(20,10,5,2.5,2,1)
tdifmax<-max(tevallarge,na.rm=T)
tdifmin<-min(tevalsmall,na.rm=T)
tdif<-tdifmax-tdifmin
tmag<-10^floor(log10(tdif))/2
if(rundata=="ief" && runfocus=="trend"){tmag=tmag/2.5}
tmin<-tmag*floor(tdifmin/tmag)
if(is.nan(tmin)){tmin<-0.5*min(teval,na.rm=T)}
if(rundata!="ief"){tmin<-min(0,tmin)}
tmax<-tmag*ceiling(tdifmax/tmag)
if(is.nan(tmax)){tmax<-1.5*max(teval,na.rm=T)}
if(tmin==0 & tmax==0){tmin<--0.5;tmax<-0.5}

#if(tmag!=0){
    
    # Number of ticks should be between tnlow and tnhig
    tnlow=5
    tnhig=10
    if(tmag==0){tdis=5}else{
    for (i in tpos){
        tcur<-(tmax-tmin)/(i*tmag)
        if(tcur<=tnhig){tdis<-tcur}
    }}
    
    
    if(rundata!="ief"){
        eu28finpos<-(eu28fin>0)*eu28fin
        eu28finneg<-(eu28fin<0)*eu28fin
        df.bar<-barplot(eu28finpos,ylim=c(tmin,tmax),yaxp=c(tmin,tmax,tdis),col=mycols,xpd=F,axes=F,las=2)
        barplot(eu28finneg,add=T,col=mycols,axes=F,axisnames=F,las=2)
        abline(h=0)
        axis(1,at=years,line=1,lwd=2,las=1,pos=c(0,0))
        axis(2,at=seq(tmin,tmax,(tmax-tmin)/tdis/5),tick=1,line=1,pos=c(0,0),lwd=1,
             labels=F,
             col.ticks="grey")
        axis(2,at=seq(tmin,tmax,(tmax-tmin)/tdis),tick=1,line=1,pos=c(0,0),
             lwd=2,las=1)
        barplot(eu28finpos,add=T,dens=mydens,angle=45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finneg,add=T,dens=mydens,angle=45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finpos,add=T,dens=mydens,angle=-45,col=mycoll,axes=F,axisnames=F,las=2)
        barplot(eu28finneg,add=T,dens=mydens,angle=-45,col=mycoll,axes=F,axisnames=F,las=2)
        
        par(new=T)
        lines(x=df.bar,y=eu28,lty=1,lwd=2)
        #see point types here: http://www.endmemo.com/program/R/pchsymbols.php
        points(x=df.bar,y=eu28,pch=21,bg="black",col="red",cex=1.5,lwd=2)
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
                            col=mycols,
                            ylim=c(tmin,tmax),
                            xaxt="n",
                            xpd=TRUE,
                            xlab="x")
            barpos<-barplot(add=T,cmmax-cmmin,offset=cmmin,
                            space=0.5,
                            dens=mydens,col=mycoll,angle=45,
                            ylim=c(tmin,tmax),xaxt="n",xpd=TRUE,xlab="x")
            barpos<-barplot(add=T,cmmax-cmmin,offset=cmmin,
                            space=0.5,
                            dens=mydens,col=mycoll,angle=-45,
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
        }else{
            
            boxplot(curmatrix,
                    notch=F,
                    range=1.0,
                    whisklty=0,
                    staplelty=0,
                    ylim=c(tmin,tmax),
                    xaxt="n",
                    xpd=TRUE,
                    labels=F,
                    xlab="")
            
            axis(1,at=1+years-years[1],adj=1, tick=TRUE, 
                 line=NA, lty=1, lwd=1,
                 labels=rep("",length(years))
            )
            text(x=1+years-years[1],
                 y=par()$usr[3]-0.04*(par()$usr[4]-par()$usr[3]),
                 labels=years, 
                 srt=90, adj=1, 
                 xpd=TRUE)
            
            for (i in c(1: min(topn+1,nrow(eu28fin)))){
                #print(paste(i,eu28fin[i,1],myticks[i],mytcol1[i],mytcol2[i],mytcol3[i]))
                points(eu28fin[i,],pch=myticks[i],col="black"   ,bg=mytcol1[i],lwd=1,cex=1.5)
                points(eu28fin[i,],pch=myticks[i],col=mytcol3[i],bg=mytcol2[i],lwd=1,cex=0.75)
            }
            
            points(eu28,pch=21,bg="black",col="red",cex=1.5,lwd=2)
        }
    }
    
    #print(par("usr"))
    uabs<-par("usr")[3]
    oabs<-par("usr")[4]
    #stop("Stop after plot")
    # box("figure",col="red",lwd=5)
    # box("plot",col="blue",lwd=4)
    # box("inner",col="green",lty="dotted",lwd=3)
    # box("outer",col="black",lwd=2)
    
    #####################################################################
    #                 LABLES Y-AXIS
    #####################################################################
    
    #mytext1<-"CH"[4] ~~ "enteric fermentation"
    ##mytext2<-substitute(A ~ " - " ~ B ~ "Tg N" ~~ yr^{-1},list(A=colnames(b)[j],B=emis[j]))
    #mytext2<-"[ Tg CH"[4] ~~ yr^{-1} ~"]"
    #if(rundata=="ief"){mytext2<-"[ Tg CH"[4] ~~ head^{-1} ~~ yr^{-1} ~"]"}
    #source("text_elements.txt")
    par(omd=c(0,xstt,ystt,yhea))
    par(mar=c(marbot,marlef,martop,marrig))
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), 
         #ylim=c(0,1), 
         ylim=c(uabs+0.04*oabs,0.96*oabs), 
         axes=F, xlab="", ylab="", type="n")
    
    if(rundata=="ief" & runfocus=="trend"){
        #textpar<-paste0("Interannual change: ",runpar)
        #textunit<-paste0("Interannual growth")
        textunit<-"Interannual" ~" growth"
        
    }            
    
    text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=min(1.5,1.5*30/nchar(textpar)),textpar,las=3,srt=90,font=2)
    text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3,textunit,las=3,srt=90,font=2)
    #print(paste(curunit,textunit))
    #text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3,curunit,las=3,srt=90,font=2)
    
    #stop("Stop after y-axis")
    # box("figure",col="red",lwd=5)
    # box("plot",col="blue",lwd=4)
    # box("inner",col="green",lty="dotted",lwd=3)
    # box("outer",col="black",lwd=2)
    
    #####################################################################
    #                         LEGEND
    #####################################################################
    # Define plotting region for legend
    #vomd<-c(omdx2,1-omdx1,omdy1,omdy2)  # horizontal min,max - vertical min,max
    par(omd=c(xleg,1,ystt,yhea))
    #par(mar=c(1.0,1.0,5.0,1.)) #bot,lef,top,rig
    par(mar=c(0,marlef,0,marrig))
    
    par(fig=c(0,1,0,1),new=T)
    par(lty=1,col="black")
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    recside<-0.2
    if(rundata=="ief" & runfocus!="countries"){recdist=0.9}else{recdist=1.2}
    avshare<-0.57
    legcex<-1.1
    minlow<-marbot*par("cxy")[2]
    maxhig<-0.9
    
    for (i in c(1:ncountries)){
        
        mytextav <- paste0(finnames[i]," ",finshares[i],"%",sep="")
        
        mytexteu<-"EU28+IS"
        
        mytexteua<-paste0("EU28+IS")
        if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
            #Give average (min-max)
            
            myround<-max(1,-floor(log10(max(eu28fin)))+2)
            mytexteub<-paste0(round(mean(eu28),myround)," (",
                              round(min(eu28),myround),"-",
                              round(max(eu28),myround),")")
            mytextav<-paste0(finnames[i]," ",
                             round(mean(eu28fin[i,]),myround)," (",
                             round(min(eu28fin[i,]),myround),"-",
                             round(max(eu28fin[i,]),myround),")")
            
        }
        if (runfocus=="trend" && runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)){
            shareAD1 <- paste0(round(act_pctcountry[i],0),"%")
            shareAD2 <- paste0(round(act_pcteu[i],0),"%/",round(ief_pcteu[i],0),"%")
            shareADeu <- paste0("(",round(act_pctcouneu,0),"%)")
        }
        
        hig=minlow+(i+0)*(maxhig-minlow)/(ncountries+1)
        mid=minlow+(i-0.5)*(maxhig-minlow)/(ncountries+1)
        low=minlow+(i-1)*(maxhig-minlow)/(ncountries+1)
        if(rundata=="adem" | runfocus=="countries"){
            #print(paste(i,low,mid,hig))
            rect(0,low,recside,hig,col=mycols[i]) 
            if (schraffierung == 1){
                rect(0,low,recside,hig,col=mycoll[i],dens=mydens[i],angle=myang1[i]) 
                rect(0,low,recside,hig,col=mycoll[i],dens=mydens[i],angle=myang2[i]) 
                rect(0,low,recside,hig) 
            }}else{
                #print(paste(i,low,mid,hig,mytcol1[i],mytcol2[i],mytcol3[i]))
                points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol1[i],col="black",lwd=1,cex=1.5)
                points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol2[i],col=mytcol3[i],lwd=1,cex=0.75)
            }
        #chartr substitutes dots with space
        #text(recside*1.2,mid,chartr("."," ",finnames[i]),cex=1.2,adj=0)
        #text(recside*recdist,mid,finnames[i],cex=legcex,adj=0)
        text(recside*recdist,mid,mytextav,cex=legcex,adj=0)
        #text(avshare,mid,mytextav,cex=legcex,adj=1)
        if (runfocus=="trend" & runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)) text(avshare+0.01,mid+0.025,shareAD1,cex=legcex-0.25,adj=0)
        if (runfocus=="trend" & runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)) text(avshare+0.01,mid-0.025,shareAD2,cex=legcex-0.25,adj=0)
    }
    
    if (rundata!="ief" ) text(0,1.00,"Average share of country",cex=legcex,adj=0,font=2)
    if (rundata=="ief" && runfocus=="value") text(0,1.00,"Average IEF of country",cex=legcex,adj=0,font=2)
    if (rundata=="ief" && runfocus=="countries") text(0,1.00,"Range of values over the period",cex=legcex,adj=0,font=2)
    if (rundata=="ief" && runfocus=="trend") text(0,1.00,"Average trend of country",cex=legcex,adj=0,font=2)
    if (runpar=="EM" && runfocus=="trend") text(0,0.95,"- Contribution AD to country trend",cex=legcex-0.3,adj=0,font=2)
    if (runpar=="EM" && runfocus=="trend") text(0,0.92,"- Contribution AD/EF to EU trend",cex=legcex-0.3,adj=0,font=2)
    if (rundata=="ief" && runfocus=="value") text(0,0.95,"% from EU28+IS average",cex=legcex,adj=0,font=3)
    if (rundata=="ief" && runfocus=="trend") text(0,0.95,"interannual change > 3%",cex=legcex,adj=0,font=2)
    
    mid=minlow+(ncountries+1-0.5)*(maxhig-minlow)/(ncountries+1)
    #print(paste(i,low,mid,hig))
    
    distance<-par()$cin[1]/par()$fin[1]*0.8
    points(x=recside/2,y=mid+distance,pch=21,bg="black",col="red",cex=1.5,lwd=2)
    text(recside*recdist,mid+distance,mytexteua,cex=legcex,adj=0,font=2)
    if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
        text(recside*recdist,mid,mytexteub,cex=legcex,adj=0,font=2)
    }
    #text(avshare,mid,"100%",cex=legcex,adj=1)
    if (runfocus=="trend" && runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)) text(avshare+0.03,mid+distance,shareADeu,cex=legcex-0.1,adj=0,font=2)
    
    
    #   box("figure",col="red",lwd=5)
    #   box("plot",col="blue",lwd=4)
    #   box("inner",col="green",lty="dotted",lwd=3)
    #   box("outer",col="black",lwd=2)
    
    #myunit<-expression("Unit: Tg N " ~~ yr^{-1})
    #mtext(myunit,1,line=0,outer=F,adj=1,cex=1.6)
    #stop("Stop after legend")
    
    #####################################################################
    #                         TITLE
    #####################################################################
    
    if (rundata=="adem" && runfocus=="value") {mtexttitle<-"Trend in EU28+IS"}
    if (rundata=="adem" && runfocus=="trend") {mtexttitle<-"Annual changes in EU28+IS"}
    if (rundata=="ief" && runfocus=="value") {mtexttitle<-"Range of values in EU28+IS"}
    if (rundata=="ief" && runfocus=="trend") {mtexttitle<-"Range in annual changes across EU28+IS"}
    if (rundata=="ief" && runfocus=="countries") {mtexttitle<-"Range values over time"}
    maxnchar<-50
    # First word which is beyond the max number of characters to be displayed
    mtexttitle0<-paste0(textcat," - ",textsou)
    if(rungas!="no gas"){
            mtexttitle0<-paste0(mtexttitle0,gsub(" ","",rungas))
        }else{
            mtexttitle0<-mtexttitle0
        }
    mtexttitle0<-gsub("_","-",mtexttitle0)    

    mvect<-strsplit(mtexttitle0," - ")[[1]]
    maxwords<-max(length(mvect),
                  match(TRUE,lapply(1:length(mvect),function(x) sum(nchar(mvect[1:x]))>maxnchar))-1,
                  na.rm=TRUE)
    mtexttitle1<-gsub(",","",toString(mvect[1:maxwords]))
    if(maxwords>length(mvect)){
        mtexttitle2<-paste(gsub(",","",toString(mvect[(maxwords+1):length(mvect)])),mtexttitle)
    }else{
        mtexttitle2<-mtexttitle
    }
    
    par(omd=c(0,1,yhea,1))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    text(0.5,0.65,adj=0.5,mtexttitle1,cex=1.3)
    text(0.5,0.15,adj=0.5,mtexttitle2,cex=1.2)
    
    #stop("Stop after title")
    # 
    #####################################################################
    #                        X-AXIS TITLE
    #####################################################################
    
    mtextxaxis<-"Year"
    
    par(omd=c(xstt,xleg,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    text(0.5,1-1.1*par("cin")[2],adj=0.5,mtextxaxis,cex=1.2,font=2)
 

  


    
    par(omd=c(0,xleg,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    
    #text(0,par("cin")[2],adj=0,mfooter1,cex=0.8)
    #text(0,0,adj=0,mfooter2,cex=0.8)
    
    #stop("Stop after x-axis")
    
    #  box("figure",col="red",lwd=5)
    #  box("plot",col="blue",lwd=4)
    #  box("inner",col="green",lty="dotted",lwd=3)
    #  box("outer",col="black",lwd=2)
#####################################################################
#                        FOOTNOTE
#####################################################################
    par(omd=c(0,1,0,ystt))
    par(mar=c(0,0,0,0)) #bot,lef,top,rig
    par(fig=c(0,1,0,1),new=T)
    
    foottextleft<-paste0("EU-GIRP.v2 (EU-Greenhouse gas Inventory Reporting Plots) (c) EC-JRC/AL https://github.com/aleip/eealocatorplots.git")
    foottextrigt<-paste0(figdate," - UID: ",textuid)    

    plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
    text(0.005,0.1,adj=0,foottextleft,cex=0.5,font=1)
    text(0.995,0.1,adj=1,foottextrigt,cex=0.5,font=1)

dev.off()
    #dev.off(dev.cur())
#}
#}