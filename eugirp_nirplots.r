# This function creates the various plots that can be used for the NIR,
# in particular: value-plots, trend-plots, and country-plots.
graphics.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#### GENERAL PLOT INFORMATION #################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# DETERMINE FIGURE NAME #######################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Cleaning up
runsect[1]<-gsub(paste0(" ",unlist(runmeta[2])),"",unlist(runsect[1]))

runsector<-as.vector(unlist(runsect[1]))
runcategory<-as.vector(unlist(runsect[2]))
runcategory<-gsub("Farming","",runcategory)
runmeastype<-as.vector(unlist(runmeas[1]))
rungas<-as.vector(unlist(runmeas[2]))
rununit<-as.vector(unlist(runmeas[3]))
runmeasure<-as.vector(unlist(runmeas[4]))
runsource<-as.vector(unlist(runmeta[1]))
runsource<-as.vector(unlist(runmeta[2]))
runsource<-as.vector(unlist(runmeta[3]))
runtarget<-as.vector(unlist(runmeta[4]))
runtype<-as.vector(unlist(runmeta[5]))

#runmethod<-paste(runmethod,runclassification,runsource,runtarget,runtype,runoption,collapse="")

runcateg<-paste(lapply(runsect,as.character),collapse=" ")
runcateg<-gsub("/","-",runcateg)
if(grepl("^4",runsect[1])) runcateg<-runsect[1]
runmethod<-paste(as.vector(unlist(runmeta[metafields[runmeta[metafields]!=""]])),collapse=". ")
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
    if(rungas!="no gas"){mtexttitle0<-paste0(mtexttitle0,": ",gsub(" ","",rungas))}
    mtexttitle0<-gsub("_","-",mtexttitle0)    
}


fnammethod<-gsub(" ","",mtexttitle0)
fnammethod<-gsub("[/: .]","",fnammethod)
fnammethod<-paste0(fnammethod,runmeastype)

# fnampar<-runmeasure
# fnampar<-gsub(" ","",fnampar)
# fnampar<-gsub("/","-",fnampar)
# fnampar<-gsub("Indirectemissions","EMind",fnampar)
# fnampar<-gsub("Emissions","EM",fnampar)
# fnampar<-gsub("Implied_emission_factor","IEF",fnampar)
# runcategmetgas<-paste0(gsub(" ","",runcateg),"-",fnammethod,".")
# runcategmetgas<-substr(runcategmetgas,0,50)
# if(rungas!="no gas"){
#     runcategmetgas<-paste0(runcategmetgas,fnampar,gsub(" ","",rungas))
# }else{
#     runcategmetgas<-paste0(runcategmetgas,fnampar,runmeastype)
# }
figdir<-figdate
if(plotparamcheck==1) figdir<-paste0(issuedir,"countryoutliers")
if (! file.exists(figdir)){
    dir.create(file.path(figdir))
    #    setwd(file.path(mainDir, figdate))
}
if(plotparamcheck!=1) runfoc<-paste0(runfoc,formatC(imeas,width=ceiling(log10(nrow(plotmeas))),flag="0"))
figname<-paste0(figdir,"/",fnammethod,"-",runfoc,"~",figdate,".",plotformat,collapse=NULL)
print(paste0(imeas,"/",nrow(plotmeas),": ",figname))
#postscript(figname)
if(plotformat=="pdf") pdf(file=figname,width=11,height=6)
if(plotformat=="png") png(file=gsub("pdf","png",figname),width=11,height=6,unit="in",res=plotresolution)
if(plotformat=="jpg") jpeg(file=gsub("pdf","jpg",figname),width=11,height=6,unit="in",res=plotresolution)
if(length(curuid)>1){stop("More than one UID selected for graph!!")}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FORMATTING OF THE UNIT ######################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
if(curunit=="kg/head/yr"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
 if(curunit=="kg/yr"){textunit<-"[ kg " ~~ yr^{-1} ~"]"} else
 if(curunit=="kg/day"){textunit<-"[ kg " ~~ day^{-1} ~"]"} else
 if(curunit=="h/day"){textunit<-"[ h " ~~ day^{-1} ~"]"} else
 if(curunit=="MJ/day"){textunit<-"[ MJ " ~~ day^{-1} ~"]"} else
 if(curunit=="t/yr"){textunit<-"[ t " ~~ yr^{-1} ~"]"} else
 if(curunit=="kg/year"){textunit<-"[ kg " ~~ yr^{-1} ~"]"} else
 if(curunit=="t/year"){textunit<-"[ t " ~~ yr^{-1} ~"]"} else
 if(curunit=="kt/year"){textunit<-"[ kt " ~~ yr^{-1} ~"]"} else
 if(curunit=="Mt/year"){textunit<-"[ Mt " ~~ yr^{-1} ~"]"} else
 if(curunit=="kg/animal"){textunit<-"[ kg " ~~ animal^{-1} ~"]"} else
 if(curunit=="kg N/yr"){textunit<-"[ kg N " ~~ yr^{-1} ~"]"} else
 if(curunit=="kg N/year"){textunit<-"[ kg N " ~~ yr^{-1} ~"]"} else
 if(curunit=="t N/yr"){textunit<-"[ t N " ~~ yr^{-1} ~"]"} else
 if(curunit=="t N/year"){textunit<-"[ t N " ~~ yr^{-1} ~"]"} else
 if(curunit=="kt N/year"){textunit<-"[ kt N " ~~ yr^{-1} ~"]"} else
 if(curunit=="kg/head/year"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
 if(curunit=="kg N/head/year"){textunit<-"[ kg N " ~~ head^{-1}  ~~ yr^{-1} ~"]"} else
 if(curunit=="kg dm/head/day"){textunit<-"[ kg dm " ~~ head^{-1}  ~~ day^{-1} ~"]"} else
 if(curunit=="t dm/ha"){textunit<-"[ t dm " ~~ ha^{-1} ~"]"} else
 if(curunit=="t CO2-C/t"){textunit<-"[ t CO"[2]*"-C" ~~ t^{-1} ~"]"} else
 if(curunit=="MJ/head/day"){textunit<-"[ MJ " ~~ head^{-1}  ~~ day^{-1} ~"]"} else
 if(curunit=="kg N2O-N/kg N"){textunit<-expression(" [kg N"[2]*"O (kg N)"^'-1'*']')} else
 if(curunit=="kg N2O/kg N handled"){textunit<-expression(" [kg N"[2]*"O (kg N handled)"^'-1'*']')} else
 if(curunit=="kg N2O/head/year"){textunit<-expression("[ kg N"[2]*"O head"^'-1'*"year"^'-1'*']')} else
 if(curunit=="kt C"){textunit<-"[ kt C ]"} else
 if(curunit=="Mt C"){textunit<-"[ Mt C ]"} else
 if(curunit=="kt DC"){textunit<-"[ kt DC ]"} else
 if(curunit=="Mt DC"){textunit<-"[ Mt DC ]"} else
 if(curunit=="kg/t dm"){textunit<-expression("[ kg (t dm)"^'-1'*']')} else
 if(curunit=="t/t dm"){textunit<-expression("[ t (t dm)"^'-1'*']')} else
 if(curunit=="t/ha"){textunit<-expression("[ t (ha)"^'-1'*']')} else
 if(curunit=="kg dm"){textunit<-expression("[ kg dm ]")} else
 if(curunit=="t dm"){textunit<-expression("[ t dm ]")} else
 if(curunit=="kt dm"){textunit<-expression("[ kt dm ]")} else
 if(curunit=="Gg"){textunit<-"[ Gg ]"} else
 if(curunit=="kg"){textunit<-"[ kg ]"} else
 if(curunit=="t"){textunit<-"[ t ]"} else
 if(curunit=="1000 metric t"){textunit<-"[ 1000 metric t ]"} else
 if(curunit=="kt"){textunit<-"[ kt ]"} else
 if(curunit=="ha"){textunit<-"[ ha ]"} else
 if(curunit=="kha"){textunit<-"[ kha ]"} else
 if(curunit=="Mio ha"){textunit<-"[ Mio ha ]"} else
 if(curunit=="ha/year"){textunit<-"[ ha " ~~ yr^{-1} ~"]"} else
 if(curunit=="kha/year"){textunit<-"[ kha " ~~ yr^{-1} ~"]"} else
 if(curunit=="Mha/year"){textunit<-"[ Mha " ~~ yr^{-1} ~"]"} else
 if(curunit=="1000 m^3"){textunit<-"[ 1000 m"^{3} ~~ "]"} else
 if(curunit=="10^6 m^3"){textunit<-"[ 10"^{6} ~ "m"^{3} ~~ "]"} else
 if(curunit=="10^9m^2/year"){textunit<-"[ 10"^{9} ~ "m"^{2} ~ "yr"^{-1} ~ "]"} else
 if(curunit=="m^3/kg VS"){textunit<-"[ m"^{3} ~ "(kg VS)"^{-1} ~ "]"} else
 if(curunit=="Mt"){textunit<-"[ Mt ]"} else
 if(curunit=="Mg"){textunit<-"[ Mg ]"} else
 if(curunit=="kt CO2 equivalent"){textunit<-"[ kt" ~ CO[2] ~ "eq ]"} else
 if(curunit=="Mt CO2 equivalent"){textunit<-"[ Mt" ~ CO[2] ~ "eq ]"} else
 if(curunit=="1000s"){textunit<-"[ 1000s" ~"]"} else
 if(curunit=="TJ"){textunit<-"[ TJ" ~"]"} else
 if(curunit=="1000 TJ"){textunit<-"[ 1000 TJ" ~"]"} else
 if(curunit=="kMt"){textunit<-"[ kMt" ~"]"} else
 if(curunit=="Mio"){textunit<-"[ Mio" ~"]"} else
 if(curunit=="%"){textunit<-"[ %" ~"]"} else
 if(curunit==""){textunit<-""} else
 stop(curunit)
     #{textunit<-curunit} 





#mydens<-as.vector(as.numeric(attributes[rownames(eu28fin),"dens"]))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# IDENTIFYING ATTRIBUTES ######################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
attributes<-as.matrix(read.table("eugirp_attributes.txt",header=T,row.names=1,check.names=F))

formatfinnames<-finnames

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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# TICKS #######################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("# Getting order of magnitude of ticks")
teval<-eu28
if(rundata=="ief"){teval<-curmatrix}
tevalpos<-eu28pos
tevalneg<-eu28neg

# tevalsmall and tevallarge give the min and max eu-sum, including negative values over the years
# --
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

tmax<-tmag*ceiling(tdifmax/tmag)
tmin<-tmag*floor(tdifmin/tmag)
if(rundata!="ief"){tmin<-min(0,tmin)}
if(is.nan(tmin)){tmin<-0.5*min(teval,na.rm=T)}
if(is.nan(tmax)){tmax<-1.5*max(teval,na.rm=T)}
if(tmin==0 & tmax==0){tmin<--0.5;tmax<-0.5}


# Number of ticks should be between tnlow and tnhig
tpos<-c(40,20,10,5,2.5,2,1)
tnlow=5
tnhig=10
if(tmag==0){tdis=5}else{
    for (i in tpos){
        tcur<-(tmax-tmin)/(i*tmag)
        if(tcur<=tnhig){tdis<-tcur}
    }}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("Start plotting")
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
    tnum<-(tmax-tmin)/tdis
    if(tnum<1){ndig<-abs(floor(log10(abs(tmax))))}else{ndig<-0}
    tseq<-round(seq(tmin,tmax,tnum),ndig)
    axis(2,at=tseq,tick=1,line=1,pos=c(0,0),lwd=2,las=1)
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
        
        axis(1,at=1+yearsnum-yearsnum[1],adj=1, tick=TRUE, 
             line=NA, lty=1, lwd=1,
             labels=rep("",length(years))
        )
        text(x=1+yearsnum-yearsnum[1],
             y=par()$usr[3]-0.04*(par()$usr[4]-par()$usr[3]),
             labels=years, 
             srt=90, adj=1, 
             xpd=TRUE)
        
        for (i in c(1: min(topn+1,nrow(eu28fin)))){
            #print(paste(i,eu28fin[i,1],myticks[i],mytcol1[i],mytcol2[i],mytcol3[i]))
            points(eu28fin[i,],pch=myticks[i],col="black"   ,bg=mytcol1[i],lwd=1,cex=1.5)
            points(eu28fin[i,],pch=myticks[i],col=mytcol3[i],bg=mytcol2[i],lwd=1,cex=0.75)
        }
        
        points(matrix(eu28),pch=21,bg="black",col="red",cex=1.5,lwd=2)
        #points(matrix(eu28),pch=21,bg="black",col="red",cex=5.5,lwd=5)
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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LABLES Y-AXIS #####################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("Plot Labels y-axis")
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
    textunit<-"Interannual" ~" growth"
}            

#text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=min(1.5,1.5*30/nchar(textpar)),textpar,las=3,srt=90,font=2)
writetext<-multilines(runmeasure,60)

if(length(writetext)>1){newcex<-0.9}else{newcex<-min(1.5,1.0*40/nchar(writetext[1]))}
text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=newcex,writetext[1],las=3,srt=90,font=1)
if(length(writetext)>1){
    text(0.22,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=newcex,writetext[2],las=3,srt=90,font=1)
}
text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3,textunit,las=3,srt=90,font=1)
#print(paste(curunit,textunit))
#text(0.4,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.3,curunit,las=3,srt=90,font=2)

#stop("Stop after y-axis")
# box("figure",col="red",lwd=5)
# box("plot",col="blue",lwd=4)
# box("inner",col="green",lty="dotted",lwd=3)
# box("outer",col="black",lwd=2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LEGEND ###########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("Plot legend")
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
maxfins<-max(abs(finshares))
minfins<-min(abs(finshares))
if(maxfins>1000) legcex<-1.0
if(rundata=="ief") legcex<-0.9
nzeros<-max(0,3-ceiling(log10(maxfins)))
if(is.infinite(nzeros))nzeros<-1
addzeros<-paste(rep("0",nzeros),collapse="")
minlow<-marbot*par("cxy")[2]
maxhig<-0.9
eu28ispos<-0.87
omitcountry<-0

nomit<-length(finshares[finshares==0])
if(ncountries>0){
    for (i in c(1:ncountries)){
        
        if(finshares[i]!=0){
            #print(paste(i,ncountries,finnames[i],finshares[i],sep="-"))
            mytextavc <- finnames[i]
            checkdig<-round(finshares[i],nzeros)
            if(round(checkdig,0)==checkdig){checkdig<-paste0(checkdig,".",addzeros)}
            mytextavv <- paste0(checkdig,"%",sep="")
            
            mytexteu<-eukp
            
            mytexteua<-paste0(eukp)
            if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
                #Give average (min-max)
                
                myround<-max(1,-floor(log10(max(eu28fin)))+2)
                mytexteub<-paste0(round(mean(t(eu28),na.rm=T),myround))
                mytexteuc<-paste0("(",
                                  round(min(eu28,na.rm=T),myround),"-",
                                  round(max(eu28,na.rm=T),myround),")")
                mytextavc<-finnames[i]
                mytextavv<-paste0(round(mean(eu28fin[i,],na.rm=T),myround))
                mytextran<-paste0("(",round(min(eu28fin[i,],na.rm=T),myround),"-",
                                  round(max(eu28fin[i,],na.rm=T),myround),")")
                
            }
            if (sharesexist){
                #if(!is.na(act_pctcountry)) 
                shareAD1 <- paste0(round(act_pctcountry[i],0),"%")
                #if(!is.na(act_pcteu)) 
                shareAD2 <- paste0("(",round(act_pcteu[i],0),"%/",round(ief_pcteu[i],0),"%)")
                shareADeu <- paste0("(",round(act_pctcouneu,0),"%)")
            }
            
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
                }}else{
                    #print(paste(i,low,mid,hig,mytcol1[i],mytcol2[i],mytcol3[i]))
                    if(finshares[i]!=0){
                        points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol1[i],col="black",lwd=1,cex=1.5)
                        points(recside/2,low+(hig-low)/2,pch=myticks[i],bg=mytcol2[i],col=mytcol3[i],lwd=1,cex=0.75)
                    }
                }
            #chartr substitutes dots with space
            #text(recside*1.2,mid,chartr("."," ",finnames[i]),cex=1.2,adj=0)
            #text(recside*recdist,mid,finnames[i],cex=legcex,adj=0)
            #text(avshare,mid,mytextav,cex=legcex,adj=1)
            if (sharesexist) {
                text(avshare+0.03,eu28ispos,shareADeu,cex=legcex-0.1,adj=0,font=2)
                text(recside*recdist,mid,mytextavc,cex=legcex-0.25,adj=0)
                text(2.2*recside*recdist,mid,mytextavv,cex=legcex-0.25,adj=1)
                text(avshare+0.11,mid+0.000,shareAD1,cex=legcex-0.25,adj=1)
                text(avshare+0.12,mid-0.000,shareAD2,cex=legcex-0.25,adj=0)
            }else{
                text(recside*recdist,mid,mytextavc,cex=legcex,adj=0)
                if(rundata=="ief"){
                    text(avshare+0.00,mid,mytextavv,cex=legcex,adj=1)
                    text(avshare+0.40,mid,mytextran,cex=legcex,adj=1)
                }else{
                    text(2.6*recside*recdist,mid,mytextavv,cex=legcex,adj=1)
                    text(2.6*recside*recdist,mid,mytextran,cex=legcex,adj=1)
                }
            }
        }
        else{
            omitcountry<-omitcountry+1        }
    }
}

#if (rundata!="ief" ) text(0,1.00,"Average share of country",cex=legcex,adj=0,font=2)
if (rundata!="ief" ) text(0,1.00,paste0("Share in year t-2 (",years[length(years)],")"),cex=legcex,adj=0,font=2)
if (rundata=="ief" && runfocus=="value") text(0,1.00,"Average IEF of country (min-max)",cex=legcex,adj=0,font=2)
if (rundata=="ief" && runfocus=="countries") text(0,1.00,"Range of values over the period",cex=legcex,adj=0,font=2)

if (rundata=="ief" && runfocus=="trend") text(0,1.00,"Average trend of country",cex=legcex,adj=0,font=2)
if (runmeastype=="EM" && runfocus=="trend") text(0,0.96,"- Contribution AD to country trend",cex=legcex-0.3,adj=0,font=2)
if (runmeastype=="EM" && runfocus=="trend") text(0,0.93,"- Contribution AD/EF to EU trend",cex=legcex-0.3,adj=0,font=2)

#if (rundata=="ief" && runfocus=="value") text(0,0.95,"% from EU28+IS average",cex=legcex,adj=0,font=3)
#if (rundata=="ief" && runfocus=="trend") text(0,0.95,"interannual change > 3%",cex=legcex,adj=0,font=2)

mid=minlow+(ncountries+1-0.5)*(maxhig-minlow)/(ncountries+1)
#print(paste(i,low,mid,hig))

distance<-par()$cin[1]/par()$fin[1]*0.8

if(rundata=="ief" & (runfocus=="value" | runfocus=="countries")){
    eu28ispos<-0.91
    #text(recside*recdist,mid,mytexteub,cex=legcex,adj=1,font=2)
    #text(recside*recdist,mid,mytexteuc,cex=legcex,adj=1,font=2)
    text(avshare+0.00,eu28ispos,mytexteub,cex=legcex,adj=1,font=2)
    text(avshare+0.40,eu28ispos,mytexteuc,cex=legcex,adj=1,font=2)
}
points(x=recside/2,y=eu28ispos,pch=21,bg="black",col="red",cex=1.5,lwd=2)
#text(recside*recdist,mid+distance,mytexteua,cex=legcex,adj=0,font=2)
text(recside*recdist,eu28ispos,mytexteua,cex=legcex,adj=0,font=2)

#text(avshare,mid,"100%",cex=legcex,adj=1)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Box with explanation country-selection ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
par(omd=c(xleg,1,ystt/2,ystt+0.1))
#par(mar=c(1.0,1.0,5.0,1.)) #bot,lef,top,rig
par(mar=c(0,marlef,0,marrig))

par(fig=c(0,1,0,1),new=T)
par(lty=1,col="black")
plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
rect(0.0,0.0,1,1)

if(length(textorder)>0){
    curmax=50
    curcex=0.7
    text2write<-multilines(textorder,curmax)
    if(length(text2write)>7){
        curmax=59
        curcex<-0.6
        text2write<-multilines(textorder,curmax)
    }
    for(i in c(1:length(text2write))){
        text(0.03,0.95-0.2*curcex*i,text2write[i],adj=0,cex=curcex)    
        
    }
}

#   box("figure",col="red",lwd=5)
#   box("plot",col="blue",lwd=4)
#   box("inner",col="green",lty="dotted",lwd=3)
#   box("outer",col="black",lwd=2)

#myunit<-expression("Unit: Tg N " ~~ yr^{-1})
#mtext(myunit,1,line=0,outer=F,adj=1,cex=1.6)
#stop("Stop after legend")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  TITLE ############################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("Plot title")
if (rundata=="adem" && runfocus=="value") {mtexttitle<-"Trend in the EU-KP"}
if (rundata=="adem" && runfocus=="trend") {mtexttitle<-"Annual changes in the EU-KP"}
if (rundata=="ief" && runfocus=="value") {mtexttitle<-"Range of values in the EU-KP"}
if (rundata=="ief" && runfocus=="trend") {mtexttitle<-"Range in annual changes across the EU-KP"}
if (rundata=="ief" && runfocus=="countries") {mtexttitle<-"Range values over time"}

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

par(omd=c(0,1,yhea,1))
par(mar=c(0,0,0,0)) #bot,lef,top,rig
par(fig=c(0,1,0,1),new=T)
plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
text(0.5,0.65,adj=0.5,mtexttitle1,cex=1.3)
text(0.5,0.15,adj=0.5,mtexttitle2,cex=1.25)

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

text(0.5,1-1.1*par("cin")[2],adj=0.5,mtextxaxis,cex=1.2,font=1)


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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#  FOOTNOTE #########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#print("Plot footnote")
par(omd=c(0,1,0,ystt))
par(mar=c(0,0,0,0)) #bot,lef,top,rig
par(fig=c(0,1,0,1),new=T)

foottextleft<-paste0("EU-GIRP.v2 (EU-Greenhouse gas Inventory Reporting Plots) (c) EC-JRC/AL https://github.com/aleip/eealocatorplots.git")
foottextrigt<-paste0(figdate," - UID: ",curuid, ". Submission from ",cursubm)

plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
text(0.005,0.1,adj=0,foottextleft,cex=0.5,font=1)
text(0.995,0.1,adj=1,foottextrigt,cex=0.5,font=1)

graphics.off()
