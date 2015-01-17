library(ggplot2)
rm(list=objects())
#dev.off()

#Link with EEA locator tool
#########################################

# Define if data from xls-file need to be updated (1) or not (0)
#        Note that the update requires sign. time.
loadnew <- 0

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
invyear<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
csvfil <-"eealocatortest_2014_AllAllAll.csv"
setwd(invyear)

# CSV file generated as follows:
# - Load EEA locator cube in excel pivot table
#   - Report Filter: Submission Year
#   - Column Label: Inventory Year
#   - Row Labels: Variable name, Party code, Notation key
# Export table to csv eealocatortest.csv
# Process csv with eealocator_csv.bash to alldata_allbrf.csv
#   Main tasks
#   - Replace long names with acronyms (defined in excel file meta_data_dimensions.xlsx
#     which needs to be exported as csv meta_data_dimensions.csv)
#   - Combine dimension-columns and remove unnecessary ones
#     Currently: 1. category and source (categorysource)
#                2. measure and gas (measuregas)
#                3. unit
#                4. party
#                5. values for years 1990ff (not those before 1990)
alldata<-read.csv(csvfil,na.string="-999")

# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
source("lists.txt")

# Load the current tasks to do
# See file curplot.csv for information on how to set it up
curtasks <- readLines("curplot.csv")
starttasks<-0
for (icurtasks in 1:length(curtasks)){
    curtask<-curtasks[icurtasks]
    if(starttasks==1){
        curtaskdetails<-unlist(strsplit(curtask,","))
        curcateg<-curtaskdetails[1]
        cursourc<-curtaskdetails[2]
        curgases<-curtaskdetails[3]
        print(paste(curtask,curcateg,cursourc,curgases))
        
        
        focus<-as.data.frame(matrix(NA, nrow=10, ncol=4))
        names(focus)<-c("ad","parameter","datatype","focus")
        nfocus<-1
                
        #Generate matrix of tasks from curplot.csv indicating: 
        # - Activity data (are required for AD plots and IEF plots for weighting)
        # - Parameter to plot (AD, EM, IEF, or other factor)
        # - Data type (determined automatically: summable (AD) or averageable (IEF))
        # - Focus: value plot (as trend), 
        #          trend plot (as inter-annual changes), 
        #          country plot (range as values from trend)
        for (icurtask in seq(4,length(curtaskdetails),2)){
            curad<-"AD"
            curpar<-curtaskdetails[icurtask]
            icurtask<-icurtask+1
            allfoci<-as.numeric(curtaskdetails[icurtask])
            
            #Check for value plot
            if ((allfoci %% 2) == 1){
                #print(paste("value before",allfoci,(allfoci %% 2)))
                allfoci<-allfoci-1
                focus[nfocus,1]<-curad
                focus[nfocus,2]<-curpar
                if(curpar=="AD" | substr(curpar,0,2)=="EM"){focus[nfocus,3]<-"adem"}else{focus[nfocus,3]<-"ief"}
                focus[nfocus,4]<-"value"
                #print(paste("Set current task",nfocus,"AD=",curad,"Par=",curpar,"cudat=",focus[nfocus,3],"curplot=",focus[nfocus,4],sep=""))
                nfocus<-nfocus+1
            }
            # Check for trend plot
            if ((allfoci %% 4) == 2){
                #print(paste("trend before",allfoci,(allfoci %% 4)))
                allfoci<-allfoci-2
                focus[nfocus,1]<-curad
                focus[nfocus,2]<-curpar
                if(curpar=="AD" | substr(curpar,0,2)=="EM"){focus[nfocus,3]<-"adem"}else{focus[nfocus,3]<-"ief"}
                focus[nfocus,4]<-"trend"
                #print(paste("Set current task",nfocus,"AD=",curad,"Par=",curpar,"cudat=",focus[nfocus,3],"curplot=",focus[nfocus,4],sep=""))
                nfocus<-nfocus+1
            }
            # Check for country plot (IEF and parameters only)
            if((! curpar=="AD") & (! substr(curpar,0,2)=="EM")){
                #print(paste("countries",allfoci,(allfoci %% 8)))
                if ((allfoci %% 8) == 4){
                    allfoci<-allfoci-4
                    focus[nfocus,1]<-curad
                    focus[nfocus,2]<-curpar
                    if(curpar=="AD" | substr(curpar,0,2)=="EM"){focus[nfocus,3]<-"adem"}else{focus[nfocus,3]<-"ief"}
                    focus[nfocus,4]<-"countries"
                    #print(paste("Set current task",nfocus,"AD=",curad,"Par=",curpar,"cudat=",focus[nfocus,3],"curplot=",focus[nfocus,4],sep=""))
                    nfocus<-nfocus+1
                }}
            
        }
        
        for (numrun in 1:(nfocus-1)){
            
            
            #Copy plot-requests to current plot
            curad   <-focus[numrun,1]
            curpar  <-focus[numrun,2]
            curdata <-focus[numrun,3]
            curfocus<-focus[numrun,4]
            
            print(paste("Current task ",numrun,". AD=",curad,". Par=",curpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))
            
            
            #             if(numrun==1){curdata="adem";curfocus="value"}
            #             if(numrun==2){curdata="adem";curfocus="trend"}
            #             if(numrun==3){curdata="ief";curfocus="value"}
            #             if(numrun==4){curdata="ief";curfocus="trend"}
            #             if(numrun==5){curdata="ief";curfocus="countries"}

            
            tmp0<-subset(alldata,measure==curpar,select=-measure)
            tmp1<-subset(tmp0,gas==curgases,select=-gas)
            tmp2<-subset(tmp1,category==curcateg,select=-category);
            tmp3<-subset(tmp2,source==cursourc,select=-source);
            u<-subset(tmp3,select=unit)
            p<-subset(tmp3,select=party)
            x<-subset(tmp3,select=c(-party,-unit))
            y<-as.matrix(sapply(x,as.numeric))
            row.names(y)<-p$party
            colnames(y)<-gsub("X","",gsub("X","",colnames(y)))
            eealocator<-y
            rm(list=c("tmp0","tmp1","tmp2","tmp3"))
            
            #Think about text-elements later
            #cat<-"4A1";ad<-"AD";ief<-"IEF"
            curunit<-unique(u)
            
            
            dimensions<-read.csv("meta_data_dimensions.txt")
            if(curunit=="kg/head/yr"){textunit<-"[ kg " ~~ head^{-1}  ~~ yr^{-1} ~"]"}
            if(curunit=="Gg"){textunit<-"[Gg" ~"]"}
            
            textcat<-dimensions[dimensions[2]==curcateg][1]
            textpar<-dimensions[dimensions[2]==curpar][1]

            #### GENERAL PLOT INFORMATION
            #source("c:/adrian/models/capri/dndc/results/20110722/nitrogen/figures/plotdefaults.r")
            xstt=0.15
            xleg=0.7
            hasfootnote<-0
            hastitle<-1
            if (hasfootnote==1){ystt=0.10}else{ystt=0.05}
            if (hastitle==1){yhea=0.95} else {yhea=1.0}
            bspace=0.1
            par(xpd=F)
            schraffierung<-1
            
            
            nyears<-ncol(eealocator)
            
            #Get emission matrix
            
            #Calculate trend
            if (curdata=="adem"){eeatrend<-eealocator[,2:nyears]-eealocator[,1:nyears-1]}
            if (curdata=="ief") {eeatrend<-eealocator[,2:nyears]/eealocator[,1:nyears-1]}
            
            topn<-10
            
            if (curfocus=="trend"){curmatrix<-eeatrend}
            if (curfocus=="value"){curmatrix<-eealocator}
            if (curfocus=="countries"){curmatrix<-eealocator}
            
            figdate<-format(Sys.time(), "%Y%m%d")
            figname<-paste("ghgplot","_",curdata,"_",curfocus,"~",figdate,".pdf",collapse=NULL,sep="")
            #postscript(figname)
            pdf(file=figname,width=11,height=6)
            
            
            if(curdata=="adem"){
                #print("Calculate EU data")
                eu28<-colSums(curmatrix,na.rm=T)
                rel<-t(t(curmatrix)/eu28)
                relav<-rowMeans(rel)
                eu28.trend<-colSums(eeatrend)
                rel.trend<-t(t(eeatrend)/eu28.trend)
                relav.trend<-rowMeans(rel.trend)
                
                topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
                
            }
            
            if(curdata=="ief"){
                eu28<-colMeans(curmatrix,na.rm=T)
                
                
                #Calculate relative value
                if(curfocus=="value"){rel<-t(t(curmatrix)/eu28)}
                if(curfocus=="countries"){rel<-t(t(curmatrix)/eu28)}
                if(curfocus=="trend"){rel<-eeatrend}
                
                #Keep only those values to plot which are not overlaying the boxplot
                endrange<-0.3
                lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
                upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
                lowerok<-(curmatrix<lowerend)
                upperok<-(curmatrix>upperend)
                relna<-lowerok+upperok
                
                #Use absolute relative deviation as sorting criterium
                #Store the maximum absolute relative deviation
                relabs<-abs(1-rel)
                relav<-rowMeans(relabs)
                relav<-apply(relabs,1,max)
                
                #Attention! this needs to be a weighted mean
                eu28.trend<-colMeans(eeatrend,na.rm=T)
                rel.trend<-t(t(eeatrend)/eu28.trend)
                relav.trend<-rowMeans(rel.trend,na.rm=T)
                
                #Keep code below in case such a criterion will be applied later
                #if(curfocus=="value"){reldiff=0.1}
                #if(curfocus=="trend"){reldiff=0.03}
                #if(curfocus=="countries"){reldiff=0.03}
                #rel[is.na(rel)]<- 999
                #relna<-rel
                #relna[relna==-999]<- 999
                ##keep only those which down-deviation larger that relidiff 
                #relna[relna<1-reldiff]<-1
                #relna[relna==999]<--999
                ##keep only those which up-deviation larger that relidiff 
                #relna[relna>1+reldiff]<-1
                #relna[relna!=1]<-0
                ##Now delete all the value with small deviations
                relplot=curmatrix*relna
                
                if(curfocus=="value"){
                    relplot[relplot==0]<-NA
                    #Criterion for selecting country: largest average deviation from EU average
                    # --nr.rm=F keeps only those countries which have large deviations for the whole time series
                    #relav<-rowMeans(relplot,na.rm=F)
                }
                
                if(curfocus=="trend"){
                    #Criterion for selecting country: largest internnual change in timeseries
                    #relav<-apply(relplot,1,max)
                    relplot[relplot==0]<-NA
                }  
                
                #---> first determine number of non-NA elements
                topn<-min(10,length(sort(relav,decreasing=F)))
                topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                topneu28<-sort(topneu28,decreasing=F)
                topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
            }
            
            #print("Determine the the top n countries contributing on average most to EU28 values")
            topnnames<-names(topneu28)
            toponames<-names(topother)
            ncountries<-topn+1
            
            
            #print("Extract top n countries from dataset and group other together for plotting")
            eu28main<-curmatrix[topnnames,]
            eu28rest<-curmatrix[toponames,]
            if(curdata=="ief" && curfocus=="trend"){
                eu28main<-relplot[topnnames,]
                eu28rest<-relplot[toponames,]
            }
            if(curdata=="adem"){Other<-colSums(eu28rest,na.rm=T)}else
            {Other<-colMeans(eu28rest,na.rm=T)}
            
            #print("# ---> combine Main countries with the 'other' countries")
            eu28fin<-rbind(eu28main,Other)
            finnames<-rownames(eu28fin)
            finshares<-round(rowMeans(eu28fin,na.rm=T)/mean(eu28)*100,1)
            
            
            #mydens<-as.vector(as.numeric(attributes[rownames(eu28fin),"dens"]))
            attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
            mydens<-(as.numeric(attributes[rownames(eu28fin),"dens"]))
            mycols<-(attributes[rownames(eu28fin),"color"])
            mycoll<-(attributes[rownames(eu28fin),"coll"])
            myang1<-(as.numeric(attributes[rownames(eu28fin),"ang1"]))
            myang2<-(as.numeric(attributes[rownames(eu28fin),"ang2"]))
            
            myticks<-(as.numeric(attributes[rownames(eu28fin),"symbol"]))
            mytcol1<-(attributes[rownames(eu28fin),"col1t"])
            mytcol2<-(attributes[rownames(eu28fin),"col2t"])
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
            if(curdata=="ief"){teval<-curmatrix}
            
            tpos<-c(20,10,5,2.5,2,1)
            tdif<-max(teval,na.rm=T)-min(teval,na.rm=T)
            tmag<-10^floor(log10(tdif))/2
            if(curdata=="ief" && curfocus=="trend"){tmag=tmag/2.5}
            tmin<-tmag*floor((min(teval,na.rm=T))/tmag)
            if(curdata!="ief"){tmin<-min(0,tmin)}
            tmax<-tmag*ceiling((max(teval,na.rm=T))/tmag)
            
            # Number of ticks should be between tnlow and tnhig
            tnlow=5
            tnhig=10
            for (i in tpos){
                tcur<-tmax/(i*tmag)
                if(tcur<=tnhig){tdis<-tcur}
            }
            
            if(curdata!="ief"){
                eu28finpos<-(eu28fin>0)*eu28fin
                eu28finneg<-(eu28fin<0)*eu28fin
                df.bar<-barplot(eu28finpos,ylim=c(tmin,tmax),yaxp=c(tmin,tmax,tdis),col=mycols,xpd=F,axes=F,las=2)
                barplot(eu28finneg,add=T,col=mycols,axes=F,axisnames=F,las=2)
                abline(h=0)
                axis(1,at=c(1990:2012),line=1,lwd=2,las=1,pos=c(0,0))
                axis(2,at=seq(tmin,tmax,tmax/tdis/5),tick=1,line=1,pos=c(0,0),lwd=1,
                     labels=F,
                     col.ticks="grey")
                axis(2,at=seq(tmin,tmax,tmax/tdis),tick=1,line=1,pos=c(0,0),
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
                
                if(curfocus=="countries"){
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
                    #Convert row names into column
                    cmt<-cbind(rownames(cmt),cmt)
                    #names(cmt)[1]<-"variable"
                    
                    boxplot(countrymatrix,notch=F,
                            range=0,whisklty=2,staplelty=2,boxlty=0,
                            ylim=c(tmin,tmax))
                    
                }else{
                    
                    boxplot(curmatrix,
                            notch=F,
                            range=1.0,
                            whisklty=0,
                            staplelty=0,
                            ylim=c(tmin,tmax),
                            xlab="")
                    
                    for (i in c(1:(topn+1))){
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
            #if(curdata=="ief"){mytext2<-"[ Tg CH"[4] ~~ head^{-1} ~~ yr^{-1} ~"]"}
            #source("text_elements.txt")
            par(omd=c(0,xstt,ystt,yhea))
            par(mar=c(marbot,marlef,martop,marrig))
            par(fig=c(0,1,0,1),new=T)
            plot(0, xlim=c(0, 1), 
                 #ylim=c(0,1), 
                 ylim=c(uabs+0.04*oabs,0.96*oabs), 
                 axes=F, xlab="", ylab="", type="n")
            
            text(0.1,tmin+(tmax-tmin)/2,adj=c(0.5,0.5),cex=1.5,textpar,las=3,srt=90,font=2)
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
            avshare<-0.68
            legcex<-1.2
            minlow<-marbot*par("cxy")[2]
            maxhig<-0.9
            
            for (i in c(1:ncountries)){
                
                mytextav <- paste(finshares[i],"%",sep="")
                mytextadef <- "(a.b%/c.d%)"
                
                hig=minlow+(i+0)*(maxhig-minlow)/(ncountries+1)
                mid=minlow+(i-0.5)*(maxhig-minlow)/(ncountries+1)
                low=minlow+(i-1)*(maxhig-minlow)/(ncountries+1)
                if(curdata!="ief"){
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
                text(recside*1.2,mid,finnames[i],cex=legcex,adj=0)
                text(avshare,mid,mytextav,cex=legcex,adj=1)
                if (curfocus=="trend") text(avshare+0.03,mid,mytextadef,cex=legcex-0.1,adj=0)
            }
            
            if (curdata!="ief" ) text(0,1.00,"Average share of country",cex=legcex,adj=0,font=2)
            if (curdata=="ief" && curfocus=="value") text(0,1.00,"Average IEF of country",cex=legcex,adj=0,font=2)
            if (curdata=="ief" && curfocus=="trend") text(0,1.00,"Average trend of country",cex=legcex,adj=0,font=2)
            if (curdata=="adem" && curfocus=="trend") text(0,0.95,"(contribution AD/EF)",cex=legcex,adj=0,font=2)
            if (curdata=="ief" && curfocus=="value") text(0,0.95,"% from EU28+IS average",cex=legcex,adj=0,font=2)
            if (curdata=="ief" && curfocus=="trend") text(0,0.95,"interannual change > 3%",cex=legcex,adj=0,font=2)
            
            mid=minlow+(ncountries+1-0.5)*(maxhig-minlow)/(ncountries+1)
            #print(paste(i,low,mid,hig))
            
            points(x=recside/2,y=mid,pch=21,bg="black",col="red",cex=1.5,lwd=2)
            text(recside*1.2,mid,"EU28+IS",cex=legcex,adj=0,font=2)
            #text(avshare,mid,"100%",cex=legcex,adj=1)
            if (curfocus=="trend") text(avshare+0.03,mid,mytextadef,cex=legcex-0.1,adj=0,font=2)
            
            
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
            
            if (curdata=="adem" && curfocus=="value") {mtexttitle<-"Trend in EU28+IS"}
            if (curdata=="adem" && curfocus=="trend") {mtexttitle<-"Annual changes in EU28+IS"}
            if (curdata=="ief" && curfocus=="value") {mtexttitle<-"Range of values in EU28+IS"}
            if (curdata=="ief" && curfocus=="trend") {mtexttitle<-"Range in annual changes across EU28+IS"}
            if (curdata=="ief" && curfocus=="countries") {mtexttitle<-"Range values over time"}
            mtexttitle<-paste(textcat," (",curgases,"): ",mtexttitle,sep="")
            
            par(omd=c(0,1,yhea,1))
            par(mar=c(0,0,0,0)) #bot,lef,top,rig
            par(fig=c(0,1,0,1),new=T)
            plot(0, xlim=c(0, 1), ylim=c(0, 1), axes=F, xlab="", ylab="", type="n")
            text(0.5,0.5,adj=0.5,mtexttitle,cex=1.6)

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
            #dev.off()
            dev.off(dev.cur())
        }
    }
    if(substr(curtasks[icurtasks],0,3)=="cat"){starttasks=1}
}


