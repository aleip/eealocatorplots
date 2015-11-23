curuid<-plotmeas$variableUID[imeas]
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
