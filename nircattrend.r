selmeasure<-"Emissions"
trendmaincatsMS<-agrigen[agrigen$measure==selmeasure,]
trendmaincatsMS<-trendmaincatsMS[trendmaincatsMS$gas!="NMVOC",]
#trendmaincatsMS<-trendmaincatsMS[!grepl("^3.E",trendmaincatsMS$sector_number),]
gasf<-sapply(1:nrow(trendmaincatsMS),function(x) gwps[which(gases==trendmaincatsMS$gas[x])])
trendmaincatsMS[,years]<-(trendmaincatsMS[,years])*gasf
trendmaincatsMS$unit<-"kt CO2 equivalent"

eusel<-trendmaincatsMS$party==eusubm
trendmaincatsEUC<-trendmaincatsMS[eusel,]
trendmaincatsMS<-trendmaincatsMS[!eusel,]


trenddetcatsMS<-agridet[agridet$measure==selmeasure,]
trenddetcatsMS<-trenddetcatsMS[trenddetcatsMS$gas!="NMVOC",]
#trenddetcatsMS<-trenddetcatsMS[!grepl("^3.E",trenddetcatsMS$sector_number),]
gasf<-sapply(1:nrow(trenddetcatsMS),function(x) gwps[which(gases==trenddetcatsMS$gas[x])])
trenddetcatsMS[,years]<-(trenddetcatsMS[,years])*gasf
trenddetcatsMS$unit<-"kt CO2 equivalent"
eusel<-trenddetcatsMS$party==eusubm
trenddetcatsMS<-trenddetcatsMS[!eusel,]



rankcategories<-function(testkey,y){
    testkey[,paste0(y,"rank")]<-frankv(x=abs(testkey[,y]),order=-1,na.last=TRUE,ties.method="first")
    testkey<-testkey[order(testkey[,paste0(y,"rank")]),]
    
    testkey[,paste0(y,"cum")]<-sapply(1:nrow(testkey),function(x) sum(testkey[1:x,y],na.rm=TRUE))
    return(testkey)
}


tmainsec<-unique(trendmaincatsMS$sector_number)
tdetsec<-unique(trenddetcatsMS$sector_number)
for (k in tmainsec){
    test<-filter(trendmaincatsMS,sector_number==k)
    assign(paste0("trend",k),test)
}
for (k in tdetsec){
    test<-filter(trenddetcatsMS,sector_number==k)
    assign(paste0("trend",k),test)
}


runi<-0
for (j in paste0("trend",c("maincatsMS","maincatsEUC",union(tmainsec,tdetsec)))){
    test<-get(j) 
    test$sharefrom<-j
    for (i in c("02","05","10","A")){
        si<-paste0("share",i)
        ti<-paste0("trend",i,"abs")
        tr<-paste0("trend",i,"rel")
        
        if(i=="02") yy<-lastyear2
        if(i=="05") yy<-years[length(years)-5]
        if(i=="10") yy<-years[length(years)-10]
        if(i=="A") yy<-firstyear
        
        
        test[,tr]<-test[,lastyear]/test[,yy]
        test[,ti]<-test[,lastyear]-test[,yy]
        test[,si]<-test[,ti]/sum(test[,ti],na.rm=TRUE)
        test<-rankcategories(test,si)
        
    }
    if(runi==0){trendshares<-test}else{trendshares<-rbind(trendshares,test)}
    runi<-runi+1
}
f1<-c("sharefrom","party","sector_number","category","gas")
for (i in c("02","05","10","A")){
    f1<-c(f1,paste0("share",i),paste0("trend",i,"abs"),paste0("trend",i,"rel"),paste0("share",i,"rank"),paste0("share",i,"cum"))
}
f1<-c(f1,years)
f1<-c(f1,setdiff(names(trendshares),f1))
trendshares<-trendshares[,f1]

tdir<-paste0(invloc,"/trends")
if(!dir.exists(tdir)){dir.create(tdir)}

con<-file(paste0(tdir,"/trendshares~",curdate(),".csv"),open="wt")
writeLines(paste0("\n#",
                  "\n# share02 - Last two years trend",
                  "\n# share05 - Last year compared to  5 years ago (e.g. 2016 vs 2011)",
                  "\n# share10 - Last year compared to 10 years ago (e.g. 2016 vs 2006)",
                  "\n# shareA  - Last year compared to 1990",
                  "\n#",
                  "\n# trend02abs (05-10-A) - Absolute change in time series (MS or EUC) last year compared to other year. Negative decrease - Positive increase",
                  "\n# trend02rel (05-10-A) - Relative change in time series (MS or EUC) last year compared to other year. Larger than 1 = increase - Smaller than 1 = decrease",
                  "\n# share02    (05-10-A) - Share of the trend in MS relative to EUC trend. ",
                  "\n#                      Example: decrease of 200 in MS but EUC increased by 50 - then share = -2 (twice as high in other direction  -- )",
                  "\n#                               increase of 25 in MS with EUC increase of 50  - then share = +0.5",
                  "\n# share02rank ... Importance of the trend in MS for overall trend (example MS contribution to EUC trend). Ranked by absolute magnitude - large negatives and positives first",
                  "\n#",
                  "\n# Filter by column sharefrom",
                  "\n# - trendmaincatsEUC: gives shares for EUC total trend in agriculture by *sector* (main categories, not very detailed)",
                  "\n# - trendmaincatsMS:  gives shares for EUC total trend in agriculture by *sector and MS* this is the longest table and probably most interesting one - check important indivual trends. filtering by country also possible",
                  "\n# - trend3.A, trend3.A.1 etc : gives shares for EU total trend in category by *MS* - can be used to analyse findings from before. can be used hierarchically first 3A then check 3A1 etc."),
           con)
write.csv(trendshares,con)
close(con)


