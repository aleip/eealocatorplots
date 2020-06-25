selmeasure<-"Emissions"
trendmaincatsMS <- agrigen[agrigen$measure==selmeasure,]
trendmaincatsMS <- trendmaincatsMS[trendmaincatsMS$gas!="NMVOC",]
trendmaincatsMS <- merge(trendmaincatsMS, gwps, by='gas')
trendmaincatsMS <- trendmaincatsMS[, (years) := .SD * gwp, .SDcols=years]
trendmaincatsMS$unit<-"kt CO2 equivalent"

selcountries <- curcountries[variable==eusubm & value==1 & code3!=eusubm]$code3
trendmaincatsEUC<-trendmaincatsMS[party==eusubm,]
trendmaincatsMS <-trendmaincatsMS[party%in%selcountries,]


trenddetcatsMS<-agridet[agridet$measure==selmeasure,]
trenddetcatsMS<-trenddetcatsMS[trenddetcatsMS$gas!="NMVOC",]
trenddetcatsMS<- merge(trenddetcatsMS, gwps, by='gas')
trenddetcatsMS <- trenddetcatsMS[, (years) := .SD * gwp, .SDcols=years]
trenddetcatsMS$unit<-"kt CO2 equivalent"

trenddetcatsMS <-trenddetcatsMS[party%in%selcountries,]

trendmaincatsMS<-as.data.frame(trendmaincatsMS)
trenddetcatsMS<-as.data.frame(trenddetcatsMS)

rankcategories<-function(testkey,y){
    testkey[,paste0(y,"rank")]<-frankv(x=abs(testkey[,y]),order=-1,na.last=TRUE,ties.method="first")
    testkey<-testkey[order(testkey[,paste0(y,"rank")]),]
    
    testkey[,paste0(y,"cum")]<-sapply(1:nrow(testkey),function(x) sum(testkey[1:x,y],na.rm=TRUE))
    return(testkey)
}


tmainsec<-unique(trendmaincatsMS$sector_number)
tdetsec<-unique(trenddetcatsMS$sector_number)

for (k in tmainsec){
    sel <-  trendmaincatsMS$sector_number==k 
    save(tmainsec, trendmaincatsMS, sel, file="a.rdata")
    assign(paste0("trend",k),trendmaincatsMS[sel,])
}
for (k in tdetsec){
    sel <-  trenddetcatsMS$sector_number==k 
    assign(paste0("trend",k),trenddetcatsMS[sel,])
}


f1<-c("sharefrom","party","sector_number","category","gas")
for (i in c("02","05","10","A")){
    f1<-c(f1,
          paste0("share",i),
          paste0("trend",i,"abs"),
          paste0("sumtrend",i),
          paste0("trend",i,"rel"),
          paste0("share",i,"rank"),
          paste0("share",i,"cum"))
}
f1<-c(f1,years)
runi<-0
for (j in paste0("trend",c("maincatsMS","maincatsEUC",union(tmainsec,tdetsec)))){
    test<-as.data.table(get(j))
    test$sharefrom<-j
    
    ncls <- paste0("y", c("last", "02","05","10","A"))
    test <- test[, (ncls) := .SD, .SDcols=c(lastyear,
                                            lastyear2, 
                                            years[length(years)-5], 
                                            years[length(years)-10], 
                                            firstyear)]
    
    ocls <- paste0("y", c("02","05","10","A"))
    
    # Absolute trends
    tcls <- paste0("trend", c("02","05","10","A"), "abs")
    test <- test[, (tcls) := ylast-.SD, .SDcols=ocls]
    ncls <- paste0("trend", c("02","05","10","A"), "rel")
    test <- test[, (ncls) := ylast/.SD, .SDcols=ocls]
    
    # Shares of trend over sum of absolute trends
    ncls <- paste0("share", c("02","05","10","A"))
    
    # Sum over the absolute trends
    scls <- paste0("sumtrend", c("02","05","10","A"))
    
    
    test <- test[, (scls) := lapply(.SD, sum, na.rm=TRUE), .SDcols=tcls]
    for (i in c("02","05","10","A")){
        si<-paste0("sumtrend",i)
        sh<-paste0("share",i)
        ti<-paste0("trend", i, "abs")
        test <- test[, tempcol := .SD, .SDcols=si]
        test <- test[, (sh) := .SD/tempcol, .SDcols=ti, by=1:nrow(test)]
        
    }        
    f2<-intersect(names(test),f1)
    test<-test[,f2, with=FALSE]
    if(runi==0){trendshares<-test}else{trendshares<-rbind(trendshares,test)}
    runi<-runi+1
}

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
                  "\n# - trendmaincatsEUC: gives shares for EUC total trend in agriculture by *sector* (main categories - not very detailed)",
                  "\n# - trendmaincatsMS:  gives shares for EUC total trend in agriculture by *sector and MS* this is the longest table and probably most interesting one - check important indivual trends. filtering by country also possible",
                  "\n# - trend3.A - trend3.A.1 etc : gives shares for EU total trend in category by *MS* - can be used to analyse findings from before. can be used hierarchically first 3A then check 3A1 etc."),
           con)
write.csv(trendshares,con)
close(con)

cols <- intersect(c( "sector_number", "gas", "share02", "trend02abs", "trend02rel", "share02cum", "shareA", "trendAabs", "trendArel", firstyear, lastyear), names(trendshares))
trsh_EUC <- trendshares[trendshares$party == "EUC" & !grepl("land", trendshares$sector_number), cols, with=FALSE]
trsh_EUC$shr <- unlist(lapply(trsh_EUC[, lastyear,  with=FALSE], function(x) round((x / sum(trsh_EUC[, lastyear, with=FALSE])), 2)))
trsh_EUC <- trsh_EUC[order(trsh_EUC$sector_number), ]
trsh_EUC_tbl <- trsh_EUC[, c("sector_number", "gas", "shr", "shareA", "share02")]
trsh_EUC_tbl$shareA <- round(trsh_EUC_tbl$shareA, 2)
trsh_EUC_tbl$share02 <- round(trsh_EUC_tbl$share02, 2)

trsh_EUC_tbl_n2 <- paste0("Contribution to total agricultural emissions (", lastyear, ")")
trsh_EUC_tbl_n3 <- paste0("Share of trend ", firstyear, "-", lastyear)
trsh_EUC_tbl_n4 <- paste0("Share of trend ", lastyear2, "-", lastyear)

names(trsh_EUC_tbl) <- c("Emission category", "Gas",trsh_EUC_tbl_n2,trsh_EUC_tbl_n3, trsh_EUC_tbl_n4)
row.names(trsh_EUC_tbl) <- NULL


cattle_pop <- as.vector(unlist(allagri[allagri$meastype == "POP" & allagri$party == "EUC" & allagri$category == "Cattle", c(firstyear, lastyear), with=FALSE]))
inorg_fert_AD <- as.vector(unlist(allagri[allagri$party == "EUC" & allagri$meastype == "AD" & allagri$sector_number == "3.D.1.1", c(firstyear, lastyear), with=FALSE]))
org_fert_AD   <- as.vector(unlist(allagri[allagri$party == "EUC" & allagri$meastype == "AD" & allagri$sector_number == "3.D.1.2", c(firstyear, lastyear), with=FALSE]))


trsh_EUC <- as.data.frame(trsh_EUC)
trsh_EUC_tbl <- as.data.frame(trsh_EUC_tbl)

