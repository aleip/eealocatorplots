if(nrow(growthcheck)>0){
    
    filnam<-paste0(invloc,"/checks/timeseries/checks",cursubm,"growthcheck.csv")
    con <- file(filnam, open="wt")
    writeLines(paste0("# File created by EU-GIRP v.",eugirp.version," on ",figdate), con)
    writeLines(paste0("# List of trend outliers. Outliers were identified with the following criteria"), con)
    writeLines(paste0("# (1) Growth Rate outside of the '95% confidence interval'. I.e. the growth rate was outside the range of median +/- 1.953 x (median-75percentile/25percentile) of the growth rates during the time period for this variable and country"), con)
    writeLines(paste0("# (2) Growth Rate larger than ", 1+ming," or smaller than ",1-ming), con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    writeLines(paste0("# years.growth: growth rates calculated as y{t}/y{t-1}"), con)
    growthcheck<-growthcheck[,co]
    write.csv(growthcheck,con)
    close(con)
}
if(nrow(paramcheck)>0){
    #Check of systematic errors
    
    filnam<-paste0(invloc,"/checks/countryoutliers/checks",cursubm,"countryoutliersserious.csv")
    con <- file(filnam, open="wt")
    writeLines(coutheader, con)
    writeLines(coutlexp1, con)
    if(trendoutlmethod==2)coutlexp2<-paste0(outlmethod2,coutlexp2)
    if(trendoutlmethod==3)coutlexp2<-paste0(outlmethod3,coutlexp2)
    writeLines(coutlexp2, con)
    writeLines(signexp1, con)
    writeLines(colexpl1, con)
    if(trendoutlmethod==2)writeLines(whisksexpl,con)
    writeLines(lulimexpl, con)
    writeLines(colexpl2, con)
    writeLines(colexpl3, con)
    write.csv(paramV,con)
    close(con)
    
    # Generate the plots to illustrate the issues
    #plotparamcheck<-1
    #fignames<-character()
    
    # Add plots 
    #source("eugirpD.2_iefplots.r")

    #fignamesc<-paste0(gsub(paste0(issuedir,"countryoutliers/"),"=HYPERLINK(\"",fignames),"\")")
    #paramcheck$plot<-fignamesc
    
    n<-names(paramcheck[!names(paramcheck)%in%c("plot",testfields)])
    oc<-c("plot",n[1:(which(n=="years"))],testfields,n[(which(n=="years")+1):length(n)])
    paramcheck<-paramcheck[,oc[oc%in%names(paramcheck)]]
    paramcheck<-paramcheck[order(paramcheck$sector_number,paramcheck$category,paramcheck$meastype),]
    #n1<-c("cursubm","party","sector_number","meastype","unit","category","value","correction"))
    #n4<-names(paramcheck[!names(paramcheck)%in%c(n1,"std")])
    #paramcheck<-paramcheck[,oc]
    writeissuelist(paramcheck,paste0(filoutliers,"list_checked.csv"))
}
