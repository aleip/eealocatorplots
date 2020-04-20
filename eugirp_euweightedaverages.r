change2datatables <- TRUE
change2datatables <- FALSE
if(change2datatables){
  
  joinADs2pars <- agrimeas[, .(meastype, gas, sector_number, category, type, variableUID)]
  
  #print(paste0("x<-",x,";sec<-",sec,";mea",mea,";cat<-",cat))
  
  # Use "Total Biomass burned [kt dm]" for IEF CH4 and N2O in Table 3.F
  joinADs2pars <- joinADs2pars[grepl("^3.F",sector_number) & meastype=="IEF", avail := 'AD']
  
  # Use "Area burned [k ha/yr]" for YIELD "Biomass available [t dm/ha] in Table 3.F
  joinADs2pars <- joinADs2pars[grepl("^3.F",sector_number) & meastype=="YIELD", avail := 'AREA']
  
  # Use "Crop production [t]" for parameters in 'Additional information' in Table 3.F
  joinADs2pars <- joinADs2pars[grepl("^3.F",sector_number) & 
                                 meastype%in%c("DM","FracBURN","FracOXIDIZED","Combustion","RatioResCrop"), avail := 'PROD']
  
  # Indirect emissions in Table 3.B.2
  joinADs2pars <- joinADs2pars[grepl("^3.B.2.5",sector_number) & variableUID=="47100CA3-9371-4944-B302-BD76A154B0F4", avail := 'Nvol']
  joinADs2pars <- joinADs2pars[grepl("^3.B.2.5",sector_number) & variableUID=="C548A926-2825-4F66-A6FE-DA55F429CB29", avail := 'Nleach']
  # 3.B.2.5 N2O Emissions per MMS
  joinADs2pars <- joinADs2pars[grepl("3.B.2.5 N2O Emissions per MMS",sector_number), avail := 'NEXC']
  
  # Solid waste disposal
  joinADs2pars <- joinADs2pars[grepl("5.[AB].",sector_number), avail := 'AD']
  # Waste incineration
  joinADs2pars <- joinADs2pars[grepl("Clinical Waste",category) & type=="Biogenic", avail := 'ADbio']
  joinADs2pars <- joinADs2pars[grepl("Clinical Waste",category) & type=="Non-biogenic", avail := 'ADnbio']
  # Waste Water treatment and discharge
  joinADs2pars <- joinADs2pars[grepl("5.D.",sector_number) & gas=="CH4", avail := 'ADORG']
  joinADs2pars <- joinADs2pars[grepl("5.D.",sector_number) & gas=="N2O", avail := 'ADEFFLUENT']
}

fassign <- gsub("clean.RData", "assignad2par", rdatallem)
if(file.exists(fassign)){
  
  # this part takes quite long and need to be improved to work faster
  # for the moment run per submissions it once and then get it back from file.
  load(file = fassign)
}else{
  
  selectadmeasures<-function(request,M,A,meas2sum,x){
    avail<-meas2sum[!meas2sum%in%c("EM","NEXC")]
    sec<-A$sector_number[x]
    cat<-A$category[x]
    cla<-A$classification[x]
    sou<-A$source[x]
    tar<-A$target[x]
    mea<-A$meastype[x]
    gas<-A$gas[x]
    typ<-A$type[x]
    uid<-A$variableUID[x]
    uni<-A$unit[x]
    
    #print(paste0("x<-",x,";sec<-",sec,";mea",mea,";cat<-",cat))
    # Use "Total Biomass burned [kt dm]" for IEF CH4 and N2O in Table 3.F
    if(grepl("^3.F",sec) & mea=="IEF") avail<-"AD"
    # Use "Area burned [k ha/yr]" for YIELD "Biomass available [t dm/ha] in Table 3.F
    if(grepl("^3.F",sec) & mea=="YIELD") avail<-"AREA"
    # Use "Crop production [t]" for parameters in 'Additional information' in Table 3.F
    if(grepl("^3.F",sec) & mea%in%c("DM","FracBURN","FracOXIDIZED","Combustion","RatioResCrop")) avail<-"PROD"
    # Indirect emissions in Table 3.B.2
    if(grepl("^3.B.2.5",sec) & uid=="47100CA3-9371-4944-B302-BD76A154B0F4") avail<-"Nvol"
    if(grepl("^3.B.2.5",sec) & uid=="C548A926-2825-4F66-A6FE-DA55F429CB29") avail<-"Nleach"
    # 3.B.2.5 N2O Emissions per MMS
    if(grepl("3.B.2.5 N2O Emissions per MMS",sec)) avail<-"NEXC"
    # Solid waste disposal
    if(grepl("5.[AB].",sec)) avail<-"AD"
    # Waste incineration
    if(grepl("Clinical Waste",cat) & typ=="Biogenic") avail<-"ADbio"
    if(grepl("Clinical Waste",cat) & typ=="Non-biogenic") avail<-"ADnbio"
    # Waste Water treatment and discharge
    if(grepl("5.D.",sec) & gas=="CH4") avail<-"ADORG"
    if(grepl("5.D.",sec) & gas=="N2O") avail<-"ADEFFLUENT"
    
    
    measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                         & M$sector_number==sec & M$category==cat & M$classification==cla 
                                         & M$source==sou & M$target==tar]))
    
    if(mea == "CLIMA" & request == "meastype") measOK <- "CLIMA"   #xavi20180221 
    if(mea == "CLIMA" & request == "unit") measOK <- "%"           #xavi20180221
    if(mea == "CLIMA" & request == "variableUID") measOK <- uid    #xavi20180221
    if(mea == "MCF" & request == "meastype") measOK <- "MCF"     #xavi20180221 
    if(mea == "MCF" & request == "unit") measOK <- uni           #xavi20180221
    if(mea == "MCF" & request == "variableUID") measOK <- uid    #xavi20180221
    
    # In Tables 3.B. ignore source (MMS) and classification 
    if(length(measOK)==0 & grepl("^3.B",sec)){
      measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                           & M$sector_number==sec & M$category==cat
                                           & M$target==tar]))
    }
    
    
    # In Tables 3.B. not all ADs are given - they are the same as in Table 3.A
    #                ignore source (MMS) and classification (Enteric vs. )
    if(length(measOK)==0 & grepl("^3.B",sec)){
      sec<-gsub("3.B.[12]","3.A",sec)
      measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                           & M$sector_number==sec & M$category==cat 
                                           & M$target==tar]))
      #print(measOK)
      if(length(measOK)==0) {
        cat<-gsub("yrs","years",cat)
        measOK<-as.vector(unique(M[,request][M$meastype %in% avail 
                                             & M$sector_number==sec & M$category==cat 
                                             & M$target==tar]))
        if(length(measOK)==0) {
          tms<-paste0("measOK<-",measOK,"x<-'",x,"';sec<-'",sec,"';mea<-'",mea,"';uid<-'",uid,"';cat<-'",cat,"';cla<-'",cla,"';sou<-'",sou,"';tar<-'",tar,"'")
          print(tms)
          stop()
        }
      }
    }
    
    
    # For cat 3.B.2 ADs might not be give ... use those for 3.A instead
    if(length(measOK)==0){
      if(grepl("^3.B.2.1",sec)){
        sec<-gsub("3.B.2","3.A",sec)
        uid<-(unique(M[,"variableUID"][M$meastype=="AD" & M$sector_number==sec]))
        measOK<-as.vector(unique(M[,request][M$variableUID==uid]))
      }
      
      if(grepl("^3.D.1.3",sec)) {
        if(request=="variableUID")measOK<-paste0("AD for",sec," (N Deposited by Grazing Animals) needs to be calculated from Table B(b)")
        if(request=="meastype")measOK<-"-"
      }
      if(grepl("^3.D.AI",sec)) {
        if(request=="variableUID")measOK<-paste0("Fractions ",sec," needs to be checked")
        if(request=="meastype")measOK<-"-"
      }
      if(grepl("^3.B.2.5 N2O Emissions per MMS",sec)) {
        if(request=="variableUID")measOK<-paste0("Total N handled in ",sec," needs to be calc&checked")
        if(request=="meastype")measOK<-"-"
      }
    }else if(length(measOK)>1){
      measOK<-c(length(measOK),measOK)
      if(uni=="t/unit"){
        measOK<-c(measOK,"variable type")
      }else{
        View(A[x,])
        print(measOK)
        print(paste0("meastype=",mea))
        tms<-paste0("x<-'",x,"';sec<-'",sec,"';mea<-'",mea,"';uid<-'",uid,"';cat<-'",cat,"';cla<-'",cla,"';sou<-'",sou,"';tar<-'",tar,"'")
        print(tms)
        stop()
      }
    }
    
    return(paste0(measOK,collapse=" "))
  }
  
  #allagri<-alldata[grepl("^3",alldata$sector_number),]
  calcmeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
  #measname<-as.data.frame(measname)
  #xavi20180221: measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
  
  measures2sum<-allagri[allagri$meastype %in% meas2sum,]
  listofmeasuresnotconsidered<-calcmeas[!calcmeas$meastype %in% c(meas2sum,meas2popweight,meas2clima,meas2mcf),]
  
  # Set up table with infor AD to link with parameter
  #xavi20180221: assignad2par<-unique(calcmeas[calcmeas$meastype %in% meas2popweight,!(names(calcmeas)%in%c("method","measure"))])
  assignad2par<-unique(calcmeas[calcmeas$meastype %in% c(meas2popweight,meas2clima,meas2mcf),!(names(calcmeas)%in%c("method","measure")), with=FALSE])
  assignad2par$adpars<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("meastype",as.data.frame(calcmeas),as.data.frame(assignad2par),meas2sum,x)))
  assignad2par$adunit<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("unit",as.data.frame(calcmeas),as.data.frame(assignad2par),meas2sum,x)))
  assignad2par$aduids<-unlist(lapply(c(1:nrow(assignad2par)),function(x)
    selectadmeasures("variableUID",as.data.frame(calcmeas),as.data.frame(assignad2par),meas2sum,x)))
  namesassignad2par<-c("meastype","gas","unit","sector_number","adpars","adunit",
                       "category","classification","source","target","option","variableUID","aduids")
  assignad2par<-assignad2par[,namesassignad2par, with=FALSE]
  measures2wei<-calcmeas[calcmeas$variableUID %in% assignad2par$variableUID,]
  if(sum(duplicated(measures2wei$variableUID))>0)  measures2wei <- measures2wei[!is.na(measures2wei$meastype),]
  parameterswithoutADs<-(assignad2par[assignad2par$adunit=="",])
  
  save(assignad2par, measures2wei, measures2sum, listofmeasuresnotconsidered, file = fassign)
}
# CORRECTIONS

# 1. Autocorrections have an identified reason - update the data table
#xavi20180216: selection<-allagri$party%in%eu & allagri$meastype%in%meas2popweight
#              MCF should be weighted by VS excretion * share in climate zone * share in system
#              However as long as this is not implemented add to meas2popweight as a proxy

#selection<-allagri$party%in%eu & allagri$meastype%in%c(meas2popweight,meas2mcf,meas2clima)
#calceu<-allagri[!selection,]
calceu <- allagri

if(exists("autocorrections")){
  
  message("\nApply autocorrections to values")
  autoc <- copy(as.data.table(autocorrections))
  autoc <- autoc[!party %in% eu, c("party","variableUID", "autocorr", years), with=FALSE]
  setnames(autoc, years, paste0("a", years))
  
  # If rerun remove column 'autocor' first
  calceu <- calceu[, -"autocorr", with=FALSE]
  autoc <- merge(calceu,autoc,by=c("party","variableUID"),all=TRUE)
  
  # Replace original values with the autocorrected ones
  autoc <- autoc[!is.na(autocorr), (years) := .SD, .SDcols=paste0("a", years)]
  autoc <- autoc[, .SD, .SDcols = setdiff(names(autoc), paste0("a", years))]
  
  calceu <- autoc  
    
}
#if(!is.null(keepNORout)){
#  calceu <- calceu[calceu$party != "NOR", ]
#}
#allagri<-calceu
# 2. 'Unidentified' outliers are removed for calculation, but data table remains untouched
if(exists("paramcheck")){
    corcalceu<-subset(paramcheck,select=c("party","variableUID","correction"))
    corcalceu<-corcalceu[corcalceu$variableUID != "",]
  
    # If rerun remove column 'autocor' first
    calceu <- calceu[, -"correction", with=FALSE]
    calceu <- merge(calceu,corcalceu,by=c("party","variableUID"),all=TRUE)
    
    calceu$correction[is.na(calceu$correction)]<-1
    calceu$correction[calceu$meastype=="Milk"&calceu$party=="LUX"]<-0
    selection2<-calceu$correction==0
    
    if(!is.null(keepNORout)){
      calceu <- calceu[calceu$party != "NOR", ]
    }
    calceucor<-calceu

    # Do not use the 'correction==0' values for the EU average, but keep them in the data...
    calceucor[selection2,years]<-NA
    
}
#allagri<-calceu
calceu <- calceucor 

dtnames <- names(calceu)
calceu <- calceu[! party %in% eu]

# For multiplication melt data
b <- melt.data.table(calceu, measure.vars = years, variable.name = "years")

# Create data table with variables to weight for aggregation and popolation data
m2w <- c(measta2weight, meastb12weight, meastb22weight)
m2s <- setdiff(b$meastype, m2w)
b <- b[, weigh := ifelse(meastype%in% m2w, 1, 0)]

# Olny emissions - admeas <- 0
b <- b["3"== sector_number, admeas := "0"]
b <- b["3.1"== sector_number, admeas := "0"]
b <- b["3.E"== sector_number, admeas := "0"]

b <- b[grepl("^3.[AB]", sector_number), admeas := "POP"]
# 3.B.2.5 N2O Emissions per MMS
b <- b[grepl("^3.B.2.5", sector_number), admeas := "NEXC"]
# Indirect emissions in Table 3.B.2
b <- b[grepl("^3.B.2.5", sector_number) & variableUID=="47100CA3-9371-4944-B302-BD76A154B0F4", admeas := "Nvol"]
b <- b[grepl("^3.B.2.5", sector_number) & variableUID=="C548A926-2825-4F66-A6FE-DA55F429CB29", admeas := "Nleach"]
b <- b[grepl("^3.[C]", sector_number), admeas := "AREA"]
# Use "Total Biomass burned [kt dm]" for IEF CH4 and N2O in Table 3.F
b <- b[grepl("^3.[DFGHIJ]", sector_number), admeas := "AD"]
# Use "Area burned [k ha/yr]" for YIELD "Biomass available [t dm/ha] in Table 3.F
b <- b[grepl("^3.[F]", sector_number) & meastype=="YIELD", admeas := "AREA"]
# Use "Crop production [t]" for parameters in 'Additional information' in Table 3.F
b <- b[grepl("^3.[F]", sector_number) & meastype%in%c("DM","FracBURN","FracOXIDIZED","Combustion","RatioResCrop"), admeas := "PROD"]

# Population data are sometimes missing for some sectors, but should be equal
# Therefore, the mean of all existing secor_numbers is used
admeas <- c("POP", "AREA", "AD", "PROD", "Nvol", "Nleach", "NEXC")
pop <- melt.data.table(calceu[meastype %in% admeas], measure.vars = years, variable.name = "years", value.name = "pop")
pop <- pop[, .(pop=mean(pop)), by=.(sector_number, category, meastype, party, years)]
setnames(pop, "meastype", "admeas")
childs <- merge(b, pop, by=c("sector_number", "category", "admeas", "party", "years"), all.x = TRUE)
childs <- childs[, total := value *  pop]
uniquechilds <- unique(childs[, .(sector_number, category, meastype, variableUID)])
write.xlsx(childs[years==1990], file="euweightedaverages_childs1990.xlsx")
write.xlsx(uniquechilds, file="euweightedaverages_ads1990.xlsx")

sumeus <- function(child, eusel = "EUC"){
  
  cntr <- country4sub[, .SD, .SDcols=c("code3", eusel)]
  setnames(cntr, names(cntr)[2], "eusel")
  cntr <- setdiff(cntr[eusel==1]$code3, eusel)
  parent <- child[party %in% cntr]
  # If there is a missing value set also population to zero to not 
  # bias the results
  parent <- parent[value==0, pop := 0]
  parent <- parent[, .(value=sum(value, na.rm=TRUE), 
                       pop=sum(pop, na.rm=TRUE), 
                       total=sum(total, na.rm = TRUE)), 
                   by=setdiff(names(parent), 
                              c("party", "method", "value", "pop", "total", "notation", 
                                "autocorr", "correction"))]
  parent <- parent[, value := ifelse(weigh==1, ifelse(pop>0, total/pop, 0), value)]
  parent$notation <- ""
  parent$correction <- ""
  parent$autocorr <- ""
  parent$method <- ""
  parent$party <- eusel
  
  # Calculate total ParPop = Value*Pop
  dfrom <- paste0(paste(setdiff(names(parent), c("weigh", "pop", "total", "value", "years")), collapse = " + "), " ~ years")
  parentsy <- dcast.data.table(parent, as.formula(dfrom), value.var="value")
  
  return(parentsy)
  
}

parkp <- sumeus(child = childs, eusel = "EUC")
par28 <- sumeus(child = childs, eusel = "EU28")
pareu <- rbind(parkp, par28)

calceunew <- rbind(calceu, pareu, fill = TRUE)
setcolorder(calceunew, dtnames)
eukp <- parkp[order(sector_number, category, gas, party)]
eu28 <- par28[order(sector_number, category, gas, party)]
allagri <- calceunew[order(sector_number, category, gas, party)]

if(FALSE){
  
  # old code
  
  calceu <- merge(calceu, assignad2par[, .(variableUID, aduids)], by="variableUID", all.x=TRUE)
  calceu <- melt.data.table(calceu, measure.vars = years, variable.name = "years", value.name = "par", na.rm = TRUE)
  
  # Extract data table with Activity data
  advals <- allagri[variableUID %in% assignad2par$aduids]
  advals <- melt.data.table(advals, measure.vars = years, variable.name = "years", value.name = "ad", na.rm = TRUE)
  setnames(advals, "variableUID", "aduids")
  
  # Multiply activity data with variable values 
  # For those variables that don't need to be weighted this will give zero 
  # (as the 'value' caclulated above resulted in NA)
  calceu <- merge(calceu, advals[, .(aduids, party, years, ad)], 
                  by=c("party", "years", "aduids"), all.x=TRUE)
  calceu <- calceu[, value := par * ad]
  calceu <- calceu[value==0, ad := 0]
  #calceu <- calceu[, correction := as.numeric(correction)]
  
  # Calculate EU sums
  ceukp <- setdiff(country4sub[EUC==1]$code3, "EUC")
  ceu28 <- setdiff(country4sub[EU28==1]$code3, "EU28")
  calceukp <- calceu[party %in% ceukp]
  calceu28 <- calceu[party %in% ceu28]
  
  eukpsum <- calceukp[, .(value=sum(value, na.rm=TRUE), 
                          par=sum(par, na.rm=TRUE), 
                          ad=sum(ad, na.rm=TRUE)),
                      by = setdiff(names(calceu), 
                                   c(c("par", "ad", "value",
                                       "notation", "autocorr", "method", "party", "correction")))]
  eukpsum$party <- "EUC"
  eukpsum <- eukpsum[! meastype %in% meas2sum, par := value/ad]
  eu28sum <- calceu28[, .(value=sum(value, na.rm=TRUE), 
                          par=sum(par, na.rm=TRUE), 
                          ad=sum(ad, na.rm=TRUE)),
                      by = setdiff(names(calceu), 
                                   c(c("par", "ad", "value",
                                       "notation", "autocorr", "method", "party", "correction")))]
  eu28sum$party <- "EU28"
  eu28sum <- eu28sum[! meastype %in% meas2sum, par := value/ad]
  
  
  # Spread years again and combine with data table of individual countries
  dcastf <- paste0(paste(intersect(allfields, names(eukpsum)),collapse= " + "), " ~ years")
  eukpsum <- dcast.data.table(eukpsum, as.formula(dcastf), value.var = 'par')
  eu28sum <- dcast.data.table(eu28sum, as.formula(dcastf), value.var = 'par')
  
  calceunew <- rbind(calceucor[!party %in% eu], eukpsum, fill = TRUE)
  calceunew <- rbind(calceunew, eu28sum, fill = TRUE)
  eukpsum <- eukpsum[order(sector_number, category, gas, party)]
  eu28sum <- eu28sum[order(sector_number, category, gas, party)]
  allagri <- calceunew[order(sector_number, category, gas, party)]
  
}
