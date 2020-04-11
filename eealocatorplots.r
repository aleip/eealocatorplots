# EU-GIRP
# File eealocatorplots.r
# Main file required for the EU-GIRP
# Purpose: A: connect to eealocator file (run eealocator_csv.bash first)
#             and generate focus
#          B: loop over focus and prepare for current plot 
#             (selection of parameter, trend/value/country-plot, etc.)
#          C. run eugirp_nirplots.r for current plot
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>

# current inventory year
gdrive <- NULL
if(Sys.info()[4]=="L01RI1203587"){ #checks machine name
  adrian<-"D:/Users/leipadr/adrian/"
}else if(Sys.info()[4]=="D01RI1600881"){
  adrian<-"x:/Adrian/"
  adrian <- "\\\\s-jrciprap246p.jrc.it/dev/ghginventory/"
}else if(Sys.info()[4]=="D01RI1701864"){
  adrian<-"E:/ghginventory/"
}else if(Sys.info()[4]=="BARAGOON"){
  adrian<-"C:/dev/ghginventory/"
  gdrive <- "c:/Users/adrian/google/projects/eugirp/"
}else if(Sys.info()[4]=="MARSBL1BHL"){
  adrian<-"X:\\Agrienv\\ghginventory\\"
}else if(Sys.info()[4]=="D01RI1600850"){# Gema PC
  adrian<-"D:\\Users\\carmoge\\Documents\\GitHub\\"
}else if(Sys.info()[4]=="S-JRCIPRAP246P"){# New server
  adrian<-"D:\\dev\\ghginventory"
}else{
  adrian<-"C:/Adrian/"
}

locplots<-paste0(adrian,"/data/inventories/ghg/unfccc/eealocatorplots")           #!!!
if(Sys.info()[4]=="MARSBL1BHL")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="BARAGOON")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1701864")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1600881")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="D01RI1600850")locplots<-paste0(adrian,"/eealocatorplots")
if(Sys.info()[4]=="S-JRCIPRAP246P")locplots<-paste0(adrian,"/eealocatorplots")
setwd(locplots)
#setwd("\\\\s-jrciprap246p.jrc.it\\dev\\ghginventory\\eealocatorplots")   #Activate this to work with the data from the server remotely
                                                                          #Also activate remote2server (remote2server <- 1) in 'curplot.r'  
options(warn=0)
source("curplot.r")
options(warn=0) #warn=2 turns warnings into errors; set to 0 if this should be avoided
options(error=recover) #error=recover goes into debug mode
options(error=NULL) #error=recover goes into debug mode


### RESTART OPTION
restartatstep <- 2 #Clean version of step (restartatstep - 1) will be loaded
rm(restartatstep)

# PART A: Link with EEA locator tool ----
# A.1 Load eea-locator data (either from text file or from pre-processed Rdata file) ####
# Return:
# - alldata: data frame containing (almost) all EEA-locator data. Years are transposed to columns.
#            Some cleaning on uids has been done, data rows with irrelevant gases, GBE removed (GBK
#            kept), sector_numbers "-" removed. 
#            Infos (e.g. documentation boxes) and notation keys stored in separate files
# - allnotations: Notations used
# - allinfos: Information given, e.g. in documentation boxes
# - alldatanovalues
# - measname
#source("eugirpA.1_eealocator.r")
if(generatealldata==1){
  source("eealocator_generate_rdata.r")
  # Select gases ####
  gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O","NMVOC")
  select<-alldata$gas %in% gases2keep
  othergases<-read.table("plots_sec2othergases.txt")
  select2<-alldata$variableUID%in%unlist(othergases)
  select<-select | select2
  alldata<-alldata[select,]
  gases<-as.character(unique(alldata$gas))
  # Select years ####
  #years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
  select<-alldata$year %in% years2keep
  alldata<-alldata[select,]
  years<-as.character(unique(alldata$year))
  years<-sort(years)
  parties<-as.character(unique(alldata$party))
  classifications<-as.character(unique(alldata$classification))
  categories<-as.character(unique(alldata$category))
  sources<-as.character(unique(alldata$source))
  targets<-as.character(unique(alldata$target))
  options<-as.character(unique(alldata$option))
  types<-as.character(unique(alldata$type))
  units<-as.character(unique(alldata$unit))
  sectors<-as.character(unique(alldata$sector_number))
  uids<-as.character(unique(alldata$variableUID))
  
  submission_version<-as.character(unique(alldata$submission_version))    
  submission_year<-as.character(unique(alldata$submission_year))
  countries<-unique(alldata[,c("party","country_name")])

  # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # Generate data frame with one columns per year ----
  # and the value in the corresponding column ---
  # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  alldata<-subset(alldata,select=(! names(alldata) %in% c("country_name","submission_version","submission_year")))
  print("Generate data frame with one columns per year")
  #startt<-Sys.time()
  cols2leave<-paste(names(alldata)[!names(alldata)%in%c("year","value")],collapse="+")
  arrange<-as.formula(paste(cols2leave,"~ year"))
  
  alldata<-dcast.data.table(alldata,arrange,value.var="value")
  # measureacronyms --------------------------------------------------------------
  # Keep long text with the exception of 'measure' which is needed to identify
  # if it is an activity data, emissions, emission factor or parameter or other
  # alldata$measurelong<-alldata$measure
  #measureacronyms<-read.csv("metadim_row8measure.txt",stringsAsFactors=FALSE)
  #alldata<-subset(temp1,select=-dummy)
  print("measureacronyms")
  measureacronyms<-read.csv("measures_20150731.txt",stringsAsFactors=FALSE)
  alldata<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
  
  stepsdone<-1

  savelist<-c("stepsdone","savelist","alldata",
              "gases","years","parties", "countries", "classifications", "categories", "sources", "targets", 
              "options", "types", "units", "sectors", "uids","measureacronyms")
  write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_agri_s1.csv"),sep=",")
  write.table(alldata[grepl("^4",alldata$sector_number),],file=paste0(csvfil,"_lulucs_s1.csv"),sep=",")
  savestep(stepsdone, savelist)
  source("curplot.r")
  options(error=NULL) #error=recover goes into debug mode
  options(warn=0)
} 

# A.2 Clean animal type names ####
# Some animal types are given under 'allmethods', the options either 'sector_number' and/or 'allmethods'
# ---> put them all to 'sector-number': sector option animal
# Approach:
# - First separate agri from other data to speed up processing
# - Clean animal types in source file "eugirp_Acleananimaltypes.r"
# - Recombine with the other sectors

#  source("curplot.r")
if(stepsdone==1){
  print("Step 2: Generate list of measures and deal with animals")
  # Remove GBE (use GBK) and remove Island ####
  
  alldata$datasource<-"nir"
  source("eugirpA.2_meastype.r")
  sel <- alldata$meastype%in%c("PROD", "DM", "FracBURN", "FracOXIDIZED", "RatioResCrop")
  alldata[sel & sector_number=="" &grepl("[Ss]orghum", category), sector_number := "3.F.1.4.1"]
  alldata[sel & sector_number=="" &grepl("[Rr]ice", category), sector_number := "3.F.1.4.2"]
  alldata[sel & sector_number=="" &grepl("[Rr]ye", category), sector_number := "3.F.1.4.3"]
  alldata[sel & sector_number=="" &grepl("[Tr]ritical", category), sector_number := "3.F.1.4.4"]
  alldata[sel & sector_number=="" &grepl("[Oa]ts", category), sector_number := "3.F.1.4.5"]
  alldata[sel & sector_number=="" &grepl("[Cc]ereal|[Mm]illet", category), sector_number := "3.F.1.4.6"]
  alldata[sel & sector_number=="" , sector_number := "3.F.5"]
  
  # Clean alldata
  alldata <- alldata[, notation:= as.character(notation)]
  alldata <- alldata[, method:= as.character(method)]
  alldata <- alldata[, method := gsub("no method", "", option)]
  alldata <- alldata[, option := gsub("no option", "", option)]
  alldata <- alldata[, source := gsub("no source", "", source)]
  alldata <- alldata[, target := gsub("no target", "", target)]
  alldata <- alldata[, type := gsub("no type", "", type)]
  
  alldata <- unique(alldata)
  save(alldata, file="checkotheranimals.rdata")
  
  # Deal with other animals and animal types without sector number
  alldata[sector_number=="NULL", sector_number:=""]
  selection<-alldata$sector_number%in%"" & alldata$classification%in%mslivestockclass
  selection<-selection | grepl("^3",alldata$sector_number)
  allagri<-alldata[selection,allfields, with=FALSE]
  alldata<-alldata[!selection,allfields, with=FALSE]
  save(allagri,file="tmpallagri60.rdata")
  source("eugirp_otherlivestock.r")
  save(allagri,file="tmpallagri76.rdata")
  
  # Calculate parameters for Cattle, Swine and Sheep as aggregate of 'childs'
  # Note:  completely rewritten 20200325
  agrimeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
  agrimeas<-agrimeas[order(agrimeas$sector_number,agrimeas$category),]
  source("aggregate_cattle.r")
  allagri <- tmpagri
  save(allagri,file="tmpallagri84.rdata")
  
  alldata<-rbind(alldata,allagri)
  save(alldata, allagri, file="checkotheranimals2.rdata")
  alltotals<-alldata[grepl("^Sector",alldata$sector_number),]
  

  print("Store Information in Documentation box")
  agridocumentationbox <- alldata[measure=="Documentation box" & grepl("^3", sector_number)]
  agridocumentationbox <- unique(agridocumentationbox[, .SD, .SDcols=setdiff(names(agridocumentationbox), years)])
  agridocumentationbox <- agridocumentationbox[! notation %in% c("NA", "")]
  agridocumentationbox <- agridocumentationbox[! is.na(notation)]
  agridocumentationbox <- agridocumentationbox[, c("party", setdiff(names(agridocumentationbox), "party")), with=FALSE]
  agridocumentationbox <- cleancolumns(agridocumentationbox)
  
  alldata <- alldata[! (measure=="Documentation box" & grepl("^3", sector_number))]
  
  print("Store Information on Emission Factors")
  agriEFinfo <- alldata[measure=="Emission factor information" & grepl("^3", sector_number)]
  # Some values are reported only for certain years - the information on the emission
  # factor therefore changes and could be e.g. NA in some years (not applicable as no emissions
  # occur) and CS in others (country-specific). Here this is merged.
  # What counts for the evaluation at the end is the info for the years with emissions
  agriEFinfo <- agriEFinfo[, paste(as.character(.SD), collapse="-"), 
                           by=setdiff(names(agriEFinfo), c("notation", years)), 
                           .SDcols="notation"]
  agriEFinfo <- agriEFinfo[, V1 := gsub("[\",c,\\(,\\)]", "", V1)]
  agriEFinfo <- agriEFinfo[! is.na(V1)]
  #agriEFinfo <- agriEFinfo[V1 != "NA"]
  
  print("Store Information on Method")
  agriMethods <- alldata[measure=="Method" & grepl("^3", sector_number)]
  agriMethods <- agriMethods[, paste(as.character(.SD), collapse="-"), 
                           by=setdiff(names(agriMethods), c("notation", years)), 
                           .SDcols="notation"]
  agriMethods <- agriMethods[, V1 := gsub("[\",c,\\(,\\)]", "", V1)]
  agriMethods <- agriMethods[! is.na(V1)]
  
  agriMethods <- rbind(agriEFinfo, agriMethods)
  setnames(agriMethods, "V1", "notation")
  # EF info and Method have different variableUIDs
  agriMethods <- dcast.data.table(agriMethods, party+sector_number+category+gas+method+classification+source+target+type+option~measure, value.var = "notation")
  
  # Separate NA in methods from rest to make less crowded
  agriMethodsNA <- agriMethods[((is.na(`Emission factor information`) | `Emission factor information`=="NA") & (is.na(Method) | Method=="NA"))]
  agriMethods <- agriMethods[! ((is.na(`Emission factor information`) | `Emission factor information`=="NA") & (is.na(Method) | Method=="NA"))]
  
  # Define 'higher Tier' or not
  agriMethods <- agriMethods[, `Higher Tier` := ifelse(grepl("T2|T3", Method)|grepl("CS", `Emission factor information`), 1, 0)]
  
  # Remove info from alldata - other than agriculture sectors are not relevant
  save(alldata, file="alldata.rdata")
  alldata <- alldata[! (measure %in% c("Documentation box", 
                                       "Emission factor information", 
                                       "Method"))]
  
  
  # Store notations in different data frame - delete from alldata ####
  agrinotations <- alldata[grepl("^3", sector_number) & notation!=""]
  agrinotations <- agrinotations[, paste(as.character(.SD), collapse="-"), 
                                 by=setdiff(names(agrinotations), c("notation", years)), 
                                 .SDcols="notation"]
  agrinotations <- agrinotations[, V1 := gsub("[\",c,\\(,\\)]", "", V1)]
  agrinotations <- agrinotations[! is.na(V1)]
  agrinotations <- agrinotations[, c("party", setdiff(names(agrinotations), "party")), with=FALSE]
  agrinotations <- cleancolumns(agrinotations)
  # Remaining 'method's -> Tier 2 (CLIMA, MCF, non-agri) and other 
  #                        Methods for LULUCF
  # Remaining 'type's   -> Information item (links to fossil fuel)
  #                        Additional variables (LULUCF) 
  #                        Activity  A.1 / B.1 / A.2 (LULUCF)
  #                        ... many others for LULUCF
  #                        Inorganic / Organic N Fertilizers (AGRI)        
  alldata <- alldata[notation==""]

  
  wb <- createWorkbook(creator = "EU-GIRP")
  f1 <- paste0(gsub("eealocator_", "agridata", csvfil), "_methods.xlsx")
  wbs <- addWorksheet(wb, sheetName = "Documentation Box")
  wbs <- writeData(x = agridocumentationbox, wb = wb, sheet = "Documentation Box")
  wbs <- addWorksheet(wb, sheetName = "Notation Keys")
  wbs <- writeData(x = agrinotations, wb = wb, sheet = "Notation Keys")
  wbs <- addWorksheet(wb, sheetName = "Methods")
  wbs <- writeData(x = agriMethods, wb = wb, sheet = "Methods")
  wbs <- addWorksheet(wb, sheetName = "Methods_NA")
  wbs <- writeData(x = agriMethodsNA, wb = wb, sheet = "Methods_NA")
  wbs <- addWorksheet(wb, sheetName = "Methods_sum")
  wbx <- unique(agriMethods[, .(`Emission factor information`, Method, `Higher Tier`)])
  wbs <- writeData(x = wbx, wb = wb, sheet = "Methods_sum")
  wbw <- saveWorkbook(wb, file=f1, overwrite = TRUE)
    
  stepsdone<-2
  savelist<-c(savelist,"alltotals", "agridocumentationbox", "agriMethods", "agriMethodsNA", "agrinotations")
  savestep(stepsdone, savelist)
  if(!is.null(gdrive)){
    file.copy(from = f1, to = paste0(gdrive, cursubm, "/", basename(f1)), overwrite =  TRUE)
  }else{
    drive_update(media = f1, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
  }
  source("curplot.r")
}else if(stepsdone>1){
  print("Step 2: List of measures & animals ... already done")
}


#stop("second step done")
# B.1 - Plots 1. Calculate EU-sums and simplify units (remove very large numbers) ####
if(stepsdone==2){
  print("Step 3a: Calculating EU-sums only for summable variables")
  
  if(!is.null(keepNORout)){ 
    print("Keeping Norway out")
    alldata_NOR<-alldata[alldata$party == "NOR",]
    alldata<-alldata[alldata$party != "NOR",]
  }
  
  alldata<-alldata[alldata$party!="EU28",]
  alldata$datasource<-"nir"
  
  # Extract calceu from alldata - combine at the end
  calceu<-alldata[grepl("^3",alldata$sector_number),]
  alldata<-alldata[!grepl("^3",alldata$sector_number),]
  
  calcmeas<-unique(subset(calceu,select=allfields[!allfields %in% c("notation","party",years)]))
  measures2sum<-calcmeas[calcmeas$meastype %in% meas2sum,]
  lc<-measures2sum[grepl("^3",measures2sum$sector_number),]
  
  print("Calculate EU-sums")
  calceu <- eu28sums(A = calceu,years = years) #xavi20180504: to calculate sums for EUA and EUC
  eu28sum <- calceu[calceu$meastype %in% meas2sum & calceu$party %in% eu,]
  
  calceu <- calceu[!calceu$party %in% eu,] #xavi20180219:removing EUC and EUA sums, both well and wrongly calculated (i.e. meas2popweight, CLIMA and MCF, which need to be averaged)
  calceu <- rbind(calceu, eu28sum) #xavi20180219: so that calceu keeps only EUC and EUA sums for those that can be summed (i.e. meas2sum), but all the sector n.3 (agri)
  agrisummeas<-measures2sum[grepl("^3",measures2sum$sector_number),]
  
  # Check on outliers in AD and EMs: no country should really dominate unless it is the only country reporting
  checkADoutliers <- calceu[grepl("^3",sector_number) & meastype %in% meas2sum]
  checkADoutliers <- melt.data.table(checkADoutliers, measure.vars = years, variable.name = "years")
  checkADoutliers <- unique(checkADoutliers)
  dofrom <- paste(setdiff(names(checkADoutliers), c("value", "party", "notation")), collapse = " + ")
  cntrs <- unique(checkADoutliers$party)
  checkADoutlms <- dcast.data.table(checkADoutliers, as.formula(paste0(dofrom, " ~ party")), value.var = "value", fill = FALSE)
  checkshares <- checkADoutlms[, (cntrs) := .SD/EUC, .SDcols=cntrs]
  checkshares <- checkshares[, maxshare := do.call(pmax, .SD), .SDcols = setdiff(cntrs, eu)]
  
  # Select those with a share > 0.95
  checkshares <- checkshares[maxshare > 0.95 & maxshare < 1]
  
  # Allow some special cases
  checkshares<-checkshares[ ! (ROU > 0.95  & grepl("^3.F.",sector_number))]
  checkshares<-checkshares[ ! (ESP > 0.95  & grepl("^3.F.",sector_number))]
  
  checkshares<-checkshares[ ! (category == "Buffalo")]
  checkshares<-checkshares[ ! (category == "Growing Lambs")]
  checkshares<-checkshares[ ! (source == "Digesters")]
  checkshares<-checkshares[ ! (option == "Option B")]
  checkshares<-checkshares[ ! (option == "Option C")]
  
  checkshares<-checkshares[ ! (grepl("^3.*8..$", sector_number))]
  checkshares<-checkshares[ ! (grepl("^3.J", sector_number))]
  write.xlsx(checkshares, file=paste0(invloc,"/checks/checks",cursubm,"ADEMoutliers.xlsx"))
  
  save(alldata,eu28sum, calceu,file=gsub("_clean","_nounitconv",rdatallem))
  
  # NEXC must be all changed, even if for one MMS the values are small
  
  calceu <- calceu[unit == "kg N/year", (years) := .SD/1000000, .SDcols = years]
  calceu <- calceu[unit == "kg N/year", unit := "kt N/year"]
  
  alldata <- rbind(alldata, calceu)  
  o<-order(alldata$sector_number,alldata$category,alldata$meastype,alldata$classification,alldata$party)
  alldata<-alldata[o,allfields, with=FALSE]
  alldata <- alldata[method=='no option', method:=""]
  
  #stop("pause")
  source("eugirp_allagri.r")  #20190125: allagri data WITH Norway (alldata WITHOUT Norway)
  
  stepsdone<-3
  emplotsdone<-0
  acountry<-curcountries[variable==eusubm & value==1]$code3
  
  savelist<-c(savelist,"emplotsdone","eu28sum","allagri","agriemissions","agridet","agrimix","agrigen", "alldata_NOR", "acountry")
  savestep(stepsdone, savelist)
  
  
  
  mixplotsfiles <- list.files(paste0(plotsdir, cursubm, "/mixplots/"), pattern = ".*jpg|.*png", full.names = TRUE)
  if(!is.null(gdrive)){
    # Not at the server - files can be copied locally and will be updloaded by Backup
    x <- lapply(1:length(mixplotsfiles), function(x) 
      file.copy(from = mixplotsfiles[x], to = paste0(gdrive, cursubm, "/mixplots/", basename(mixplotsfiles[x])), overwrite =  TRUE))
    file.copy(from = f2, to = paste0(gdrive, cursubm, "/", basename(f1)), overwrite =  TRUE)
  }else{
    drive_update(media = mixplotsfiles[x], path = paste0(gdrive, cursubm, "/mixplots/"), overwrite = TRUE, verbose = TRUE) 
    drive_upload(media = f2, path = paste0("eugirp/", cursubm, "/"), overwrite = TRUE, verbose = TRUE)
  }
  source("curplot.r")
}else if(stepsdone>2){
  print("Step 3a: EU sums already calculated")
}

#stop("Third step done")
# B.2 - Plots 1. Do emission plots ####
#emplotsdone<-0
doemissionplots <- FALSE
doemissionplots<-TRUE
if(stepsdone>2){
    if(doemissionplots==TRUE){
        if(emplotsdone==0){
            print("Step 4: Emission plots")
            if(!is.null(keepNORout)) print("Keeping Norway out of these plots")
          
            adempars<-c("AD","EM")
            
            rundata<-"adem"
            runfocus<-"value"
            datasource<-"nir"
            
            #Do temporary corrections for observations
            
            #if(cursubm == "20190508" & file.info(paste0(csvfil,".txt"))$ctime < "2019-05-11 15:11:54 CEST"){
            #  alldata[alldata$variableUID == "E92759E4-BD91-46D2-BEE4-B21C3C0D3207" & alldata$party == "POL", ]$`1995` <- 146800
            #  alldata[alldata$variableUID == "131C4470-1F4D-4A5C-93F9-094BC7BA86F9" & alldata$party == "POL", ]$`1995` <- 52000
            #}
            
            temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
            plotdata<-temp[[1]]
            plotdata$autocorr<-NA
            plotdata$correction<-1
            plotmeas<-temp[[2]]
            plotmeas<-plotmeas[, i := .I, by=1:nrow(plotmeas)]
            adddefault<-temp[[3]]
            sharesexist<-temp[[4]]
            plotmeas <- plotmeas[sector_number!="-"]
            plotmeas <- plotmeas[, i := .I]

            x1<-368;x2<-nrow(plotmeas)
            x1<-155;x2<-156
            x1<-1;x2<-2
            x1<-167; x2<-nrow(plotmeas)
            x1<-1; x2<-nrow(plotmeas)
            #imeas <- 171
            #loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")
            #View(alldata[alldata$variableUID == "91817067-8DB6-41D6-A348-57E2C17B655D", ])
            for(imeas in x1:x2){
              loopoverplots(imeas = imeas,runfocus = runfocus,eusubm = "EUC")
            }
            plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
            write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")
            
            zipplots <- function(sec){
              lfil <- list.files(path = paste0(plotsdir,cursubm, "/", sec), 
                                 pattern = "*jpg", full.names = TRUE)
              if(length(lfil)>0){
                cat("\nzipping ", sec, "to ", paste0(plotsdir,cursubm, "_", sec, ".zip"))
                zipr(zipfile = paste0(plotsdir,cursubm, "_", sec, ".zip"), files = lfil)
              }
            }
            secs2zip <- c("sec1", "sec2", "sec4", "sec5", "sec6", 
                          "secJ", "secK", "secN", "secS", "valueadem")
            ok <- sapply(secs2zip, function(x) zipplots(x))
            
            emplotsdone<-1
            
            savelist<-c(savelist,"emplotsdone","plotmeas")
            savefile <- gsub("_s[0-9]", "", rdatallem)
            save(list=savelist,file=savefile)
            file.copy(from = savefile, to = gsub(".RData",paste0("_s", stepsdone,".RData"), savefile), overwrite = TRUE)
            file.copy(from = savefile, to = gsub(".RData",paste0("_s", stepsdone, "~",figdate,".RData"),savefile), overwrite = TRUE)
            mixplotsfiles <- list.files(paste0(plotsdir, cursubm, "/valueadem/"), pattern = ".*jpg|.*png", full.names = TRUE)
            if(!is.null(gdrive)){
              # Not at the server - files can be copied locally and will be updloaded by Backup
              file.copy(from = rdatallem, to = paste0(gdrive, "rdatabase/", basename(rdatallem)), overwrite =  TRUE)
              if(!dir.exists(paste0(gdrive, cursubm, "/valueadem"))){dir.create(paste0(gdrive, cursubm, "/valueadem"))}
              x <- lapply(1:length(mixplotsfiles), function(x) 
                file.copy(from = mixplotsfiles[x], to = paste0(gdrive, cursubm, "/valueadem/", basename(mixplotsfiles[x])), overwrite =  TRUE))
            }else{
              #drive_update
              drive_upload(media = rdatallem,        path = paste0(gdrive, "rdatabase/"),          overwrite = TRUE, verbose = TRUE)
              drive_update(media = mixplotsfiles[x], path = paste0(gdrive, cursubm, "/valueadem/"), overwrite = TRUE, verbose = TRUE) 
            }
            #stop("End of general part (Emission plots done!)")
            source("curplot.r")
        }else{
            print("Step 4: Emission plots already done")
        }
    }else{
        if(emplotsdone==0) print("Emission plots not yet done but not requested")
        if(emplotsdone==1) print("Emission plots already done")
    }
}

#stop("plots done")
#++++ END OF GENERAL PART 
#++++ BELOW SECTOR-3 SPECIFIC PART
# 2019: Norway is included in all checks
# A.3 Calculate trend and growth rates ####
if(stepsdone==3){
    print("Step 4: Calculating trends and growth rates")
    if(!is.null(keepNORout)) print("Including (again) Norway")
    
    nyears<-length(years)
    period1<-as.character(years[1]:years[nyears-1])
    period2<-as.character(years[2]:years[nyears])
    
    #agriemissions$option[agriemissions$option==0]<-""
    agriemissions$datasource<-"nir"
    agriemissions$option<-""
    agriemissions<-unique(agriemissions)
    
    # Agrishares as agriemissions relative to total emissions ####
    # 2019: both 'alltotals' and 'agriemissions' include Norway
    totalval <- alltotals[alltotals$classification==signclass&alltotals$gas=="Aggregate GHGs",]
    
    # Add totals agriculture emissions
    agriCO2eq <- dt2CO2eq(allagri[meastype=="EM"])
    totalagg <- agriCO2eq[sector_number=="3" & gas=="Aggregate GHGs"]
    
    # Combine and melt
    totalcom <- rbind(totalval, totalagg, fill=TRUE)
    totalcom <- melt.data.table(totalcom[, .SD, .SDcols=c("type", "party", years)], id.vars = c("party", "type"), value.name = "totalvalue")
    #setnames(totalval, "classification", "total")
    totalval <- dcast.data.table(totalcom, party + variable ~ type, value.var = "totalvalue")
    totalval <- totalval[, .(party, variable, agri=V1, withLULUCF=`Total (with LULUCF)`, woLULUCF=`Total (without LULUCF)`)]
    
    # Calculate shares and clean data table
    agrishares <- melt.data.table(agriemissions[, -"notation", with=FALSE], measure.vars = years)
    agrishares <- merge(agrishares, totalval, by=c("party", "variable"))
    agrishares <- agrishares[, `:=` (agri = value/agri, wLULUCF=value/withLULUCF, woLULUCF=value/woLULUCF)]
    agrishares <- melt.data.table(agrishares, measure.vars = c("agri", "wLULUCF", "woLULUCF"), value.name = "share", variable.name = "shtype")
    aform <- paste(setdiff(names(agrishares), c("variable", "share", "value", "withLULUCF")), collapse="+")
    agrishares <- dcast.data.table(agrishares[share != 0], as.formula(paste0(aform, " ~ variable")), value.var = "share")
    agrishares <- agrishares[, `:=` (meastype='share',unit=shtype, measure=paste0("Share from Totals ", shtype))]
    agrishares <- agrishares[, -'shtype', with=FALSE]
    
    f1 <- paste0(gsub("eealocator_", "agridata", csvfil), "_shares.xlsx")
    f2 <- paste0(gsub("eealocator_", "agridata", csvfil), "_CO2eq.xlsx")
    write.xlsx(agrishares, file = f1, asTable = TRUE, overwrite=TRUE)
    write.xlsx(agriCO2eq, file = f2, asTable = TRUE, overwrite=TRUE)
    
    # Signnificant categories on the basis of the share threshold criterium only ####
    otheryears<-years[!years%in%signyear]
    
    signcategories<-agrishares[, -otheryears, with=FALSE]
    if(signtype == "Total (without LULUCF)") {
      signcategories <- signcategories[unit=="woLULUCF"]
    }else{
      signcategories <- signcategories[unit=="wLULUCF"]
    }
    
    #### This needs to be redone!!!
    # Determination of potentially significant issues using the shares vs total
    # Questions: only for last year, or using max share?
    # Other questions: 
    dontdoforthemoment <- TRUE
    if(! dontdoforthemoment){
      signcategories[,"maxshare"]<-apply(agrishares[,years],1,max)
      signshares<-c(signyear,"maxshare")
      signcategories[,"potsig"]<-whichmatrix(D = signcategories[,as.character(signyear)],v = which(signcategories[,as.character(signyear)]>signthreshold,arr.ind = TRUE))    
      signcategories$potsig[signcategories$potsig>0]<-1
      shiftfields<-c(metafields,"meastype","unit","measure","notation")
      sigfield<-c(allfields[!allfields%in%c(years,"variableUID",shiftfields)],"potsig",signyear,"maxshare","variableUID",shiftfields)
      signcategories<-signcategories[order(signcategories$sector_number,signcategories$category),sigfield]
      
      if (! file.exists(paste0(invloc,"/checks/significant/"))){dir.create(file.path(paste0(invloc,"/checks/significant")),showWarnings = FALSE )}
      fileunder<-paste0(invloc,"/checks/significant/signcategories~",figdate,".csv")
      con<-file(fileunder,open = "wt")
      writeLines(signcatexp,con)
      write.csv(signcategories,con)
      close(con)
    }
    
    tmp <- as.data.frame(allagri)
    alltrend<-as.data.frame(matrix(0,rep(0,ncol(tmp)),ncol=ncol(tmp)))
    alltrend<-allagri[tmp$meastype %in% meas2sum,]
    alltrend[,period2]<-tmp[tmp$meastype %in% meas2sum,period2]-tmp[tmp$meastype %in% meas2sum,period1]
    alltrend[,years[1]]<-NA
    
    mgrowth<-c(meas2popweight,meas2clima,meas2mcf)
    allgrowth<-as.data.frame(matrix(0,rep(0,ncol(tmp)),ncol=ncol(allagri)))
    allgrowth<-tmp
    allgrowth[,period2]<-tmp[,period2]/tmp[,period1]
    allgrowth[is.nan(allgrowth)] <- 0
    allgrowth[is.infinite(allgrowth)] <- 1
    allgrowth[,years]<-round(allgrowth[,years],3)
    allgrowth[,years[1]]<-NA
    allgrowth <- as.data.table(allgrowth)
    
    stepsdone<-4
    checksteps<-4
    savelist<-c(savelist,"alltrend","allgrowth","agrishares","signcategories","signthreshold")
    savestep(stepsdone, savelist)
    if(!is.null(gdrive)){
      # Not at the server - files can be copied locally and will be updloaded by Backup
      if(!dir.exists(paste0(gdrive, cursubm, "/valueadem"))){dir.create(paste0(gdrive, cursubm, "/valueadem"))}
      file.copy(from = f1, to = paste0(gdrive, cursubm, "/", basename(f1)), overwrite =  TRUE)
      file.copy(from = f2, to = paste0(gdrive, cursubm, "/", basename(f2)), overwrite =  TRUE)
    }else{
      drive_update(media = f1, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
      drive_update(media = f2, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
    }
    source("curplot.r")
}else if(stepsdone>3){
    print("Step 4: Trends and growth rates already calculated")
}

#stop("step 4 done")
# A.4 NE-check and check on unit errors. Prepare for outlier check ####
if(stepsdone==4){
    print("# A.4 NE-check and check on unit errors. Prepare for outlier check ####")
    if(!is.null(keepNORout)){
      print("Norway data is included for these checkings")
      alldata <- rbind(alldata, alldata_NOR)
    } 
    
    print(paste0("Step ",stepsdone+1,"a: Caluclate allagri for EU28"))
    allagri$datasource<-"nir"
    allagri<-allagri[allagri$party!="EU28",]
    #xavi20180220: allagri<-eu28sums(allagri,aeu = c("EUC","EUA"),years = years)    
    #xavi20180220: this is already done for EUC (I think not necessary for EUA?)
    allagri<-allagri[order(allagri$sector_number,allagri$category,allagri$meastype),]
    
    #remove option from Cattle, Dairy Cattle, Non-Dairy Cattle
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option[allagri$category%in%allcattle]<-""
    allagri<-unique(allagri)
    curnames<-c("sector_number","category","meastype","option","party","variableUID",years)
    
    # Create table with statistical info for each variable
    agrimeas<-unique(subset(allagri,select=allfields[!allfields %in% c("notation","party",years)]))
    agrimeas<-agrimeas[order(agrimeas$sector_number,agrimeas$category),]
    
    ### NE check sone manually with EEA tables
    dontdonecheck <- TRUE
    if(! dontdonecheck){
      print(paste0("Step ",stepsdone+1,"b: Check NEs"))
      if (! file.exists(paste0(invloc,"/checks/nechecks"))){
        dir.create(file.path(paste0(invloc,"/checks/necheck/")))}
      source("eugirp_checknes.r")
    }
    
    print(paste0("Step ",stepsdone+1,"c: Check units"))
    if (! file.exists(paste0(invloc,"/checks/autocorrections"))){dir.create(file.path(paste0(invloc,"/checks/autocorrections/")))}
    source("eugirp_checkunits.r")
    
    print(paste0("Step ",stepsdone+1,"d: Calculation statistics distribution for parameters and growth"))
    source("eugirp_calcstats.r")
    
    stepsdone<-5
    savelist<-c(savelist,"agrimeas", "paramdata","paramstats", "growthdata","growthstats","autocorrections")
    savestep(stepsdone, savelist)
    if(!is.null(gdrive)){
      # Not at the server - files can be copied locally and will be updloaded by Backup
      if(!dir.exists(paste0(gdrive, cursubm, "/valueadem"))){dir.create(paste0(gdrive, cursubm, "/valueadem"))}
      file.copy(from = f1, to = paste0(gdrive, cursubm, "/", basename(f1)), overwrite =  TRUE)
    }else{
      drive_update(media = f1, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
    }
    source("curplot.r")
}else if(stepsdone>4){
    print("Step 5: NEchecks and Unitchecks done; param and growth prepared for outlier checks.")
}


#stop("step 5 done")
doplots <- FALSE
doplots <- TRUE

# A.3 Check for outlier errors and write outlier lists ####
if(stepsdone==5){
    
    print("# A.3 Check for outlier errors and calculate EU weighted averages ####")
    #if(!is.null(keepNORout)){
    #  print("Norway data is included for these checkings (not for calculating EU weighted averages!!)")
    #  alldata <- rbind(alldata, alldata_NOR)
    #} 
  
    print(paste0("Step ",stepsdone+1,"a: Check for outlier errors on parameters @ ",curtime(1)))
    outcheck<-"param"
    source("eugirp_checkoutliers.r")
    
    #Why commented??
    #source("eugirp_ipccdefaults.r")
    print(paste0("Step ",stepsdone+1,"b: Check for outlier errors on growth @ ",curtime(1)))
    
    # The outlier check for 'growth' is not really used as the plots are better to see problems
    # It doesn't work as the 
    outcheck<-"growth"
    nyears<-length(years)
    source("eugirp_checkoutliers.r")
    # Add the gas to each measure so see what the corresponding emissions are
    #al20200127 ... gas already availabel in paramcheck - not clear what the formula below was doing
    #levels(paramcheck$gas)<-factor(signcategories$gas)
    paramcheck<-paramcheck[!paramcheck$party%in%eu,]
    paramcheck<-paramcheck[remagglevel(paramcheck,mt = 1),]
    
    # Ispotentialissue:
    # Method: (a) Significant issue only if the last year is included in the list of outliers
    #         (b) Checks if the difference to using the median reported value is significant
    #             acc. to the significance threshold. Uses data frame significantcategories
    #             calculated earlier
    # Flags: over --> issue is a potential overestimation
    #        overnotlast ---> issue is a potential overestimation but last year is not identified
    #        oversource ---> issue interests a significant source category, but overestimation might be below threshold
    #        ... in analogy for 'under'
    testfieldsvalues<-as.data.table(Reduce(rbind,lapply(c(1:nrow(paramcheck)),function(x) 
    {
      #cat(" ",x)
      y <- Reduce(cbind,ispotentialissue(line = paramcheck[x,],
                                         S = signcategories,
                                         signyear = as.character(signyear),
                                         signthreshold = signthreshold))
      y <- as.data.table(y)
      names(y) <- paste0("V", 1:6)
      return(y)
    }
    ))
    )
    names(testfieldsvalues) <- testfields
    paramcheck <- cbind(paramcheck, testfieldsvalues)
    paramcheck[is.na(paramcheck)] <- ""
    paramchecked<-0
    
    # Load IPCC default values
    paramcheck<-loadipccdefaults(as.data.frame(paramcheck),1,nrow(paramcheck),insert="value")
    write.csv(paramcheck,file="paramcheck_ipcc.csv")

    print(paste0("Step ",stepsdone+1,"c: Prepare for plot-names @ ",curtime(1)))
    rundata<-"ief"
    runfocus<-"value"
    #plotparamcheck: plotmeas is overwritten by paramcheck
    plotparamcheck<-0
    # Create link to plots
    paramcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(plotsdir,"../../plots/",unlist(lapply(c(1:nrow(paramcheck)),function(x) 
                        plotname(dsource = "",plotsdir = plotsdir,issuedir = issuedir,imeas = "*",
                                 runsect = paramcheck[x,sectfields],
                                 runmeta = paramcheck[x,metafields],
                                 runmeas = paramcheck[x,measfields],
                                 runfoc = paste0("nir",runfocus),
                                 figdate = figdate,plotformat = plotformat,rundata = rundata,cursubm = cursubm,
                                 plotparamcheck=0))))),
               "\",\"Link to plot\")")
    t<-growthcheck
    co<-c("plot",names(growthcheck))
    growthcheck[is.na(growthcheck)] <- ""
    dfgrowth <- as.data.frame(growthcheck)
    growthcheck$plot<-
        paste0("=HYPERLINK(\"",
               gsub(" ","",
                    gsub(plotsdir,"../../plots/",unlist(lapply(c(1:nrow(growthcheck)),function(x) 
                        plotname("",plotsdir,issuedir,"*",dfgrowth[x,sectfields],dfgrowth[x,metafields],dfgrowth[x,measfields],
                                runfoc=paste0("nir",runfocus),figdate,plotformat,rundata,cursubm,plotparamcheck=0))))),
               "\",\"Link to plot\")")
    
    
    print(paste0("Step ",stepsdone+1,"d: Calculate key categories @ ",curtime(1)))
    
    dokeycategories <- FALSE
    if(dokeycategories){
      keycategories<-keycategories()
      keyeuagri<-keycateuc()
    }
    print(paste0("Step ",stepsdone+1,"e: Prepare flags @ ",curtime()))
    test0<-matrix(rep(0,nrow(growthcheck)*length(flag4issues)),ncol=length(flag4issues),nrow=nrow(growthcheck))
    test0<-as.data.frame(test0)
    names(test0)<-flag4issues
    growthcheck<-growthcheck[,.SD, .SDcols=setdiff(names(growthcheck), "gas")]
    growthcheck<-cbind(growthcheck,test0)
    #growthcheck$party<-as.character(growthcheck$party)
    print(paste0("Step ",stepsdone+1,"f: Integrate outcome into growthcheck and to writeoutlierlist @ ",curtime()))
    x1<-1;x2<-nrow(growthcheck)
    
    # This requires comnplete re-coding: 
    # It uses files from 2015 that urgentely require update
    # Also, it uses a lot of slow functions that certainly can be improved.
    doaddflags <- FALSE
    if(doaddflags){
      test<-lapply(c(x1:x2),function(x) 
        unlist(flags4newissue(line = as.data.frame(growthcheck)[x,],check = "growth",x = x)))
      test<-Reduce(rbind,test)
      growthcheck[x1:x2,flag4issues]<-test
      print(paste0("Step ",stepsdone+1,"g: Load now solved issues @ ",curtime()))
      growthcheck<-addsolved2check(growthcheck,c("recalc"))
    }
    
    cog<-names(growthcheck)
    
    print(paste0("Step ",stepsdone+1,"a: Making Growth plots @ ",curtime()))
    mainanimals<-c("Dairy Cattle","Non-Dairy Cattor","Sheep","Swine","Poultry")
    mainmeasures<-c("AD","IEF","POP","AREA","NRATE","FracGASF","FracGASM","FracLEACH")
    
    if(doplots){
    
        for(mm in mainmeasures) {makegrowthplot(secs="3.",meastype=mm)}
    
    }
    
    
    
    test0<-matrix(rep(0,nrow(paramcheck)*length(flag4issues)),ncol=length(flag4issues),nrow=nrow(paramcheck))
    test0<-as.data.frame(test0)
    names(test0)<-flag4issues
    paramcheck<-paramcheck[,-which(names(paramcheck)=="gas")]
    paramcheck<-cbind(paramcheck,test0)
    paramcheck$party<-as.character(paramcheck$party)
    # !!
    print(paste0("Step ",stepsdone+1,"h: Integrate outcome into paramcheck and to writeoutlierlist @ ",curtime()))
    x1<-1;x2<-nrow(paramcheck)
    #source("eugirp_functions.r")
    if(doaddflags){
      keycategories <- keycategories()  #xavi20180131
      test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(paramcheck[x,],"outlier",x)))
      test<-Reduce(rbind,test)
      paramcheck[x1:x2,flag4issues]<-test
      #Load now solved issues!
      # --> function addsolved2check in file eugirp_functions.r (ca line 1160)
      paramcheck<-addsolved2check(paramcheck,c("recalc"))
      
      print(paste0("Step ",stepsdone+1,"i: Write country outlier list @ ",curtime()))
      test<-as.data.frame(test)
      names(test)<-flag4issues
      write.csv(test,file=paste0(filoutliers,"list_checked4emrt.csv"))
    }
    
    print(paste0("Step ",stepsdone+1,"j: Write outlier list @ ",curtime(1)))
    cof<-names(paramcheck)
    source("eugirp_writeoutlierlist.r")
    
    # removing again NOR data
    #if(!is.null(keepNORout)) alldata <- alldata[alldata$party != "NOR",] 
    
    stepsdone<-6
    savelist<-c(savelist,"growthcheck","paramcheck","paramchecked","keycategories")
    savestep(stepsdone, savelist)
    source("curplot.r")
}else if(stepsdone>5){
    print(paste0("Step 6: Check for outliers errors already done"))
}

    
stop("step 6 done")
doplots <- FALSE
doplots <- TRUE
# Calculate EU weighted averages and make adem and ief plots####
if(stepsdone==6){
    # Make growth plots to check ... improve loop!!
    #temporarycommented 
    print(paste0("Step ",stepsdone+1,"d: Calculate EU weighted averages"))
    #stop("now write issues")
    #paramcheck$correction[paramcheck$party=="SWE"&paramcheck$meastype=="VSEXC"]<-0
    
    #removing NOR data before to calculate averages
    if(!is.null(keepNORout)){ 
      print("Averages computed keeping OUT Norway")
      allagri_NOR<-allagri[allagri$party == "NOR",]
      allagri<-allagri[allagri$party != "NOR",]
    }
    source("eugirp_euweightedaverages.r")
    f1 <- paste0(gsub("eealocator_", "agridata", csvfil), "_allagriEU.xlsx")
    f2 <- paste0(gsub("eealocator_", "agridata", csvfil), "_allagriMS.xlsx")
    write.xlsx(eukpsum, file = f1, asTable = TRUE, overwrite=TRUE)
    write.xlsx(allagri[!party %in% eu], file = f2, asTable = TRUE, overwrite=TRUE)
  
  
    export4uba(allagri = allagri)
    
    # Including again Norway data
    if(!is.null(keepNORout)){
      allagri_NOR$autocorr <- ""
      allagri_NOR$correction <- ""
      allagri <- rbind(allagri, allagri_NOR)
    }  
    print(paste0("Step ",stepsdone+1,"e: Make plots"))
    datasource<-"nir"
    runfocus<-"value"
    rundata<-"ief"
    #source("eugirp_prepareplots.r")
    temp<-generateplotdata(rundata = rundata,datasource = datasource,subcountries = "EUC")
    plotdata<-temp[[1]]
    plotmeas<-temp[[2]]
    adddefault<-temp[[3]]
    sharesexist<-temp[[4]]
    eukp <- 'EUC'
    
    namesnoref <- setdiff(names(plotmeas), c("ref2006", "ref1997"))
    
    x1<-942;x2<-nrow(plotmeas)
    x1<-31;x2<-33
    x1<-1;x2<-nrow(plotmeas)
    
    if(doplots){
      
      for(imeas in x1:x2){
        loopoverplots(imeas = imeas,runfocus = "value",eusubm = "EUC")
        }
      for(imeas in x1:x2){
        loopoverplots(imeas = imeas,runfocus = "range",eusubm = "EUC")
        }
    
    }
    plotmeas$imeas<-unlist(lapply(c(1:nrow(plotmeas)),function(x) x))
    write.table(data.frame("ID"=rownames(plotmeas),plotmeas),file=paste0(plotsdir,"/",rundata,"plots~",curtime(),".csv",collapse=NULL),row.names=FALSE,sep=";",dec=".")
    
    
    stepsdone<-stepsdone+1

    save(listofmeasuresnotconsidered,measures2sum,measures2wei,file=rdatmeasu)
    savelist<-unique(c(savelist,"assignad2par"))
    savelist<-c(savelist,"growthcheck","paramcheck","paramchecked","keycategories")
    savestep(stepsdone, savelist)
    
    f3 <- list.files(path = paste0(invloc, "/tables4eu"), pattern = paste0(cursubm))
    f3 <- f3[! grepl("~", f3)]
    f4 <- list.files(path = paste0(plotsdir, cursubm, "/valueief"), pattern = "jpg", full.names = TRUE)
    f5 <- list.files(path = paste0(plotsdir, cursubm, "/rangeief"), pattern = "jpg", full.names = TRUE)
    if(!is.null(gdrive)){
      # Not at the server - files can be copied locally and will be updloaded by Backup
      file.copy(from = f1, to = paste0(gdrive, cursubm, "/", basename(f1)), overwrite =  TRUE)
      file.copy(from = f2, to = paste0(gdrive, cursubm, "/", basename(f2)), overwrite =  TRUE)
      x <- lapply(1:length(f3), function(x) 
        file.copy(from = f3[x], to = paste0(gdrive, cursubm, "/", basename(f3[x])), overwrite =  TRUE))
      if(!dir.exists(paste0(gdrive, cursubm, "/valueief"))){dir.create(paste0(gdrive, cursubm, "/valueief"))}
      x <- lapply(1:length(f4), function(x) 
        file.copy(from = f4[x], to = paste0(gdrive, cursubm, "/valueief/", basename(f4[x])), overwrite =  TRUE))
      if(!dir.exists(paste0(gdrive, cursubm, "/rangeief"))){dir.create(paste0(gdrive, cursubm, "/rangeief"))}
      x <- lapply(1:length(f5), function(x) 
        file.copy(from = f5[x], to = paste0(gdrive, cursubm, "/rangeief/", basename(f5[x])), overwrite =  TRUE))
    }else{
      drive_update(media = f1, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
      drive_update(media = f2, path = paste0(gdrive, cursubm, "/"), overwrite = TRUE, verbose = TRUE) 
    }
    
    source("curplot.r")
    
}else if(stepsdone>6){
    print(paste0("Step 7: Check for outlier errors already done"))
}

stop("step 7 done")
# Note AL20200327 - up to Step 7 script revised to work with data.tables & various other improvements
#                 - check github for issues that need to be addressed before next submission
# C - Make checks for sector 3 ####
#checksteps<-7
if(stepsdone==7) {
    print(paste0("Step ",stepsdone+1,": Make specific checks for Sector 3 - Set 1"))
    #load(rdatmeasu)
    
    # source("tmp_otherlivestockearlier.r")
    # Again manual correction of BEL-swine/sheep problem
    #     allagri<-allagri[!allagri$variableUID=="8A72A9BE-DB94-4277-ADFC-8AE85BE6B999",]
    #     uidrem<-"76660D32-4F2A-4C65-81C3-71FA2C340542"
    #     uidnew<-"AB1CC8F6-D71C-46A1-A846-B5E76E2DE3A2"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BEL"]<-uidnew
    
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    #     uidrem<-"1FB8F04A-E6A1-47B7-A6F0-407836FDF3EF"
    #     uidnew<-"3FB4B8CB-B0A8-4BA1-8D81-CD412F301D1B"
    #     allagri$variableUID[allagri$variableUID==uidrem&allagri$party=="BEL"]<-uidnew
    #     allagri<-allagri[!allagri$variableUID==uidrem,]
    
    #keycategories<-keysources()
    allagri<-allagri[!is.na(allagri$sector_number),]
    allcattle<-c("Cattle","Dairy Cattle","Non-Dairy Cattle")
    allagri$option<-""
    allagri<-unique(allagri)
    
    source("agrichecks1ADs.r")
    source("agrichecks2Nex.r")
    source("agrichecks3.r")
    source("eugirp_checklulucf.r")
    
    # Write out list of issues ####
    checks$correction<-1
    checks[,resolved]<-""
    sel<-grepl("agrichecks",checks$val)
    checks$val<-paste0("=HYPERLINK(\"",checks$val,"\")")
     
    #agrichecks<-rbind(checks[names(check1)],check1,check2,check3,check4,check5)
    agrichecks<-checks
    #Load now solved issues!
    agrinames<-c("check","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved)
    
    print("Bind climacheck and agricheck")
    agrichecks<-agrichecks[agrichecks$ms!="all",]
    names(agrichecks)<-agrinames
    #climcheck<-filldf(climcheck,names(agrichecks))
    #climcheck<-climcheck[,names(agrichecks)]
    #agrichecks<-filldf(agrichecks,climcheck)
    #agrichecks<-rbind(agrichecks,climcheck)
    agrichecks$party<-as.character(agrichecks$party)
    #agrinames<-c("test","val1","val2","obs","sector_number","category","party","years","range","plot","correction",resolved,docfields)
    # Integrate outcome into paramcheck and to writeoutlierlist!!
    x1<-55;x2<-56
    x1<-1;x2<-nrow(agrichecks)
    test<-lapply(c(x1:x2),function(x) unlist(flags4newissue(agrichecks[x,],"agri",x)))
    test<-Reduce(rbind,test)
    agrichecks<-filldf(agrichecks,flag4issues,fillwith = "")
    agrichecks<-convert2char(agrichecks)
    agrichecks[x1:x2,flag4issues]<-test
    #write.csv(agrichecks[allcheckfields4emrt],file=paste0(issuedir,"agrichecks.csv"))
    
    #agrichecks<-addsolved2check(agrichecks,c("recalc","outlier","timeseries","ne_empty","notakey"))
    agrichecks<-addsolved2check(agrichecks,c("recalc","recalculation","outlier","timeseries","ne_empty","notakey"))
    
    #agrichecks<-filldf(agrichecks,allcheckfields)
    
    write.csv(agrichecks,file=paste0(issuedir,"agrichecks",curdate(),".csv"))
    
    stepsdone<-7+1
    savelist<-c(savelist,"checkuids","agrichecks")
    #save(list=savelist,file=rdatallem)
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    save(list=savelist,file=rdatallem)
    if(nrow(drive_find(paste0("eealocatorplots"))) == 0) drive_mkdir("eealocatorplots")
    if(!cursubm %in% drive_ls("eealocatorplots/")$name) drive_mkdir(cursubm, "eealocatorplots")
    source("eugirp_files2googleddrive.r")
    if(nrow(drive_find(paste0("eealocator_", cursubm, "_clean.RData"))) == 0){
      for(f2d in files2upload){
        drive_upload(media = f2d, 
                     path = as_dribble(paste0("eealocatorplots/", cursubm, "/")), 
                     #name = NULL, type = NULL, 
                     verbose = FALSE)
      }
    }else{
      for(f2d in files2upload){
        drive_update(file = paste0("eealocatorplots/", cursubm, "/", sub('.*\\/', '', f2d)), 
                     media = f2d, 
                     verbose = TRUE)
      }
    }
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>7){
    print(paste0("Step 7: Sector 3 checks 1 already done"))
}

#stop("Step 8 done")
if(stepsdone==8) {
  #nor dentro
    print(paste0("Step ",stepsdone+1,": Comparison with FAO"))
    source("eugirp_faocomparison.r")
    source("eugirp_exportUIDs4capri.r")
    stepsdone<-9
    save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
    save(list=savelist,file=rdatallem)
    #drive_update(paste0("eealocatorplots/", cursubm, "/", "eealocator_", cursubm, "_clean.RData"),
    #             media = rdatallem, 
    #             verbose = FALSE)
    #drive_upload(media = gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem), 
    #             #path = NULL, 
    #             #name = NULL, type = NULL, 
    #             verbose = FALSE)
    source("curplot.r")
}else if(stepsdone>8){
    print(paste0("Step 7: Comparison with FAO already done"))
}
stop("Step 9 done")




# UNCERTAINTY ####
source("eugirp_uncertainty.r")

# A.3 Determine Key categories ####
if(exists("paramchecked")){
    
    #paramchecked<-1
    
    #Note: set paramchecked to 1 if the file has been checked and update file name below
    #      if the issues are written into file, paramchecked is set to 2 and the 
    if(paramchecked==1){
        print(paste0("Step ",checksteps+1,": Write out country issues and list corrections needed for IEF calculation "))
        
        
        listoftext<-paste0(filoutliers,"_listoftext~",format(Sys.time(),format="%Y%m%d-%H%M"),".csv")
        corfile<-paste0(filoutliers,"list_checked.csv")
        con1 <- file(listoftext, open="wt")
        writeLines(paste0("# ",date()),con1)
        writeLines(paste0("# List if issues that need to be added to the EEA review tool.",
                          "\n# Issue-number and other info to be inserted into the file:",
                          "\n","=HYPERLINK(\"",cursubm,"co_checked.csv","\")"),con1)
        writeLines(paste0("# List of country outlier issues to be included in the tool"),con1)
        writeLines(paste0("id",",","file",",","observation",",","question"),con1)
        close(con1)
        corrections<-read.csv(file=corfile,comment.char = "#",header=TRUE)
        corrections<-corrections[!is.na(corrections$cursubm),]
        corrections[is.na(corrections[,"share"])]<-""
        corrections[is.na(corrections[,"effect"])]<-""
        if(!"issueID"%in%names(corrections)) corrections[,"issueID"]<-""
        corrections[,"share"]<-as.character(corrections[,"share"])
        corrections[,"effect"]<-as.character(corrections[,"effect"])
        corrections[,"issueID"]<-as.character(corrections[,"issueID"])
        xyears<-grepl("X....",names(corrections))
        yyears<-gsub("X","",names(corrections)[xyears])
        names(corrections)[xyears]<-yyears
        donotusepar<-subset(corrections,corrections==0,select=c("party","variableUID"))
        
        #correct unit which was wrong (why??)
        #levels(allagri$gas)<-factor(unique(allagri$gas))
        #levels(corrections$gas)<-gases
        allagri$variableUID<-as.character(allagri$variableUID)
        correctgas<-function(uid){
            gas<-unique(allagri$gas[allagri$variableUID==uid])
        }
        corrections$gas<-unlist(lapply(c(1:nrow(corrections)),function(x) 
            correctgas(corrections$variableUID[x])))
        
        
        #resolved=0: unresolved
        #resolved=4: follow-up unresolved issue
        resolved<-corrections[corrections$resolved!=0 & corrections$resolved!=4,]
        notresolved<-corrections[corrections$resolved==0 | corrections$resolved==4,]
        firstissue<-60
        firstissue<-1
        lastissue<-2
        lastissue<-nrow(notresolved)
        notresolved[c(firstissue:lastissue),"issueID"]<-unlist(lapply(c(firstissue:lastissue),function(x) writeoutlierissue(allagri,notresolved[x,],listoftext)))
        
        
        allissues<-rbind(notresolved,resolved)
        
        selection<-grepl(cursubm,allissues$issueID)
        allissues$issueID[selection]<-paste0(allissues$issueID[selection],".csv")
        allissues$issueID[selection]<-unlist(lapply(c(1:sum(selection)), function(x) linkto(allissues$issueID[x])))
        selection<-grepl("png",allissues$plot)
        allissues$plot<-as.character(allissues$plot)
        tmp<-allissues[selection,]
        allissues$plot[selection]<-unlist(lapply(c(1:sum(selection)), function(x) linkto(tmp$plot[x])))
        allissues$plot[!selection]<-""
        
        writeissuelist(allissues,paste0(filoutliers,"_written.csv"))
        
        
        paramchecked<-2
        savelist<-c(savelist,"corrections")
        save(list=savelist,file=rdatallem)
        save(list=savelist,file=gsub(".RData",paste0("_s",stepsdone,"~",figdate,".RData"),rdatallem))
        source("curplot.r")
        print("Comment 'paramchecked<-1'!")
        
    }else if(paramchecked==2){
        print(paste0("Required corrections identified and outlier issues written to files"))
    }else if(paramchecked==0){
        print("Outliers need to be checked and 'paramchecked' set MANUALLY to 1")
    }
}   


