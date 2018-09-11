###########################################################################
########                                                       ############
########               Processing Uncertainty Data             ############
########                    Years 2019-...                     ############
########                                                       ############
###########################################################################


# eugirp_uncertainty.r
#
# Created on: Summer 2018
#
# Created by: Xavier Rotllan-Puig (xavi.rotllan.puig@gmail.com)
#
# Description: The aim of this script is to read in and process the 
#              emissions/uncertainties data reported by the countries, and   
#              aggregate them into categories (e.g. Enteric Fermentation, etc).
#              Data from 2012-2018 is not processed here. However it can be 
#              loaded if necessary.
#              The script called "eugirp_uncertainty_2012_2018.r" is the one 
#              to be used to process the old data sets.
#              
# 
# Inputs:
#       - Emissions/Uncertainty data sets reported by the countries. They must be 
#         in a folder called the year to be processed (e.g. 2019), placed in  
#         ~/ghginventory/ecir/uncertainty/
# 
# Outputs:
#       - Some csv files: (1) All row data reported by countries, (2) aggregations of 
#         emissions and (3) aggregations of uncertainties
#       - All these tables together in a .RData file to easily read them in
#         
# References:
#       - 
#          
##------------------------------------------------------------------------------------------


#source("E:/ghginventory/eealocatorplots/curplot.r")

##------------------------------------------------------------------------------------------

library(xlsx)
library(tidyr)


#### Reading in old results (2012-2018) ####
old_data <- "y"
old_data <- "n"

if (old_data == "y") {
  yr1 <- yr - 1
  load(paste0(invloc, "/uncertainty/uncert_all_2012_", yr1, ".RData"), verbose = TRUE)
}

#### Reading in data sets ####

print(paste0("Year: ", yr))

fldr <- paste0(invloc,"/uncertainty/", yr)
setwd(fldr)

fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
fles$files <- rownames(fles)
fles <- fles[, names(fles) %in% c("files", "mtime")]

#countrs <- unique(countries3[!countries3 %in% c("ITA")])

uncert_all1 <- as.data.frame(matrix(nrow = 0, ncol = 0)) 

for (ct in unique(countries3)){
  print(paste0("Reading uncertainty data for country: ", ct))
  #if(ct == "FRK") stop()
  
  fle <- list.files(path = fldr, pattern = paste0("^", ct), full.names = FALSE)
  
  if(length(fle) > 1){
    fle <- as.data.frame(fle)
    names(fle)[1] <- "files"
    fle <- merge(fle, fles, all.x = TRUE)
    fle <- fle[order(fle$mtime, decreasing = TRUE),]
    if(ct == "ISL"){
      fle <- as.vector(fle$files[grepl("without", fle$files)])
    }else{
      fle <- as.vector(fle[1,1])
    }
  }
  
  if(length(fle) == 0){ print(paste0("Nothing imported for ", ct)) ; next }
  
  if(ct == "DEU") {
    sheetName <- "ANNEX VII"
    sheetIndex <- NULL
  }else if(ct == "LTU") {
    sheetIndex <- NULL
    sheetName <- "Article14_unc excl LULUCF"
  }else if(ct == "LVA") {
    sheetIndex <- NULL
    if(yr == "2018") sheetName = "UNC_2016_without_LULUCF"
  }else if(ct == "ROU") {
    sheetIndex <- NULL
    if(yr == "2018") { sheetName <- "Without LULUCF" } else { sheetName <- "MMR IR-Article14" }
  }else{ 
    sheetIndex <- 1
    sheetName <- NULL
  }
  
  
  if (ct == "CYP"){
    tbl <-  read.xlsx(fle, sheetIndex = 2, sheetName = NULL, startRow = 9, as.data.frame = TRUE, header = FALSE)
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA" | tbl == "C" | tbl == "-" | tbl == "ND"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)  #corrct = FALSE  recalculates X7 to X13
    
  }else if(ct == "DNM"){
    tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 10, as.data.frame = TRUE, header = FALSE)
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "C"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    
  }else if(ct == "EST"){
    tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "C"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    
  }else if(ct == "GRC"){
    tbl <- read.xlsx(fle, sheetName = "Uncertainty_per_gas", startRow = 4, as.data.frame = TRUE, header = FALSE)
    tbl <- tbl[substr(tbl[,1], 1, 1) %in% c(1:9), c(1:7)]
    nms <- as.vector(sapply(tbl, is.factor))
    nms[1:2] <- FALSE
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
    
  }else if(ct == "HRV" & yr != "2015" ){
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl$X2 <-  enc2native(as.character(tbl$X2))
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "ND"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    
  }else if(ct == "HUN"){
    tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)
    tbl[,2] <- paste(tbl[,1], tbl[,2], sep = " - ")
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 2:14]
    tbl[tbl == "NO" | tbl == "NO,IE,NA" | tbl == "NA,NO,IE" | tbl == "NO,NA,IE" | tbl == " "] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    names(tbl) <- names(uncert_all1)[-c((length(names(uncert_all1)) - 1), length(names(uncert_all1)))]
    tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    
  }else if(ct == "ITA"){
    tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
    tbl$X1 <- gsub("CH4", "", tbl$X1)
    tbl$X1 <- gsub("N2O", "", tbl$X1)
    tbl$X1 <- gsub("CO2", "", tbl$X1)
    tbl$X1 <- gsub("HFCs", "", tbl$X1)
    tbl$X1 <- gsub("PFCs", "", tbl$X1)
    tbl$X1 <- gsub("SF6", "", tbl$X1)
    tbl$X1 <- gsub("NF3", "", tbl$X1)
    tbl$X1 <- gsub("y-", "y -", tbl$X1)
    tbl$X1 <- gsub(" - $", "", tbl$X1)
    tbl$X1 <- gsub("-$", "", tbl$X1)
    tbl$X1 <- gsub(" -  $", "", tbl$X1)
    tbl$X1 <- gsub("- $", "", tbl$X1)
    tbl$X1 <- gsub("Enteric Fermentation", "3.A - Enteric Fermentation", tbl$X1)
    tbl$X1 <- gsub("Manure Management", "3.B - Manure Management", tbl$X1)
    tbl$X1 <- gsub("Indirect  Emissions from 3.B - Manure Management", "3.B - Indirect Emissions from Manure Management", tbl$X1)
    tbl$X1 <- gsub("Rice cultivations", "3.C - Rice cultivations", tbl$X1)
    tbl$X1 <- gsub("Direct  Emissions from Managed soils", "3.D - Direct Emissions from Managed soils", tbl$X1)
    tbl$X1 <- gsub("Indirect  Emissions from Managed soils", "3.D - Indirect Emissions from Managed soils", tbl$X1)
    tbl$X1 <- gsub("Field burning of agricultural residues", "3.F - Field burning of agricultural residues", tbl$X1)
    tbl$X1 <- gsub("Liming", "3.G - Liming", tbl$X1)
    tbl$X1 <- gsub("Urea application", "3.H - Urea application", tbl$X1)
    
    tbl <-  tbl[!is.na(tbl$X2), 1:13]
    tbl <-  tbl[!is.na(tbl$X3), ]
    #tbl[tbl == "NA" | tbl == "C" | tbl == "-" | tbl == "ND"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    
  }else if(ct == "LVA"){
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
    
  }else if(ct == "NLD"){
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE) 
    
  }else if(ct == "POL"){
    gass <- c("CO2", "CH4", "N2O") 
    tbl1 <- as.data.frame(matrix(ncol = 0, nrow = 0)) 
    categs_long <- read.xlsx("E:/ghginventory/ecir/uncertainty/codings_POL.xlsx", sheetName = "codings_POL_long", as.data.frame = TRUE, header = FALSE)
    categs_short <- read.xlsx("E:/ghginventory/ecir/uncertainty/codings_POL.xlsx", sheetName = "codings_POL_short", as.data.frame = TRUE, header = FALSE)
    for (gs in 1:length(gass)){
      tbl <-  read.xlsx(fle, sheetIndex = gs, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[1:13]
      #tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[tbl == "NA" | tbl == "C" | tbl == "-" | tbl == "ND" | tbl == "NO" | tbl == "NA,NO"] <- NA
      if(any(grepl("4.b    Goats", tbl$X1)))  tbl$X1 <- sub("4.b    Goats", "4.c    Goats", tbl$X1)
      if(any(grepl("4.c     Horses", tbl$X1))) tbl$X1 <- sub("4.c     Horses", "4.d    Horses", tbl$X1)
      if(any(grepl("4.d   Poultry", tbl$X1))) tbl$X1 <- sub("4.d   Poultry", "4.e   Poultry", tbl$X1)
      if(any(grepl("3.a N2O from product uses", tbl$X1))) tbl$X1 <- sub("3.a N2O from product uses", "3. N2O from product uses [Activity in N2O used, EF in t/t]", tbl$X1)
      
      tbl <- tbl[tbl$X1 %in% categs_long$X1, ]
      
      if(nrow(tbl) == nrow(categs_long)){
        tbl <- cbind(categs_long, tbl)
      }else if (nrow(tbl) == nrow(categs_short)){
        tbl <- cbind(categs_short, tbl)
      }else{
        stop("problem with codes of categories... ")
      }
      
      tbl[[3]] <- paste0(tbl[[2]], tbl[[3]])
      tbl <- tbl[, c(3:length(tbl))]
      tbl <- tbl[!is.na(tbl$X4), ]
      
      nms <- c(FALSE, FALSE, as.vector(sapply(tbl[3:13], is.factor)))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x)))
      
      tbl1 <- rbind(tbl1, tbl)
    }
    tbl <- tbl1 ; rm(tbl1)

    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)  
    
  }else if(ct == "ROU"){
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE) 
    
  }else if(ct == "SVK"){
    
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = TRUE) 
    
  }else if(ct == "SVN"){
    tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
    tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    tbl[tbl == "NA"] <- NA
    nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    tbl <- recalc_uncert(tbl = tbl, corrct = FALSE) 
    
  }else{
    
    if(ct == "CZE" & yr %in% c(2016, 2018)) {
      tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[grepl("3.A", tbl$X1), 2] <- "CH4"
      tbl[grepl("3.B", tbl$X1), 2][2] <- "N2O"
      tbl[grepl("3.G Li", tbl$X1), 2] <- "CO2"
      #View(tbl)
      #View(uncert_all1)

    }else{
      tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
    }
    
    tbl[tbl == "NA" | tbl == "C" | tbl == "-" | tbl == "ND"] <- NA
    nms <- as.vector(c(FALSE, FALSE, sapply(tbl[3:13], is.factor)))
    tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
    
    if (ct == "AUT") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)  #corrct = TRUE  corrects X5 and X6 dividing by fctr (default = 100), and recalculates everything
    if (ct == "BEL") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "BGR") tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)  #corrct = FALSE  only recalculates X7 to X13
    if (ct == "CZE") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "DEU") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "ESP") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "FRK"& yr == 2016) tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "IRL") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    if (ct == "ISL") tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
    if (ct == "MLT" & yr == 2016)  tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
    if (ct == "PRT") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
    
  }
  
  tbl$party <- ct
  tbl$year <- yr
  
  uncert_all1 <- rbind(uncert_all1, tbl)
  #View(uncert_all1)
  
} #end for country3



#To find column names
setwd(paste0(invloc, "/uncertainty/", invyear))

fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
fles$files <- rownames(fles)
fles <- fles[, names(fles) %in% c("files", "mtime")]

hdr <-  read.xlsx(tail(fles[grepl("^A", fles$files), 2], 1), sheetName = "MMR IR-Article14", rowIndex = 8, as.data.frame = TRUE, header = FALSE)
names(uncert_all1) <- c(as.vector(as.matrix(hdr[1, 1:13])), "party", "year")
uncert_all1 <- uncert_all1[!is.na(uncert_all1$`Year x emissions or removals`), ]


# saving data
setwd(paste0(invloc, "/uncertainty"))
write.csv(uncert_all1, paste0(invloc, "/uncertainty/uncert_all_", yr, ".csv"), row.names = FALSE)



#### Aggregating Emissions ####

EMagg1 <- as.data.frame(uncert_all1 %>% group_by(party, year) %>% summarise(TotalGHGem = sum(`Year x emissions or removals`, na.rm = TRUE)))

uncert_agri <- uncert_all1[grepl("^3", uncert_all1$`IPCC category/Group`), ]

uncert_agri[, 1] <- gsub("\\.", "", uncert_agri[, 1])
uncert_agri[, 1] <- gsub(" ", "", uncert_agri[, 1])
uncert_agri[, 2] <- gsub("Methan", "CH4", uncert_agri[, 2])
uncert_agri[, 2] <- gsub("CH4 ", "CH4", uncert_agri[, 2])
uncert_agri[, 2] <- gsub(" CO2        ", "CO2", uncert_agri[, 2])
uncert_agri[, 2] <- gsub("Kohlendioxid", "CO2", uncert_agri[, 2])
uncert_agri[, 2] <- gsub("Lachgas", "N2O", uncert_agri[, 2])
uncert_agri[, 2] <- gsub("N2O ", "N2O", uncert_agri[, 2])

uncert_3A_CH4 <- uncert_agri[grepl("^3A", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3A_CH4 <- as.data.frame(uncert_3A_CH4 %>% group_by(party, year) %>% summarise(EntericFerm_3A_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3A_CH4[, 3] <- EMagg_3A_CH4[, 3] * 21
EMagg1 <- merge(EMagg1, EMagg_3A_CH4, by = c("party", "year"), all = TRUE)

uncert_3A_N2O <- uncert_agri[grepl("^3A", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3A_N2O <- as.data.frame(uncert_3A_N2O %>% group_by(party, year) %>% summarise(EntericFerm_3A_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3A_N2O[, 3] <- EMagg_3A_N2O[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3A_N2O, by = c("party", "year"), all = TRUE)


uncert_3B_CH4 <- uncert_agri[grepl("^3B", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3B_CH4 <- as.data.frame(uncert_3B_CH4 %>% group_by(party, year) %>% summarise(ManureMan_3B_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3B_CH4[, 3] <- EMagg_3B_CH4[, 3] * 21
EMagg1 <- merge(EMagg1, EMagg_3B_CH4, by = c("party", "year"), all = TRUE)

uncert_3B_N2O <- uncert_agri[grepl("^3B", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3B_N2O <- as.data.frame(uncert_3B_N2O %>% group_by(party, year) %>% summarise(ManureMan_3B_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3B_N2O[, 3] <- EMagg_3B_N2O[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3B_N2O, by = c("party", "year"), all = TRUE)


uncert_3C_CH4 <- uncert_agri[grepl("^3C", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3C_CH4 <- as.data.frame(uncert_3C_CH4 %>% group_by(party, year) %>% summarise(RiceCult_3C_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3C_CH4[, 3] <- EMagg_3C_CH4[, 3] * 21
EMagg1 <- merge(EMagg1, EMagg_3C_CH4, by = c("party", "year"), all = TRUE)

uncert_3C_N2O <- uncert_agri[grepl("^3C", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3C_N2O <- as.data.frame(uncert_3C_N2O %>% group_by(party, year) %>% summarise(RiceCult_3C_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#uncert_3C_N2O[, 3] <- uncert_3C_N2O[, 3] * 310
EMagg1 <- merge(EMagg1, uncert_3C_N2O, by = c("party", "year"), all = TRUE)


uncert_3D_CH4 <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3D_CH4 <- as.data.frame(uncert_3D_CH4 %>% group_by(party, year) %>% summarise(AgrSoils_3D_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_CH4[, 3] <- EMagg_3D_CH4[, 3] * 21
EMagg1 <- merge(EMagg1, EMagg_3D_CH4, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_tot <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3D_N2O_tot <- as.data.frame(uncert_3D_N2O_tot %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_tot = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_tot[, 3] <- EMagg_3D_N2O_tot[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3D_N2O_tot, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_direct <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[grepl("[Dd]irect", uncert_3D_N2O_direct$`IPCC category/Group`), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[!grepl("[Ii]ndirect", uncert_3D_N2O_direct$`IPCC category/Group`), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[!grepl("[Aa]nimal", uncert_3D_N2O_direct$`IPCC category/Group`), ]
EMagg_3D_N2O_direct <- as.data.frame(uncert_3D_N2O_direct %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_Direct = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_direct[, 3] <- EMagg_3D_N2O_direct[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3D_N2O_direct, by = c("party", "year"), all = TRUE)

if (ct == "POL"){
  uncert_3D_N2O_animalProd <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
  uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[grepl("[Aa]nimal", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
  #uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[!grepl("[Dd]irect", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
}else {
  uncert_3D_N2O_animalProd <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
  uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[grepl("[Aa]nimal", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
  uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[!grepl("[Dd]irect", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
}
EMagg_3D_N2O_animalProd <- as.data.frame(uncert_3D_N2O_animalProd %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_AnimalProd = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_animalProd[, 3] <- EMagg_3D_N2O_animalProd[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3D_N2O_animalProd, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_indirect <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[grepl("[Ii]ndirect", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[!grepl("[Aa]nimal", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
EMagg_3D_N2O_indirect <- as.data.frame(uncert_3D_N2O_indirect %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_Indirect = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_indirect[, 3] <- EMagg_3D_N2O_indirect[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3D_N2O_indirect, by = c("party", "year"), all = TRUE)


uncert_3F_CH4 <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3F_CH4 <- as.data.frame(uncert_3F_CH4 %>% group_by(party, year) %>% summarise(FieldBurning_3F_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_CH4[, 3] <- EMagg_3F_CH4[, 3] * 21
EMagg1 <- merge(EMagg1, EMagg_3F_CH4, by = c("party", "year"), all = TRUE)

uncert_3F_N2O <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3F_N2O <- as.data.frame(uncert_3F_N2O %>% group_by(party, year) %>% summarise(FieldBurning_3F_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_N2O[, 3] <- EMagg_3F_N2O[, 3] * 310
EMagg1 <- merge(EMagg1, EMagg_3F_N2O, by = c("party", "year"), all = TRUE)


## Aggr EU28+ISL
EMagg_eu <- as.data.frame(EMagg1[,-1] %>% group_by(year) %>% summarise_all(sum, na.rm = TRUE))
EMagg_eu$party <- "EU28+ISL"
EMagg_eu <- EMagg_eu[, c("party", names(EMagg_eu)[-length(EMagg_eu)])]

EMagg1 <- rbind(EMagg1, EMagg_eu)


## Aggr Total Agriculture
EMagg1$TotalAgriculture <- apply(EMagg1[, c(4:11, 15:16)], 1, sum, na.rm = TRUE)
EMagg1 <- EMagg1[, c(1:3, 17, 4:16)]
#EMagg_agri <- as.data.frame(uncert_agri %>% group_by(party, year) %>% summarise(TotalAgriculture = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg1 <- merge(EMagg1, EMagg_agri, by = c("party", "year"), all = TRUE)
#View(EMagg1)


#View(EMagg1)
write.csv(EMagg1, paste0(invloc, "/uncertainty/EMagg_all_", yr, ".csv"), row.names = FALSE)



## Calculating percentages of EM (agriculture)

EMagg_perc_1 <- as.matrix(EMagg1[, -c(1:4,11:13)])
EMagg_perc_1[is.na(EMagg_perc_1)] <- 0


EMagg_perc_1 <- round(prop.table(as.matrix(EMagg_perc_1), margin=1)*100, 2)
EMagg_perc2 <- EMagg1[, -c(11:13)]
EMagg_perc2[, -c(1:4)] <- EMagg_perc_1
EMagg_perc2[is.nan(EMagg_perc2)] <- 0

write.csv(EMagg_perc2, paste0(invloc, "/uncertainty/EMagg_all_percent_", yr, ".csv"), row.names = FALSE)



#### Calculating Uncertainty aggregations ####

# uEM: Combined Uncertainty

uEM1 <- as.data.frame(matrix(nrow = 0, ncol = 0))

for (ct in unique(countries3)){
  for (ctg in LETTERS[1:6][-5]){
    for (gs in c("CH4", "N2O")){
      
      uncert_3 <- uncert_agri[grepl(paste0("^3", ctg), uncert_agri$`IPCC category/Group`) & 
                                uncert_agri$Gas == gs &
                                uncert_agri$party == ct, ]
      
      if (nrow(uncert_3) == 0) next
      
      #View(uncert_3)
      relerror <- uncert_3$`Combined uncertainty`
      emission <- uncert_3$`Year x emissions or removals`
      
      
      if (any(grepl("[Dd]airy", uncert_3$`IPCC category/Group`))){ #aggregating Dairy and non-Dairy cattles to Cattle
        
        uncert_3_noDair <- uncert_3[!grepl("[Dd]airy", uncert_3$`IPCC category/Group`), ]
        if (any(grepl("[Cc]attle", uncert_3_noDair$`IPCC category/Group`))){
          relerror <- relerror[!grepl("[Dd]airy", uncert_3$`IPCC category/Group`)]
          emission <- emission[!grepl("[Dd]airy", uncert_3$`IPCC category/Group`)]
        }else{
          relerror_1 <- relerror[grepl("[Dd]airy", uncert_3$`IPCC category/Group`)]
          emission_1 <- emission[grepl("[Dd]airy", uncert_3$`IPCC category/Group`)]
          uncert_EM_cattle <- sumerr(relerror_1, emission_1)
          
          relerror <- c(uncert_EM_cattle, relerror[!grepl("[Dd]airy", uncert_3$`IPCC category/Group`)])
          emission <- c( sum(emission[grepl("[Dd]airy", uncert_3$`IPCC category/Group`)]), emission[!grepl("[Dd]airy", uncert_3$`IPCC category/Group`)])
          
        }
      }
      
      
      
      if(ctg == "D" & gs == "N2O"){ 
        
        #agrregating 3D.N2O.b1 and b2 to 3D.N2O.b
        sel_b12 <- grepl("[Aa]tmospheric", uncert_3$`IPCC category/Group`) | grepl("[Ll]eaching", uncert_3$`IPCC category/Group`)
        if (any(sel_b12)){
          relerror_1 <- relerror[sel_b12]
          emission_1 <- emission[sel_b12]
          uncert_EM_3b <- sumerr(relerror_1, emission_1)
          emiss_EM_3b <- sum(emission[sel_b12])
        }else{
          uncert_EM_3b <- 0
          emiss_EM_3b <- 0
        }
        
        
        #agrregating 3D.N2O.a2
        sel_2abc <- grepl("[Aa]pplied[Tt]o[Ss]oils", uncert_3$`IPCC category/Group`)
        if (any(sel_2abc)){
          relerror_1 <- relerror[sel_2abc]
          emission_1 <- emission[sel_2abc]
          uncert_EM_a2 <- sumerr(relerror_1, emission_1)
          emiss_EM_a2 <- sum(emission[sel_2abc])
        }else{
          uncert_EM_a2 <- 0
          emiss_EM_a2 <- 0
        }
        
        #agrregating 3D.N2O.a
        sel_a <- !sel_2abc & !sel_b12
        if (any(sel_a)){
          relerror_1 <- c(relerror[sel_a], uncert_EM_a2)
          emission_1 <- c(emission[sel_a], emiss_EM_a2 )
          uncert_EM_3a <- sumerr(relerror_1, emission_1)
          emiss_EM_3a <- sum(emission_1)
        }
        
        relerror <- c(uncert_EM_3a, uncert_EM_3b)
        emission <- c(emiss_EM_3a, emiss_EM_3b)
        
        if(exists("uncert_EM_3a")){rm(uncert_EM_3a, emiss_EM_3a)}
        if(exists("uncert_EM_3b")){rm(uncert_EM_3b, emiss_EM_3b)}
        if(exists("uncert_EM_a2")){rm(uncert_EM_a2, emiss_EM_a2)}
        
      }
      
      uncert_EM_agrr <- sumerr(relerror, emission)
      
      uEM_i <- t(c(ct, yr, paste0("3", ctg), gs, paste0("3", ctg, ".", gs, ".0"), uncert_EM_agrr))
      uEM1 <- rbind(uEM1, uEM_i)
      
      
    }#end of for Gas
  }#end fo categories (A-F)
}#end of for country

uEM1 <- spread(uEM1[,-c(3:4)], V5, V6)  #data frame to wide format

uEM1$TotalGHGEm <- NA
uEM1$TotalAgriculture <- NA
uEM1$'3D.N2O_Direct' <- NA
uEM1$'3D.N2O_AnimalProd' <- NA
uEM1$'3D.N2O_Indirect' <- NA
uEM1[uEM1 == '<NA>']  <- NA 
uEM1[, -c(1:2)]  <- lapply(uEM1[, -c(1:2)], function(x) as.numeric(as.character(x)))
#apply(uEM1[, -c(1:2)], 2, sum, na.rm=T)

uEM1 <- uEM1[, order(names(uEM1))]
uEM1 <- uEM1[, c(16:17, 15:14, 1:8, 10, 9, 11:13)]
names(uEM1) <- names(EMagg1)

#View(uEM1)
#View(EMagg1)


EMagg_noEU_LUX_yr <- EMagg1[EMagg1$party %in% countries3[!countries3 %in% c("LUX")] & EMagg1$year %in% yr, ]
uEM_yr <- uEM1[uEM1$party %in% countries3[!countries3 %in% c("LUX")] & uEM1$year %in% yr, ]

mrgd <- merge(uEM_yr, EMagg_noEU_LUX_yr, by = c("party", "year"), all = TRUE)
mrgd[is.na(mrgd)] <- 0
mrgd <- mrgd[order(mrgd[, 1], mrgd[, 2]), ]
uEM_yr <- uEM_yr[order(uEM_yr[, 1], uEM_yr[, 2]), ]

uEM_yr$TotalAgriculture <- unlist(apply(mrgd, 1, function(x){ sumerr(as.numeric(x[c(5:12, 16:17)]), as.numeric(x[c(20:27, 31:32)])) } ))

uncert_agri_yr_LUX <- uncert_agri[uncert_agri$party %in% c("LUX") & uncert_agri$year %in% yr & uncert_agri$Gas %in% c("CH4", "N2O"), ]
uEM_yr_LUX <- as.data.frame(matrix(NA, nrow = length(unique(uncert_agri_yr_LUX$year)), ncol = ncol(uEM_yr)))
names(uEM_yr_LUX) <- names(uEM_yr)
uEM_yr_LUX$party <- "LUX"
uEM_yr_LUX$year <- unique(uncert_agri_yr_LUX$year)
for (y in unique(uncert_agri_yr_LUX$year)){
  uEM_yr_LUX[uEM_yr_LUX$year == y, ]$TotalAgriculture <- sumerr(uncert_agri_yr_LUX[uncert_agri_yr_LUX$year == y, ]$`Combined uncertainty`,
                                                                      uncert_agri_yr_LUX[uncert_agri_yr_LUX$year == y, ]$`Year x emissions or removals`)
}

uEMagg1 <- rbind(uEM_yr, uEM_yr_LUX)

uEMagg1[, c(3:17)] <- uEMagg1[, c(3:17)] * 100

## Fixing errors in the reported values

uEMagg1[uEMagg1$party == "BGR", - c(1:3)] <- uEMagg1[uEMagg1$party == "BGR", - c(1:3)] / 100
uEMagg1[uEMagg1$party == "DNM", - c(1:3)] <- uEMagg1[uEMagg1$party == "DNM", - c(1:3)] / 100
uEMagg1[uEMagg1$party == "FRK" & uEMagg1$year %in% yr, - c(1:3)] <- uEMagg1[uEMagg1$party == "FRK" & uEMagg1$year %in% yr, - c(1:3)] / 100
uEMagg1[uEMagg1$party == "HRV" & uEMagg1$year %in% yr, - c(1:3)] <- uEMagg1[uEMagg1$party == "HRV" & uEMagg1$year %in% yr, - c(1:3)] * 100
uEMagg1[uEMagg1$party == "SWE" & uEMagg1$year %in% yr, - c(1:3)] <- uEMagg1[uEMagg1$party == "SWE" & uEMagg1$year %in% yr, - c(1:3)] / 100



#View(uEMagg1)  


#View(uEMagg1)  
write.csv(uEMagg1, paste0(invloc, "/uncertainty/uEMagg_all_", yr, ".csv"), row.names = FALSE)


#### Appending new data to old ####

if (old_data == "y"){
  uncert_all <- rbind(uncert_all, uncert_all1)
  EMagg <- rbind(EMagg, EMagg1)
  uEMagg <- rbind(uEMagg, uEMagg1)
  EMagg_perc <- rbind(EMagg_perc, EMagg_perc2)
  save(list = c("uncert_all", "EMagg", "EMagg_perc", "uEMagg"),
       file = paste0(invloc, "/uncertainty/uncert_all_2012_", yr, ".RData"))
  
}else{
  uncert_all <- uncert_all1
  EMagg <- EMagg1
  uEMagg <- uEMagg1
  EMagg_perc <- EMagg_perc2
  save(list = c("uncert_all", "EMagg", "EMagg_perc", "uEMagg"),
       file = paste0(invloc, "/uncertainty/uncert_all_", yr, ".RData"))
}






