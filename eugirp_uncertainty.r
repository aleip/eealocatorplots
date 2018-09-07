#source("E:/ghginventory/eealocatorplots/curplot.r")

##------------------------------------------------------------------------------------------

library(xlsx)
library(tidyr)


#### Reading in data sets ####

if(file.exists(paste0(invloc, "/uncertainty/uncert_all.csv"))){
  uncert_all <- read.csv(paste0(invloc, "/uncertainty/uncert_all.csv"), header = TRUE)
  names(uncert_all) <- c(paste0("X", c(1:13)), "party", "year")
} else {
  uncert_all <- as.data.frame(matrix(nrow = 0, ncol = 0)) 
}


for (y in yr){
  
  if(y %in% unique(uncert_all$year)) next
  
  print(paste0("Year: ", y))
  
  fldr <- paste0(invloc,"/uncertainty/", y)
  setwd(fldr)
  
  fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
  fles$files <- rownames(fles)
  fles <- fles[, names(fles) %in% c("files", "mtime")]
  
  #countrs <- unique(countries3[!countries3 %in% c("ITA")])
  
  for (ct in unique(countries3)){
    print(paste0("Reading uncertainty data for country: ", ct))
    #if(ct == "FRK") stop()
    
    if(ct == "BEL" & y == 2017){
      fle <- list.files(path = fldr, pattern = paste0("^", "Bel"), full.names = FALSE)
    }else{
      fle <- list.files(path = fldr, pattern = paste0("^", ct), full.names = FALSE)
    }
    
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
      if(y == "2015") {sheetName <- "Uncertainty" } else { sheetName <- "ANNEX VII" }
      sheetIndex <- NULL
    }else if(ct == "LTU") {
      sheetIndex <- NULL
      if (y == "2015") {sheetName <- "without LULUCF" }else{ sheetName <- "Article14_unc excl LULUCF" }
    }else if(ct == "LVA") {
      sheetIndex <- NULL
      if(y == "2018") sheetName = "UNC_2016_without_LULUCF"
      if(y == "2017") sheetName = "UNC_2015_without_LULUCF"
      if(y == "2016") sheetName = "LVA_2014_UNCERT_without_LULUCF"
      if(y == "2015") sheetName = "UNC_2013_without_LULUCF"
    }else if(ct == "ROU") {
      sheetIndex <- NULL
      if(y == "2018") { sheetName <- "Without LULUCF" } else { sheetName <- "MMR IR-Article14" }
    }else{ 
      sheetIndex <- 1
      sheetName <- NULL
    }
    
    
    if (ct == "CYP"){
      if(grepl("2018", fldr)){
        tbl <-  read.xlsx(fle, sheetIndex = 2, sheetName = NULL, startRow = 9, as.data.frame = TRUE, header = FALSE)
      }else if(grepl("2016", fldr)){
        tbl <-  read.xlsx(fle, sheetIndex = NULL, sheetName = "2014", startRow = 11, as.data.frame = TRUE, header = FALSE)
      }else if(grepl("2015", fldr)){
        tbl <-  read.xlsx(fle, sheetIndex = NULL, sheetName = "2012", startRow = 4, as.data.frame = TRUE, header = FALSE)
        if(grepl("2015", fldr)) tbl$X1 <- gsub("^3", "9", tbl$X1)
        if(grepl("2015", fldr)) tbl$X1 <- gsub("^4", "3", tbl$X1)
      }
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[tbl == "NA" | tbl == "C" | tbl == "-" | tbl == "ND"] <- NA
      nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
      
      if(grepl("2016", fldr)) tbl[, 3:13] <-  tbl[, 3:13] / 100
      
      tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)  #corrct = FALSE  recalculates X7 to X13
      
    }else if(ct == "DNM"){
      tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 10, as.data.frame = TRUE, header = FALSE)
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[tbl == "C"] <- NA
      nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
      
    }else if(ct == "EST"){
      if(grepl("2018", fldr)){
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)
      }else{
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
      }
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
      
    }else if(ct == "HRV" & y != "2015" ){
      tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
      tbl$X2 <-  enc2native(as.character(tbl$X2))
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[tbl == "ND"] <- NA
      nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
      tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
      
    }else if(ct == "HUN"){
      if(y == "2016"){
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)[,-c(1, 15:16)]
        tbl <- tbl[!is.na(tbl$X3),]
        names(tbl) <- paste0("X", c(1:13))
      } else if(y == "2015"){
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)[,c(1:14)]
        tbl <- tbl[!is.na(tbl$X3),]
        tbl$X2 <- ifelse(substr(tbl$X2, 1, 1) %in% c(1:9), as.vector(tbl$X2), paste0(as.vector(tbl$X1), as.vector(tbl$X2)))
        tbl <- tbl[,-1]
        names(tbl) <- paste0("X", c(1:13))
      }else{
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)
        tbl[,2] <- paste(tbl[,1], tbl[,2], sep = " - ")
        tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 2:14]
      }
      tbl[tbl == "NO" | tbl == "NO,IE,NA" | tbl == "NA,NO,IE" | tbl == "NO,NA,IE" | tbl == " "] <- NA
      nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
      names(tbl) <- names(uncert_all)[-c((length(names(uncert_all)) - 1), length(names(uncert_all)))]
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
      
      if(y == "2016"){
        
        for (gs in 1:length(gass)){
          categs <- read.xlsx("E:/ghginventory/ecir/uncertainty/codings_POL.xlsx", sheetName = paste0("2016_", gs), as.data.frame = TRUE, header = FALSE)
          
          tbl <-  read.xlsx(fle, sheetIndex = gs, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[1:13]
          tbl <-  tbl[1:(nrow(tbl)-7), ]
          tbl[,1] <- categs[,3]
          tbl[tbl == "NA"] <- NA
          tbl[tbl == "NO"] <- NA
          nms <- c(FALSE, FALSE, as.vector(sapply(tbl[3:13], is.factor)))
          tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x)))
          tbl$X9 <- as.numeric(tbl$X9)
          
          tbl1 <- rbind(tbl1, tbl)
        }
        
        tbl <- tbl1 ; rm(tbl1)
        tbl <- tbl[!is.na(tbl$X5), ]
        
      }else{
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
        #tbl <- tbl[!is.na(tbl$X5), ]
        
      }
      
      tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)  
      
    }else if(ct == "ROU"){
      tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
      tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      tbl[tbl == "NA"] <- NA
      nms <- c(FALSE, FALSE, sapply(tbl[3:13], is.factor))
      tbl[, nms] <- lapply(tbl[, nms], function(x) as.numeric(as.character(x))) 
      tbl <- recalc_uncert(tbl = tbl, corrct = FALSE) 
      
    }else if(ct == "SVK"){
      
      if( y == "2018"){
        tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)[,-14]
        tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
      } else if (y == "2017"){
        tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 9, as.data.frame = TRUE, header = FALSE)[,-14]
        tbl$X1 <- gsub("Liming", "3.G Liming", tbl$X1)
        tbl$X1 <- gsub("Urea Application", "3.H Urea Application", tbl$X1)
        tbl$X1 <- gsub("Enteric Fermentation", "3.A Enteric Fermentation", tbl$X1)
        tbl$X1 <- gsub("Manure Management", "3.B Manure Management", tbl$X1)
        tbl$X1 <- gsub("Direct N2O Emissions From Managed Soils", "3.D.1 Direct N2O Emissions From Managed Soils", tbl$X1)
        tbl <-  tbl[tbl$X2 %in% as.vector(unique(tbl$X2))[1:6], 1:13]
      } else {
        tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 9, as.data.frame = TRUE, header = FALSE)[,-14]
        tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), ]
      }
      
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
      
      
      if(ct == "FIN" & y == "2017") fle <- "FIN_MMR-IRArticle14_Template_v4_EEA_FIN-2017-03-15.xlsx"
      
      if (ct == "BEL" & y == "2017"){
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 4, as.data.frame = TRUE, header = FALSE)
        tbl <-  tbl[substr(tbl$X1, 1, 1) %in% c(1:9), 1:13]
        
      }else if(ct == "DEU" & y == "2015") {
        tbl <-  read.xlsx2(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 2, as.data.frame = TRUE, header = FALSE)
        tbl$X1 <- paste0(tbl$X1, tbl$X2) 
        tbl <- tbl[, c(1, 3, 5, 7, 9, 10)]
        names(tbl) <- paste0("X", c(1:6))
        tbl <- tbl[substr(tbl$X1, 1, 1) %in% c(1:9), ]
        tbl[, 7:13] <- NA
        
      }else if(ct == "FRK" & y == "2016") {
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 6, as.data.frame = TRUE, header = FALSE)[,-c(1:2)]
        tbl$X4 <- paste0(tbl$X3, tbl$X4) 
        tbl <- tbl[!tbl$X4 == "NANA", c(2:14)]
        names(tbl) <- names(uncert_all)[1:13]
        
      }else if (ct == "ISL" & y == "2016"){
        fle <- paste0(fldr, "/", fle, "/ISL_MMR-IRArticle14_Uncertainty_2016.xlsx")
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
        tbl$X13 <- paste0(tbl$X4, tbl$X13)
        tbl <- tbl[, c(13:25)]
        tbl$X13 <- gsub("NAAnimal production", "3.D. Animal Production", tbl$X13)
        #tbl <- tbl[!is.na(tbl$X15), ]
        #tbl <- tbl[!is.na(tbl$X14), ]
        tbl <- tbl[tbl$X13 != "NANA", ]
        tbl <- tbl[!is.na(tbl$X17), ]
        tbl$X13 <- gsub("NAConsumption", "2..Consumption", tbl$X13)
        tbl$X13 <- gsub("NASolvent", "2..Solvent", tbl$X13)
        tbl$X13 <- gsub("^NA", "4.. ", tbl$X13)
        tbl <- tbl[substr(tbl$X13, 1, 1) %in% c(1:9), ]
        names(tbl) <- paste0("X", c(1:13))
        tbl[] <- lapply(tbl, gsub, pattern = ",", replacement = "")
        tbl[, 3:13] <- lapply(tbl[, 3:13], function(x) as.numeric(as.character(x)))

      }else if (ct == "MLT" & y == "2016"){
        fle <- paste0(fldr, "/", fle, "/MT_MMR-IRArticle14_Uncert_2016_v2.xlsx")
        tbl <-  read.xlsx(fle, sheetIndex = sheetIndex, sheetName = sheetName, startRow = 11, as.data.frame = TRUE, header = FALSE)
        tbl$X3 <- paste0(tbl$X1, tbl$X3)
        tbl <- tbl[!tbl$X3 == "NANA", c(3:15)]
        tbl <- tbl[!is.na(tbl$X5), ]
        tbl <- tbl[!is.na(tbl$X4), ]
        names(tbl) <- paste0("X", c(1:13))

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
      if (ct == "FRK") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
      if (ct == "IRL") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
      if (ct == "ISL") {
        if(y == "2016"){
          tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
        }else{
          tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
        }
      }
      if (ct == "MLT" & y == 2016)  tbl <- recalc_uncert(tbl = tbl, corrct = FALSE)
      if (ct == "PRT") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
      
    }
    
    tbl$party <- ct
    tbl$year <- y
    
    uncert_all <- rbind(uncert_all, tbl)
    #
    
  } #end for country3
  
} #end for yr


#To find column names
setwd(paste0(invloc, "/uncertainty/", invyear))

fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
fles$files <- rownames(fles)
fles <- fles[, names(fles) %in% c("files", "mtime")]

hdr <-  read.xlsx(tail(fles[grepl("^A", fles$files), 2], 1), sheetName = "MMR IR-Article14", rowIndex = 8, as.data.frame = TRUE, header = FALSE)
names(uncert_all) <- c(as.vector(as.matrix(hdr[1, 1:13])), "party", "year")
uncert_all <- uncert_all[!is.na(uncert_all$`Year x emissions or removals`), ]


# saving data
setwd(paste0(invloc, "/uncertainty"))
write.csv(uncert_all, paste0(invloc, "/uncertainty/uncert_all.csv"), row.names = FALSE)



#### Aggregating Emissions ####
#data1 <- uncert_all

EMagg <- as.data.frame(uncert_all %>% group_by(party, year) %>% summarise(TotalGHGem = sum(`Year x emissions or removals`, na.rm = TRUE)))

uncert_agri <- uncert_all[grepl("^3", uncert_all$`IPCC category/Group`), ]

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
EMagg <- merge(EMagg, EMagg_3A_CH4, by = c("party", "year"), all = TRUE)

uncert_3A_N2O <- uncert_agri[grepl("^3A", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3A_N2O <- as.data.frame(uncert_3A_N2O %>% group_by(party, year) %>% summarise(EntericFerm_3A_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3A_N2O[, 3] <- EMagg_3A_N2O[, 3] * 310
EMagg <- merge(EMagg, EMagg_3A_N2O, by = c("party", "year"), all = TRUE)


uncert_3B_CH4 <- uncert_agri[grepl("^3B", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3B_CH4 <- as.data.frame(uncert_3B_CH4 %>% group_by(party, year) %>% summarise(ManureMan_3B_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3B_CH4[, 3] <- EMagg_3B_CH4[, 3] * 21
EMagg <- merge(EMagg, EMagg_3B_CH4, by = c("party", "year"), all = TRUE)

uncert_3B_N2O <- uncert_agri[grepl("^3B", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3B_N2O <- as.data.frame(uncert_3B_N2O %>% group_by(party, year) %>% summarise(ManureMan_3B_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3B_N2O[, 3] <- EMagg_3B_N2O[, 3] * 310
EMagg <- merge(EMagg, EMagg_3B_N2O, by = c("party", "year"), all = TRUE)


uncert_3C_CH4 <- uncert_agri[grepl("^3C", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3C_CH4 <- as.data.frame(uncert_3C_CH4 %>% group_by(party, year) %>% summarise(RiceCult_3C_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3C_CH4[, 3] <- EMagg_3C_CH4[, 3] * 21
EMagg <- merge(EMagg, EMagg_3C_CH4, by = c("party", "year"), all = TRUE)

uncert_3C_N2O <- uncert_agri[grepl("^3C", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3C_N2O <- as.data.frame(uncert_3C_N2O %>% group_by(party, year) %>% summarise(RiceCult_3C_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#uncert_3C_N2O[, 3] <- uncert_3C_N2O[, 3] * 310
EMagg <- merge(EMagg, uncert_3C_N2O, by = c("party", "year"), all = TRUE)


uncert_3D_CH4 <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3D_CH4 <- as.data.frame(uncert_3D_CH4 %>% group_by(party, year) %>% summarise(AgrSoils_3D_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_CH4[, 3] <- EMagg_3D_CH4[, 3] * 21
EMagg <- merge(EMagg, EMagg_3D_CH4, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_tot <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3D_N2O_tot <- as.data.frame(uncert_3D_N2O_tot %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_tot = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_tot[, 3] <- EMagg_3D_N2O_tot[, 3] * 310
EMagg <- merge(EMagg, EMagg_3D_N2O_tot, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_direct <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[grepl("[Dd]irect", uncert_3D_N2O_direct$`IPCC category/Group`), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[!grepl("[Ii]ndirect", uncert_3D_N2O_direct$`IPCC category/Group`), ]
uncert_3D_N2O_direct <- uncert_3D_N2O_direct[!grepl("[Aa]nimal", uncert_3D_N2O_direct$`IPCC category/Group`), ]
EMagg_3D_N2O_direct <- as.data.frame(uncert_3D_N2O_direct %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_Direct = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_direct[, 3] <- EMagg_3D_N2O_direct[, 3] * 310
EMagg <- merge(EMagg, EMagg_3D_N2O_direct, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_animalProd <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[grepl("[Aa]nimal", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[!grepl("[Dd]irect", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
EMagg_3D_N2O_animalProd <- as.data.frame(uncert_3D_N2O_animalProd %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_AnimalProd = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_animalProd[, 3] <- EMagg_3D_N2O_animalProd[, 3] * 310
EMagg <- merge(EMagg, EMagg_3D_N2O_animalProd, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_indirect <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[grepl("[Ii]ndirect", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[!grepl("[Aa]nimal", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
EMagg_3D_N2O_indirect <- as.data.frame(uncert_3D_N2O_indirect %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_Indirect = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3D_N2O_indirect[, 3] <- EMagg_3D_N2O_indirect[, 3] * 310
EMagg <- merge(EMagg, EMagg_3D_N2O_indirect, by = c("party", "year"), all = TRUE)


uncert_3F_CH4 <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3F_CH4 <- as.data.frame(uncert_3F_CH4 %>% group_by(party, year) %>% summarise(FieldBurning_3F_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_CH4[, 3] <- EMagg_3F_CH4[, 3] * 21
EMagg <- merge(EMagg, EMagg_3F_CH4, by = c("party", "year"), all = TRUE)

uncert_3F_N2O <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3F_N2O <- as.data.frame(uncert_3F_N2O %>% group_by(party, year) %>% summarise(FieldBurning_3F_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_N2O[, 3] <- EMagg_3F_N2O[, 3] * 310
EMagg <- merge(EMagg, EMagg_3F_N2O, by = c("party", "year"), all = TRUE)


## Aggr EU28+ISL
EMagg_eu <- as.data.frame(EMagg[,-1] %>% group_by(year) %>% summarise_all(sum, na.rm = TRUE))
EMagg_eu$party <- "EU28+ISL"
EMagg_eu <- EMagg_eu[, c("party", names(EMagg_eu)[-length(EMagg_eu)])]

EMagg <- rbind(EMagg, EMagg_eu)


## Aggr Total Agriculture
EMagg$TotalAgriculture <- apply(EMagg[, c(4:11, 15:16)], 1, sum, na.rm = TRUE)
EMagg <- EMagg[, c(1:3, 17, 4:16)]
#EMagg_agri <- as.data.frame(uncert_agri %>% group_by(party, year) %>% summarise(TotalAgriculture = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg <- merge(EMagg, EMagg_agri, by = c("party", "year"), all = TRUE)
View(EMagg)




## Adding years 2012 - 2014
yrs <- c(2012:2014)

for (y in yrs){
  #y <- yrs[3]
  print(paste0("Year: ", y))
  
  y1 <- y - 2
  
  if(y1 != 2012){
    emgg <-  read.xlsx(paste0(y1, ".EC-IR.4_uncertainty.Tier1_level.xls"), sheetName = "EMagg", startRow = 3, as.data.frame = TRUE, header = FALSE)
    emgg <-  merge(country4sub[, 1:2], emgg, by.x = "code2", by.y = "X1", all.y = TRUE)[-1]
    names(emgg)[1] <- "X1"
    emgg$X1 <- gsub("GBK", "GBE", emgg$X1)
    emgg <- emgg[emgg$X1 %in% countries3, paste0("X", c(1, 3:13))]
    emgg$X3 <- y
    emgg1 <- as.data.frame(matrix(0, ncol = 3, nrow = nrow(emgg)))
    emgg <- cbind(emgg, emgg1)
    emgg$W1 <- 0
    emgg$W2 <- 0
    emgg <- emgg[ , c(1:5, 16, 6:8, 17, 9:10, 13:15, 11:12)]
    names(emgg) <- names(EMagg)
  } else {
    emgg <-  read.xlsx(paste0(y1, ".EC-IR.4_uncertainty.Tier1_level.xls"), sheetName = "EMagg", startRow = 6, as.data.frame = TRUE, header = FALSE)
    emgg <-  merge(country4sub[, 1:2], emgg, by.x = "code2", by.y = "X1", all.y = TRUE)[-1]
    names(emgg)[1] <- "X1"
    emgg$X1 <- gsub("GBK", "GBE", emgg$X1)
    emgg <- emgg[emgg$X1 %in% countries3, paste0("X", c(1, 4:16))]
    emgg$X3 <- y
    emgg$W1 <- 0
    emgg$W2 <- 0
    emgg <- emgg[ , c(1, 15, 2:4, 16, 5:7, 17, 8:14)]
    names(emgg) <- names(EMagg)
  }
  
  nms <- as.vector(c(FALSE, FALSE, sapply(emgg[3:15], is.factor)))
  emgg[, nms] <- as.numeric(as.character(emgg[, nms]))
  
  
  EMagg_eu <- as.data.frame(emgg[,-1] %>% group_by(year) %>% summarise_all(sum, na.rm = TRUE))
  EMagg_eu$party <- "EU27"
  EMagg_eu <- EMagg_eu[, c("party", names(EMagg_eu)[-length(EMagg_eu)])]
  
  EMagg <- rbind(EMagg, emgg, EMagg_eu)
  
}


View(EMagg)

write.csv(EMagg, paste0(invloc, "/uncertainty/EMagg_all.csv"), row.names = FALSE)



## Calculating percentages of EM (agriculture)

EMagg_perc_1 <- as.matrix(EMagg[, -c(1:4,11:13)])
EMagg_perc_1[is.na(EMagg_perc_1)] <- 0


EMagg_perc_1 <- round(prop.table(as.matrix(EMagg_perc_1), margin=1)*100, 2)
EMagg_perc <- EMagg[, -c(11:13)]
EMagg_perc[, -c(1:4)] <- EMagg_perc_1
EMagg_perc[is.nan(EMagg_perc)] <- 0

write.csv(EMagg_perc, paste0(invloc, "/uncertainty/EMagg_all_percent.csv"), row.names = FALSE)



#### Calculating Uncertainty aggregations ####

# uEM: Combined Uncertainty

uEM <- as.data.frame(matrix(nrow = 0, ncol = 0))

for (y in yr){
  for (ct in unique(countries3)){
    for (ctg in LETTERS[1:6][-5]){
      for (gs in c("CH4", "N2O")){
        
        uncert_3 <- uncert_agri[grepl(paste0("^3", ctg), uncert_agri$`IPCC category/Group`) & 
                                   uncert_agri$Gas == gs &
                                   uncert_agri$party == ct &
                                   uncert_agri$year == y,]
        
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
          sel_b12 <- grepl("[Aa]tmospheric", uncert_3$`IPCC category/Group`) | grepl("[L]eaching", uncert_3$`IPCC category/Group`)
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

        uEM_i <- t(c(ct, y, paste0("3", ctg), gs, paste0("3", ctg, ".", gs, ".0"), uncert_EM_agrr))
        uEM <- rbind(uEM, uEM_i)
       

      }#end of for Gas
    }#end fo categories (A-F)
  }#end of for country
}#end of for year

uEM <- spread(uEM[,-c(3:4)], V5, V6)  #data frame to wide format

uEM$TotalGHGEm <- NA
uEM$TotalAgriculture <- NA
uEM$'3D.N2O_Direct' <- NA
uEM$'3D.N2O_AnimalProd' <- NA
uEM$'3D.N2O_Indirect' <- NA
uEM[uEM == '<NA>']  <- NA 
uEM[, -c(1:2)]  <- lapply(uEM[, -c(1:2)], function(x) as.numeric(as.character(x)))
#apply(uEM[, -c(1:2)], 2, sum, na.rm=T)

uEM <- uEM[, order(names(uEM))]
uEM <- uEM[, c(16:17, 15:14, 1:8, 10, 9, 11:13)]
names(uEM) <- names(EMagg)

View(uEM)
View(EMagg)


EMagg_noEU_LUX_15_18 <- EMagg[EMagg$party %in% countries3[!countries3 %in% c("LUX")] & EMagg$year %in% c(2015:2018), ]
uEM_15_18 <- uEM[uEM$party %in% countries3[!countries3 %in% c("LUX")] & uEM$year %in% c(2015:2018), ]

mrgd <- merge(uEM_15_18, EMagg_noEU_LUX_15_18, by = c("party", "year"), all = TRUE)
mrgd[is.na(mrgd)] <- 0
mrgd <- mrgd[order(mrgd[, 1], mrgd[, 2]), ]
uEM_15_18 <- uEM_15_18[order(uEM_15_18[, 1], uEM_15_18[, 2]), ]

uEM_15_18$TotalAgriculture <- unlist(apply(mrgd, 1, function(x){ sumerr(as.numeric(x[c(5:12, 16:17)]), as.numeric(x[c(20:27, 31:32)])) } ))

uncert_agri_15_18_LUX <- uncert_agri[uncert_agri$party %in% c("LUX") & uncert_agri$year %in% c(2015:2018) & uncert_agri$Gas %in% c("CH4", "N2O"), ]
uEM_15_18_LUX <- as.data.frame(matrix(NA, nrow = length(unique(uncert_agri_15_18_LUX$year)), ncol = ncol(uEM_15_18)))
names(uEM_15_18_LUX) <- names(uEM_15_18)
uEM_15_18_LUX$party <- "LUX"
uEM_15_18_LUX$year <- unique(uncert_agri_15_18_LUX$year)
for (y in unique(uncert_agri_15_18_LUX$year)){
  uEM_15_18_LUX[uEM_15_18_LUX$year == y, ]$TotalAgriculture <- sumerr(uncert_agri_15_18_LUX[uncert_agri_15_18_LUX$year == y, ]$`Combined uncertainty`,
                                                                      uncert_agri_15_18_LUX[uncert_agri_15_18_LUX$year == y, ]$`Year x emissions or removals`)
}

uEMagg <- rbind(uEM_15_18, uEM_15_18_LUX)

uEMagg[, c(3:17)] <- uEMagg[, c(3:17)] * 100

## Fixing errors in the reported values

uEMagg[uEMagg$party == "BGR", - c(1:3)] <- uEMagg[uEMagg$party == "BGR", - c(1:3)] / 100
uEMagg[uEMagg$party == "DNM", - c(1:3)] <- uEMagg[uEMagg$party == "DNM", - c(1:3)] / 100
uEMagg[uEMagg$party == "HRV" & uEMagg$year %in% c(2016:2017), - c(1:3)] <- uEMagg[uEMagg$party == "HRV" & uEMagg$year %in% c(2016:2017), - c(1:3)] * 100
uEMagg[uEMagg$party == "SWE" & uEMagg$year %in% c(2016, 2018), - c(1:3)] <- uEMagg[uEMagg$party == "SWE" & uEMagg$year %in% c(2016, 2018), - c(1:3)] / 100




View(uEMagg)  

## Adding years 2012 - 2014
yrs <- c(2012:2014)

for (y in yrs){
  print(paste0("Year: ", y))
  
  y1 <- y - 2
  
  if(y1 != 2012){
    uemgg <-  read.xlsx(paste0(y1, ".EC-IR.4_uncertainty.Tier1_level.xls"), sheetName = "uEMagg", startRow = 3, endRow = 17, as.data.frame = TRUE, header = FALSE)
    uemgg <-  merge(country4sub[, 1:2], uemgg, by.x = "code2", by.y = "X1", all.y = TRUE)[-1]
    names(uemgg)[1] <- "X1"
    uemgg$X1 <- gsub("GBK", "GBE", uemgg$X1)
    uemgg$X2 <- y
    uemgg <- uemgg[ , c(1:2, 4:6, 3, 7:9, 3, 10:11, 3, 3, 3, 12:13)]
    names(uemgg) <- names(uEMagg)
    uemgg[uemgg == 0] <- NA

  } else {
    uemgg <-  read.xlsx(paste0(y1, ".EC-IR.4_uncertainty.Tier1_level.xls"), sheetName = "uEMagg", startRow = 6, endRow = 20, as.data.frame = TRUE, header = FALSE)[, -c(4, 17)]
    uemgg <-  merge(country4sub[, 1:2], uemgg, by.x = "code2", by.y = "X1", all.y = TRUE)[-1]
    names(uemgg)[1] <- "X1"
    uemgg$X1 <- gsub("GBK", "GBE", uemgg$X1)
    uemgg$X2 <- y
    uemgg <- uemgg[ , c(1:5, 3, 6:8, 3, 9:15)]
    names(uemgg) <- names(uEMagg)
    uemgg[uemgg == 0] <- NA
  }
  
  uemgg[, c(1:2)] <- lapply(uemgg[, c(1:2)], function(x) as.factor(x)) 
  
  #uemgg_eu <- as.data.frame(uemgg[,-1] %>% group_by(year) %>% summarise_all(sum, na.rm = TRUE))
  #uemgg_eu$party <- "EU27"
  #uemgg_eu <- uemgg_eu[, c("party", names(uemgg_eu)[-length(uemgg_eu)])]
  #uEMagg <- rbind(uEMagg, uemgg, uemgg_eu)
  
  uEMagg <- rbind(uEMagg, uemgg)
  
}

View(uEMagg)  
write.csv(uEMagg, paste0(invloc, "/uncertainty/uEMagg_all.csv"), row.names = FALSE)


View(uncert_agri[uncert_agri$party == "ESP" & uncert_agri$year == 2015, ])
View(uncert_agri[uncert_agri$party == "AUT", ])
























