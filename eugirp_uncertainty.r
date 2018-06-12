
##------------------------------------------------------------------------------------------

library(xlsx)
uncert_all <- as.data.frame(matrix(nrow = 0, ncol = 0)) 


for (y in yr){
  print(paste0("Year: ", y))
  fldr <- paste0(invloc,"/uncertainty/", y)
  setwd(fldr)
  
  fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
  fles$files <- rownames(fles)
  fles <- fles[, names(fles) %in% c("files", "mtime")]
  
  #countrs <- unique(countries3[!countries3 %in% c("ITA")])
  
  for (ct in countries3){
    print(paste0("Reading uncertainty data for country: ", ct))
    
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
      }
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
        tbl <- tbl[!is.na(tbl$X5), ]
        
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
      
      if(ct == "DEU" & y == "2015") {
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
        tbl <- tbl[!is.na(tbl$X15), ]
        tbl <- tbl[!is.na(tbl$X14), ]
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
      if (ct == "PRT") tbl <- recalc_uncert(tbl = tbl, corrct = TRUE)
      
    }
    
    tbl$party <- ct
    tbl$year <- y
    
    uncert_all <- rbind(uncert_all, tbl)
    #
    
  }
} #end for yr


setwd(paste0(invloc, "/uncertainty/2018"))

fles <- as.data.frame(file.info(list.files(pattern="*.xlsx")))
fles$files <- rownames(fles)
fles <- fles[, names(fles) %in% c("files", "mtime")]

hdr <-  read.xlsx(tail(fles[grepl("^A", fles$files), 2], 1), sheetIndex = sheetIndex, sheetName = sheetName, rowIndex = 8, as.data.frame = TRUE, header = FALSE)
names(uncert_all) <- c(as.vector(as.matrix(hdr[1, 1:13])), "party", "year")
uncert_all <- uncert_all[!is.na(uncert_all$`Year x emissions or removals`), ]


# saving data
setwd(paste0(invloc, "/uncertainty"))
write.csv(uncert_all, paste0(invloc, "/uncertainty/uncert_all.csv"), row.names = FALSE)



# Aggregating Emissions
#data1 <- uncert_all

EMagg <- as.data.frame(uncert_all %>% group_by(party, year) %>% summarise(TotalGHGem = sum(`Year x emissions or removals`, na.rm = TRUE)))

uncert_agri <- uncert_all[grepl("^3", uncert_all$`IPCC category/Group`), ]
EMagg_agri <- as.data.frame(uncert_agri %>% group_by(party, year) %>% summarise(TotalAgriculture = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_agri$TotalAgriculture <- NA
EMagg <- merge(EMagg, EMagg_agri, by = c("party", "year"), all = TRUE)

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
EMagg <- merge(EMagg, EMagg_3D_N2O_direct, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_animalProd <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[grepl("[Aa]nimal", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
uncert_3D_N2O_animalProd <- uncert_3D_N2O_animalProd[!grepl("[Dd]irect", uncert_3D_N2O_animalProd$`IPCC category/Group`), ]
EMagg_3D_N2O_animalProd <- as.data.frame(uncert_3D_N2O_animalProd %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_AnimalProd = sum(`Year x emissions or removals`, na.rm = TRUE)))
EMagg <- merge(EMagg, EMagg_3D_N2O_animalProd, by = c("party", "year"), all = TRUE)

uncert_3D_N2O_indirect <- uncert_agri[grepl("^3D", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[grepl("[Ii]ndirect", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
uncert_3D_N2O_indirect <- uncert_3D_N2O_indirect[!grepl("[Aa]nimal", uncert_3D_N2O_indirect$`IPCC category/Group`), ]
EMagg_3D_N2O_indirect <- as.data.frame(uncert_3D_N2O_indirect %>% group_by(party, year) %>% summarise(AgrSoils_3D_N2O_Indirect = sum(`Year x emissions or removals`, na.rm = TRUE)))
EMagg <- merge(EMagg, EMagg_3D_N2O_indirect, by = c("party", "year"), all = TRUE)


uncert_3F_CH4 <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("CH4"), ]
EMagg_3F_CH4 <- as.data.frame(uncert_3F_CH4 %>% group_by(party, year) %>% summarise(FieldBurning_3F_CH4 = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_CH4[, 3] <- EMagg_3F_CH4[, 3] * 21
EMagg <- merge(EMagg, EMagg_3F_CH4, by = c("party", "year"), all = TRUE)

uncert_3F_N2O <- uncert_agri[grepl("^3F", uncert_agri$`IPCC category/Group`) & uncert_agri$Gas %in% c("N2O"), ]
EMagg_3F_N2O <- as.data.frame(uncert_3F_N2O %>% group_by(party, year) %>% summarise(FieldBurning_3F_N2O = sum(`Year x emissions or removals`, na.rm = TRUE)))
#EMagg_3F_N2O[, 3] <- EMagg_3F_N2O[, 3] * 21
EMagg <- merge(EMagg, EMagg_3F_N2O, by = c("party", "year"), all = TRUE)


## Aggr EU28+ISL
EMagg_eu <- as.data.frame(EMagg[,-1] %>% group_by(year) %>% summarise_all(sum, na.rm = TRUE))
EMagg_eu$party <- "EU28+ISL"
EMagg_eu <- EMagg_eu[, c("party", names(EMagg_eu)[-length(EMagg_eu)])]

EMagg <- rbind(EMagg, EMagg_eu)
View(EMagg)

write.csv(EMagg, paste0(invloc, "/uncertainty/EMagg_all.csv"), row.names = FALSE)


