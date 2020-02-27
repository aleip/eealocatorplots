source("curplot.r")
require(openxlsx)

getCRFTable4B <- function(crffolder, crfzfile){
  
  openCRF <- function(crffile, cnt){
    a <-as.data.table(read.xlsx(xlsxFile = paste0(crffolder, "/", crffile), 
                                sheet = "Table4.B", 
                                rows = 10:27, cols = 1:18, 
                                colNames = FALSE, 
                                skipEmptyCols = FALSE))
    a <- a[1:(which(grepl("2. Land", a$X1))-1)]
    a <- a[! grepl("B. Total", X1)]
    if(nrow(a) > 1) {
      a <- a[! grepl("1. Cropland", X1)]
    }else{
      a <- a[1, X1 := "OnlyCropland"] 
    }
    a <- a[, c(1, 16)]
    a$party <- cnt
    a$year <- as.numeric(substr(strsplit(basename(crffile), paste0(cnt, "_2020_"))[[1]][2], 1, 4))
    return(a)
    
  }
  
  cnt <- substr(crfzfile, 1, 3)
  unzip(paste0(crffolder, "/", crfzfile), exdir = crffolder, setTimes = TRUE, overwrite = TRUE)
  crffiles <- list.files(crffolder, pattern = paste0(cnt, "_2020.*xlsx"))
  crffile <- crffiles[1]
  table4b <- Reduce(rbind, lapply(1:length(crffiles), function(x) openCRF(paste0(crffiles[x]), cnt)))
  
  unlink(paste0(crffolder, "/", crffiles[! grepl("1990|2018", crffiles)]))
  return(table4b)

}

crffolder <- paste0(invloc, "/../crfs")
crfzips <- list.files(crffolder, pattern = "CRF2020.*zip$")
crfzfile <- crfzips[2]

table4ball <- Reduce(rbind, lapply(1:length(crfzips), function(x) getCRFTable4B(crffolder, crfzips[x])))
table4sum <- table4ball[X16 != "NO"]
table4sum <- table4sum[X16 != "NA"]
table4sum <- table4sum[, X16 := as.numeric(X16)]
table4sum <- table4sum[! is.na(X16)] 
table4sum <- table4sum[, .(party, year, X1, Cstockchange=X16, Closs=min(0, X16)), by=1:nrow(table4sum)]
table4sum <- table4sum[year > 1989]

table4bsum <- table4sum[, lapply(.SD, sum, na.rm=TRUE), by=.(party, year), .SDcols="Closs"]
table4bsum <- dcast.data.table(table4bsum, party ~ year, value.var="Closs")

table4file <- paste0(invloc, "/checks/lulucf", "/table4B1_Clossesmineralsoils.rdata")
save(table4ball, table4bsum, file=table4file)

