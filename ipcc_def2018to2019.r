

ipcc <- read.csv(file="ipcc_defaults2018.csv")
nrow(ipcc)
View(ipcc)

ipcc_nrate <- ipcc[ipcc$meastype == "NRATE", ]
nrow(ipcc_nrate)
View(ipcc_nrate)

ipcc_ief <- ipcc[ipcc$meastype == "IEF", ]
ipcc_ief <- ipcc_ief[ipcc_ief$gas == "N2O", ]
nrow(ipcc_ief)
View(ipcc_ief)

unique(ipcc_ief$X.1)
unique(ipcc_ief$unit)
unique(ipcc_ief$sector_number)

cat_ief <- sort(as.vector(unique(ipcc_ief$category)))
cat_nrate <- sort(as.vector(unique(ipcc_nrate$category)))

setdiff(cat_ief, cat_nrate)
setdiff(cat_nrate, cat_ief)


ief_nrate <- merge(ipcc_ief, ipcc_nrate[, names(ipcc_nrate) %in% c("category", "IPCC2006", "region")], by = "category", all.x = TRUE)
nrow(ief_nrate)
View(ief_nrate)
ief_nrate$IPCC2006 <- as.numeric(as.character(ief_nrate$IPCC2006.x)) * as.numeric(as.character(ief_nrate$IPCC2006.y))
ief_nrate <- ief_nrate[, !names(ief_nrate) %in% c("IPCC2006.x", "IPCC2006.y")]
ief_nrate$region.x <- ief_nrate$region.y
names(ief_nrate)[12] <- "region"
ief_nrate <- ief_nrate[, - (length(names(ief_nrate)) - 1)]
ief_nrate <- ief_nrate[, names(ipcc)]
ief_nrate$IPCC2006 <- as.factor(ief_nrate$IPCC2006)

View(ief_nrate)

nrow(ipcc)
nrow(ipcc[ipcc$meastype != "IEF" & ipcc$gas != "N2O", ])
ipcc_1 <- ipcc[ipcc$meastype != "IEF" & ipcc$gas != "N2O", ]
ipcc_1 <- rbind(ipcc_1, ief_nrate)

nrow(ipcc_1)
View(ipcc_1)

write.csv(ipcc_1, "ipcc_defaults.csv", row.names = FALSE, quote = FALSE)
ipcc_1 <- read.csv("ipcc_defaults.csv", header = T)
View(ipcc_1)

