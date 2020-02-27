source("curplot.r")
curlivestock <- c("Dairy Cattle", "Non-Dairy Cattle", "Sheep", "Goats", "Swine", "Poultry")
curmeastypes <- c("Milk", "NRATE", "GE", "DIGEST", "MASS")

feedcheck <- as.data.table(allagri)[meastype %in% curmeastypes & category %in% curlivestock]
feedcheck <- melt.data.table(feedcheck, id.vars=c("party", "category", "meastype"), measure.vars=as.character(seq(1990, lastyear, 1)), variable.name="year")
feedcheck <- dcast.data.table(feedcheck, party + category + year ~ meastype, value.var="value")

feedcheck <- feedcheck[, `:=` (
  milk.vs.de = Milk/DIGEST,
  milk.vs.nex = Milk/NRATE,
  mass.vs.nex = MASS/NRATE
)]


milkde <- melt.data.table(feedcheck, id.vars=c("party", "category", "year"), measure.vars=c("Milk", "DIGEST", "milk.vs.de"), variable.name="meastype")[!is.na(value)]
milkdestd <- milkde[, .(stdeva = var(value), mean = mean(value)), by=.(party, category, meastype)]
milkdestd <- milkdestd[, rstd := signif(stdeva/mean, 3)]
milkdestd <- milkdestd[,.(party, category, year = "RVAR", meastype, value = rstd)]
milkde <- rbind(milkde, milkdestd)
milkde <- dcast.data.table(milkde, party + category + meastype ~ year, value.var="value")
milkde <- milkde[, lastvsfirst := .SD/`1990`, .SDcols=lastyear]
milkdecheck <- milkde[, .SD, .SDcols=c("party", "category", "meastype", seq(1990, as.numeric(lastyear)-1, 2), lastyear, "RVAR", "lastvsfirst")]
milkdecheck <- milkdecheck[category != "Non-Dairy Cattle"]
write.csv(milkdecheck, file=paste0(issuedir, "agricheck_feed", "/milk_vs_de~", format(Sys.time(), "%Y%m%d"), ".csv"))

# Check Belgium
feedcheck[party=="BEL" & category == "Dairy Cattle"]
feedcheck[party=="FIN" & category == "Dairy Cattle"] 
feedcheck[party=="GBE" & category == "Dairy Cattle"] 
feedcheck[party=="GRC" & category == "Dairy Cattle"] 
feedcheck[party=="HUN" & category == "Dairy Cattle"] 
feedcheck[party=="IRL" & category == "Dairy Cattle"] 


milknrate <- melt.data.table(feedcheck, id.vars=c("party", "category", "year"), measure.vars=c("Milk", "NRATE", "milk.vs.nex"), variable.name="meastype")[!is.na(value)]
milknratestd <- milknrate[, .(stdeva = var(value), mean = mean(value)), by=.(party, category, meastype)]
milknratestd <- milknratestd[, rstd := signif(stdeva/mean, 3)]
milknratestd <- milknratestd[,.(party, category, year = "RVAR", meastype, value = rstd)]
milknrate <- rbind(milknrate, milknratestd)
milknrate <- dcast.data.table(milknrate, party + category + meastype ~ year, value.var="value")
milknrate <- milknrate[, lastvsfirst := .SD/`1990`, .SDcols=lastyear]
milknratecheck <- milknrate[, .SD, .SDcols=c("party", "category", "meastype", seq(1990, as.numeric(lastyear)-1, 2), lastyear, "RVAR", "lastvsfirst")]
milknratecheck <- milknratecheck[category != "Non-Dairy Cattle"]

milknratecheck <- milknratecheck[, increase := ifelse(lastvsfirst > 1.05, 1, 0)]
lvsfirst <- dcast.data.table(milknratecheck[, .(party, category, meastype, increase)], party + category ~ meastype, value.var="increase")
lvsfirst <- lvsfirst[!is.na(Milk)]
lvsfirst <- lvsfirst[(Milk != NRATE) | (Milk + NRATE == 0)]
lvsfirst <- melt.data.table(lvsfirst, id.vars=c("party", "category"), variable.name="meastype")
milknrate2 <- merge(milknratecheck, lvsfirst[, .(party, category, meastype)], by=c("party", "category", "meastype"))

write.csv(milknrate2, file=paste0(issuedir, "agricheck_feed", "/milk_vs_nrate~", format(Sys.time(), "%Y%m%d"), ".csv"))


massnrate <- melt.data.table(feedcheck, id.vars=c("party", "category", "year"), measure.vars=c("MASS", "NRATE", "mass.vs.nex"), variable.name="meastype")[!is.na(value)]
massnratestd <- massnrate[, .(stdeva = var(value), mean = mean(value)), by=.(party, category, meastype)]
massnratestd <- massnratestd[, rstd := signif(stdeva/mean, 3)]
massnratestd <- massnratestd[,.(party, category, year = "RVAR", meastype, value = rstd)]
massnrate <- rbind(massnrate, massnratestd)
massnrate <- dcast.data.table(massnrate, party + category + meastype ~ year, value.var="value")
massnrate <- massnrate[, lastvsfirst := .SD/`1990`, .SDcols=lastyear]
massnratecheck <- massnrate[, .SD, .SDcols=c("party", "category", "meastype", seq(1990, as.numeric(lastyear)-1, 2), lastyear, "RVAR", "lastvsfirst")]
massnratecheck <- massnratecheck[category != "Non-Dairy Cattle"]

# Threshold 3% increase
massnratecheck <- massnratecheck[, increase := ifelse(lastvsfirst > 1.05, 1, 0)]
lvsfirst <- dcast.data.table(massnratecheck[, .(party, category, meastype, increase)], party + category ~ meastype, value.var="increase")
lvsfirst <- lvsfirst[!is.na(MASS)]
lvsfirst <- lvsfirst[(MASS != NRATE) | (MASS + NRATE == 0)]

# Dairy cattle already covered by Milkcheck
lvsfirst <- lvsfirst[category != "Dairy Cattle"]
# Ignore if both mass and nrate are constant
lvsfirst <- lvsfirst[MASS >0 | NRATE > 0]


massnrate2 <- melt.data.table(lvsfirst, id.vars=c("party", "category"), variable.name="meastype")
massnrate2 <- merge(massnratecheck, massnrate2[, .(party, category, meastype)], by=c("party", "category", "meastype"))

write.csv(massnrate2, file=paste0(issuedir, "agricheck_feed", "/mass_vs_nrate~", format(Sys.time(), "%Y%m%d"), ".csv"))
feedcheck[party=="DEU" & category == "Swine", .(party, category, year, DIGEST, GE, MASS, NRATE, mass.vs.nex)] 
feedcheck[party=="DNM" & category == "Swine", .(party, category, year, DIGEST, GE, MASS, NRATE, mass.vs.nex)] 
feedcheck[party=="ESP" & category == "Swine", .(party, category, year, DIGEST, GE, MASS, NRATE, mass.vs.nex)] 
