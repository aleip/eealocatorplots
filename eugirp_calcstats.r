
# EXCLUDE OTHER LIVESTOCK FROM CHECKS - INHOMOGENEOUS COMPOSITION
exclcat <- c("Other Livestock", "Other Other Livestock", "Other Other Other Livestock")
paramdata <- as.data.table(paramdata)
paramdata <- paramdata[ ! category %in% exclcat & meastype != 'POP']

# Calculate statistical moments (call functions) ####
param <- melt.data.table(paramdata[,c(years,"party","variableUID"), with=FALSE], id.vars = c("party", "variableUID"), variable.name = "years")
newcols<-c("min","p25","median","p75","max")
newqant<-c(0, 0.25, 0.5, 0.75, 1)
statsperyear <- param[, as.list(quantile(value, newqant, na.rm=TRUE)), by=.(variableUID, years)]
statsallyear <- param[, as.list(quantile(value, newqant, na.rm=TRUE)), by=.(variableUID)]

#mad: Median Absolute Deviation
# Compute the median absolute deviation, i.e., the (lo-/hi-) median of the absolute deviations from the median, 
# and (by default) adjust by a factor for asymptotically normal consistency.
momenallyear <- param[, .(mad=mad(value, na.rm=TRUE),
                          mean=mean(value, na.rm=TRUE), 
                          sd=sd(value, na.rm=TRUE)), by=.(variableUID)]
statsallyear <- merge(statsallyear, momenallyear, by="variableUID")
paramstats <- merge(agrimeas, statsallyear, by="variableUID")
paramstats <- paramstats[order(sector_number, meastype, gas)]
paramstaty <- merge(agrimeas, statsperyear, by="variableUID")
paramstaty <- paramstaty[order(sector_number, meastype, gas)]
param <- merge(param, statsallyear, by="variableUID")

growth<-as.data.table(growthdata[,c(setdiff(years,"1990"),"party","variableUID"), with=FALSE])
growth <- melt.data.table(growth, id.vars = c("party", "variableUID"), variable.name = "years")
gallyear <- growth[, as.list(quantile(value, newqant, na.rm=TRUE)), by=.(variableUID)]
mallyear <- growth[, .(mad=mad(value, na.rm=TRUE),
                          mean=mean(value, na.rm=TRUE), 
                          sd=sd(value, na.rm=TRUE)), by=.(variableUID)]
gallyear <- merge(gallyear, mallyear, by="variableUID")
growthstats <- merge(agrimeas, gallyear, by="variableUID")
growthstats <- growthstats[order(sector_number, meastype, gas)]

f1 <- paste0(gsub("eealocator_", "agridata", csvfil), "_stats.xlsx")
wb <- createWorkbook()
wbs <- addWorksheet(wb, sheetName = "stats_allyears")
wbs <- writeData(wb, sheet = "stats_allyears", x = "Statistics over all countries and years per variableUID", startRow = 1)
wbs <- writeDataTable(wb, sheet="stats_allyears", x = paramstats, startRow = 2)

wbs <- addWorksheet(wb, sheetName = "stats_singleyears")
wbs <- writeData(wb, sheet = "stats_singleyears", x = "Statistics over all countries per year and per variableUID", startRow = 1)
wbs <- writeDataTable(wb, sheet="stats_singleyears", x = paramstaty, startRow = 2)

wbs <- addWorksheet(wb, sheetName = "growthstats_allyears")
wbs <- writeData(wb, sheet = "growthstats_allyears", x = "Statistics of growth rates over all countries and years per variableUID", startRow = 1)
wbs <- writeDataTable(wb, sheet="growthstats_allyears", x = growthstats, startRow = 2)
saveWorkbook(wb, file = f1, overwrite=TRUE)

