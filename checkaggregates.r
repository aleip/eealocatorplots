mainanimals <- fread("main_animaltypes.csv")
wb <- createWorkbook(creator="EU-GIRP")
wf <- paste0(gsub("eealocator_", "agridata", csvfil), "_AnimalTypecheck.xlsx")

test <- tmpagri[meastype=="EM" & gas %in% c("CH4", "N2O")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testem <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "EM")
wbs <- writeDataTable(wb, sheet = "EM", x = testem)

test <- tmpagri[meastype=="POP"]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testpop <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "POP")
wbs <- writeDataTable(wb, sheet = "POP", x = testpop)

test <- tmpagri[meastype=="IEF" & gas %in% c("CH4", "N2O")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testief <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "IEF")
wbs <- writeDataTable(wb, sheet = "IEF", x = testief)

test <- tmpagri[meastype=="GEav" & gas %in% c("no gas")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testGEav <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "GEav")
wbs <- writeDataTable(wb, sheet = "GEav", x = testGEav)

test <- tmpagri[meastype=="YM" & gas %in% c("no gas")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testYM <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "YM")
wbs <- writeDataTable(wb, sheet = "YM", x = testYM)

test <- tmpagri[meastype=="NRATE" & gas %in% c("no gas")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testNRATE <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "NRATE")
wbs <- writeDataTable(wb, sheet = "NRATE", x = testNRATE)

test <- tmpagri[meastype=="MASS" & gas %in% c("no gas")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testMASS <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "MASS")
wbs <- writeDataTable(wb, sheet = "MASS", x = testMASS)

test <- tmpagri[meastype=="B0" & gas %in% c("no gas")]
test <- merge(mainanimals, test, by=c("sector_number", "category"))
testB0 <- dcast(test[,c(1, 2, 10, 14, 15)], sector_number + category ~ party, value.var = "1990" )
wbs <- addWorksheet(wb, sheetName = "B0")
wbs <- writeDataTable(wb, sheet = "B0", x = testB0)


wbw <- saveWorkbook(wb, file=wf, overwrite = TRUE)

testief[, c("sector_number", "category", abcex), with=FALSE]
