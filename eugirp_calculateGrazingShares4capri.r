# Calculate Grazing shares for CAPRI disaggregation
# 
require(gdxrrw)

dtagri <- as.data.table(allagri)

mms <- dtagri[gas=="no gas" & grepl("3.B.2", sector_number) & meastype=="NEXC" & category != "Farming"]

# Group animals into cattle, sheep & goats, pigs, poultry
unique(mms[, .(sector_number, category)])
selani <- c("Dairy Cattle", "Non-Dairy Cattle", "Sheep", "Swine", "Poultry", "Goats")
selyrs <- names(mms)[grepl("^[12]", names(mms))]
selcls <- c("category", "source", "party")

mms <- mms[category %in% selani, c(selcls, selyrs), with=FALSE]
mmsm <- melt.data.table(mms, id.vars=c("category", "source", "party"), measure.vars=selyrs, variable.name="year", value.name="value")

# Aggregate Sheep and Goat (as they are merged in CAPRI)
sgt <- mmsm[category %in% c("Sheep", "Goats")]
sgt <- sgt[, sum(value), by = c("source", "party", "year")]
sgt$category <- "SheepGoats"
setnames(sgt, "V1", "value")
mmsm <- rbind(mmsm[! category %in% c("Sheep", "Goats")], sgt[, .(category, source, party, year, value)])

# Calculate total manure excretion 
mmst <- mmsm[, sum(value), by=c("category", "party", "year")]
mmst$source <- "total"
setnames(mmst, "V1", "value")
mmsm <- rbind(mmsm, mmst[, .(category, source, party, year, value)])

# Calculate grazing fraction
mmsd <- dcast.data.table(mmsm, category + party + year ~ source, value.var="value", fill=0)
mmsd <- mmsd[, FracGraz := `Pasture  range and paddock`/total]


# Merge with CAPRI MS 
msmatch <- unique(as.data.table(country4sub[, c("code3", "capri")]))
msmatch <- msmatch[capri !="" & !grepl("EU", capri)]
mmsd <- merge(mmsd, msmatch, by.x="party", by.y="code3")

# Generate map to CAPRI MAACT
m_lvstk <- unique(FracGraz$category)
s_maact <- as.data.table(c("DCOL", "DCOH", "BULL", "BULH", "HEIL", "HEIH", 
             "SCOW", "HEIR", "CAMF", "CAFF", "CAMR", "CAFR", 
             "PIGF", "SOWS", "SHGM", "SHGF", "HENS", "POUF"))
s_maact <- s_maact[, V2 := "NonDairyCattle"]
s_maact <- s_maact[V1 %in% c("DCOL", "DCOH", "BULL", "BULH", "HEIL", "HEIH"), V2 := "DairyCattle"]
s_maact <- s_maact[V1 %in% c("PIGF", "SOWS"), V2 := "Swine"]
s_maact <- s_maact[V1 %in% c("HENS", "POUF"), V2 := "Poultry"]
s_maact <- s_maact[V1 %in% c("SHGM", "SHGF"), V2 := "SheepGoats"]
s_maact <- s_maact[, set:= paste0(V1, " . ", "'", V2, "'")]

sfil <- file("s_fracGraz.gms", open = "w")
writeLines("set s_invcats 'Animal types from inventories for which grazing shares are calculated' /", sfil)
writeLines("'DairyCattle', 'NonDairyCattle', 'Swine', 'Poultry', 'SheepGoats'", sfil)
writeLines("/;\n\n", sfil)
writeLines("set m_invcats2maact(*, *) 'Mapping inventory animal categories to MAACT' /", sfil)
write.table(s_maact[, .(set)], quote=FALSE, row.names=FALSE, col.names=FALSE, sfil)
writeLines("/;\n\n", sfil)
close(sfil)

FracGraz <- dcast.data.table(mmsd, capri + category ~ year, value.var="FracGraz")
FracGraz <- FracGraz[, capri := paste0(capri, "000000")]
FracGraz <- FracGraz[, category := gsub("-", "", gsub(" ", "", category))]

sfil <- file("p_fracGraz.csv", open = "w")
writeLines("$ondelim", sfil)
writeLines("parameter p_fracGraz(*, *, *) 'Grazing shares calculated from inventory data';", sfil)
writeLines("table p_fracGraz(*, *, *)", sfil)
write.csv(FracGraz, quote=FALSE, row.names=FALSE, na="0", sfil)
writeLines(";\n\n", sfil)
writeLines("$offdelim", sfil)
close(sfil)




