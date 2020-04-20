aggregatesector <- function(dt, cat="Non-Dairy Cattle"){
  
  dtnames <- names(dt)
  
  # For multiplication melt data
  b <- melt.data.table(dt, measure.vars = years, variable.name = "years")
  
  # Create data table with variables to weight for aggregation and popolation data
  m2w <- c(measta2weight, meastb12weight, meastb22weight)
  m2s <- setdiff(b$meastype, m2w)
  b <- b[, weigh := ifelse(meastype%in% m2w, 1, 0)]
  
  # Population data are sometimes missing for some sectors, but should be equal
  # Therefore, the mean of all existing secor_numbers is used
  pop <- melt.data.table(dt[meastype == "POP"], measure.vars = years, variable.name = "years", value.name = "pop")
  pop <- pop[, .(pop=mean(pop)), by=.(category, party, years)]
  bpop <- merge(b, pop, by=c("category", "party", "years"))
  parents <- bpop[, total := value *  pop]
  parents <- parents[, .(value=sum(value, na.rm=TRUE), pop=sum(pop, na.rm=TRUE), total=sum(total, na.rm = TRUE)), 
                     by=setdiff(names(parents), c("category", "variableUID", "value", "pop", "total", "notation"))]
  parents <- parents[, value := ifelse(weigh==1, ifelse(pop>0, total/pop, 0), value)]
  parents$category <- cat
  parents$notation <- ""
  parents <- parents[, sector_number := paste0(sector_number, ".2")]
  
  # Calculate total ParPop = Value*Pop
  dfrom <- paste0(paste(setdiff(names(parents), c("weigh", "pop", "total", "value", "years")), collapse = " + "), " ~ years")
  parentsy <- dcast.data.table(parents, as.formula(dfrom), value.var="value")
  
  #Not sure if needed
  #keyvalues <- intersect(names(parentsy), c(names(agrimeas)))
  #parentsy <- merge(parentsy, agrimeas, by=keyvalues, all.x=TRUE)
  
  # Generate variableUIDs if they do not yet exist
  parentz <- unique(parentsy[, .SD, .SDcols = setdiff(names(parentsy), c("party", years))])
  parentz <- parentz[, variableUID := newuid(sector_number,category,meastype,unit,method,source,target,option,gas), by=1:nrow(parentz)]
  
  parentsn <- merge(parentsy, parentz, by=setdiff(names(parentz), "variableUID"))
  parentsn <- parentsn[, .SD, .SDcols=dtnames]
  
  return(parentsn)
}

calcparbypop <- function(dtc, chlds, parent, meastypes, newsector){
  
  dtnames <- names(dtc)
  dtt <- dtc
  
  dtt <- dtt[meastype %in% meastypes & category %in% chlds]
  dtt <- melt.data.table(dtt, measure.vars = years, variable.name = "years")
  
  pop <- melt.data.table(dtc[meastype == "POP" & category %in% c(chlds, parent)], measure.vars = years, variable.name = "years", value.name = "pop")
  pop <- pop[, .(pop=mean(pop)), by=.(category, party, years)]
  
  bpop <- merge(dtt, pop, by=c("category", "party", "years"))
  parents <- bpop[, total := value *  pop]
  # Here do only aggregation by weighing
  parents$weigh <- 1
  
  # If there is a missing value set also population to zero to not 
  # bias the results
  parents <- parents[value==0, pop := 0]
  parents <- parents[, .(value=sum(value, na.rm=TRUE), pop=sum(pop, na.rm=TRUE), total=sum(total, na.rm = TRUE)), 
                     by=setdiff(names(parents), c("sector_number", "category", "variableUID", "value", "pop", "total", "notation"))]
  parents <- parents[, value := ifelse(weigh==1, ifelse(pop>0, total/pop, 0), value)]
  parents$notation <- ""
  parents$category <- parent
  parents$sector_number <- newsector
  
  # Calculate total ParPop = Value*Pop
  dfrom <- paste0(paste(setdiff(names(parents), c("weigh", "pop", "total", "value", "years")), collapse = " + "), " ~ years")
  parentsy <- dcast.data.table(parents, as.formula(dfrom), value.var="value")
  
  # Generate variableUIDs if they do not yet exist
  parentz <- unique(parentsy[, .SD, .SDcols = setdiff(names(parentsy), c("party", years))])
  parentz <- parentz[, variableUID := newuid(sector_number,category,meastype,unit,method,source,target,option,gas), by=1:nrow(parentz)]
  
  parentsn <- merge(parentsy, parentz, by=setdiff(names(parentz), "variableUID"))
  parentsn <- parentsn[, .SD, .SDcols=dtnames]
  
  return(parentsn)
  
}

addBpop <- function(dtp){
  
  # Option B categories no population in Tables B
  bpop <- dtp[meastype=="POP" & sector_number=="3.A.1"]
  bpop1 <- copy(bpop)[, sector_number := gsub("3.A.1", "3.B.1.1", sector_number)]
  bpop2 <- copy(bpop)[, sector_number := gsub("3.A.1", "3.B.2.1", sector_number)]
  bpop <- rbind(bpop1, bpop2)
  
  bvars <- unique(bpop[, .(sector_number,category,meastype,unit,method,source,target,option,gas)])
  bvars <- bvars[, variableUID := newuid(sector_number,category,meastype,unit,method,source,target,option,gas), by=1:nrow(bvars)]
  bpopv <- merge(bpop[, setdiff(names(bpop), "variableUID"), with=FALSE], bvars, by=setdiff(names(bvars), "variableUID"))
  
  return(bpopv)
  
}

load("C:/dev/ghginventory/eealocatorplots/tmpallagri76.rdata")

tmpagri <- allagri


## ADD POPULATION DATA FOR OPTIONS
## --> Population data are availabel at the 'Cattle' Level, with sector_number including the Options (e.g. '3.A.1 Option C' ... category=='Cattle')
##     Population data at lower level are missing for all Options
##     - with exception of 'Other Cattle' for method=='Option C' - which represents the total Cattle population
##                    and  'Cattle' with method == 'no method' - which also represents the total Cattle population

pop <- tmpagri[meastype=="POP"  & grepl("Option [ABC]", method) & !grepl("Option", sector_number)]
pop <- pop[! ((method=="Option C" & category=="Other Cattle") | (method=='' & category=="Cattle"))]

# --> For a good selection of counries, extract countries using Option A, B, or C and use one per group
#     Make a selection to have one per group and check available population data
apart <- unique(tmpagri[method=="Option A"]$party)
bpart <- unique(tmpagri[method=="Option B"]$party)
cpart <- unique(tmpagri[method=="Option C"]$party)
abcex <- c(apart[1], bpart[1], cpart[1])
pop[party %in% abcex, c(1:4, 10:15, 44)][order(party)]

popn <- addBpop(pop)
popn[party %in% abcex, c(1:4, 10:15, 44)][order(party)]

tmpagri <- rbind(tmpagri, popn)
setorder(tmpagri, party, sector_number, category)

################################################################################
# Option A: Dairy and Non-Dairy Cattle already OK
#           Cattle OK, with and without Option flag
################################################################################

secs <- c("3.A.", "3.B.1.", "3.B.2.")
a <- tmpagri[method=="Option A" & grepl(paste(secs, collapse="|"), sector_number)]
tmpagri <- tmpagri[, sector_number := gsub("\\.$", "", sector_number)]
tmpagri <- tmpagri[sector_number%in%paste0(secs, "1") & method=="Option A" & category=="Dairy Cattle", sector_number := paste0(sector_number, ".1")]
tmpagri <- tmpagri[sector_number%in%paste0(secs, "1") & method=="Option A" & category=="Non-Dairy Cattle", sector_number := paste0(sector_number, ".2")]


################################################################################
# Option B: Dairy Cattle = Mature Dairy Cattle
#           Non-Dairy Cattle = Growing Cattle + Other Mature Cattle
#           Cattle: - With Option B in sector_number 
#                   - 'Normal' with 'no option' and without option flag
#                   ==> Delete the Option B and aggregate only Dairy and Non-Dairy
# 
# ---> The results is: Dairy Cattle 'converted' to 3.A.1.1 (all infor kept)
#                      Non-Dairy Cattle 'aggregated' to 3.A.1.2 (notation missing)
#                      Original (Mature and growing) non-dairy kept under 3.A.1 (all info kept)
# 
################################################################################

b <- tmpagri[method=="Option B" & grepl(paste(secs, collapse="|"), sector_number)]
tmpagri <- tmpagri[!(method=="Option B" & grepl(paste(secs, collapse="|"), sector_number))]

bdairy <- b[category=="Mature Dairy Cattle"]
bdairy <- bdairy[, sector_number := paste0(sector_number, ".1")]
bdairy <- bdairy[, category := "Dairy Cattle"]
bcattle <- b[category=="Cattle"] #Don't use
bcattle <- bdairy[, sector_number := gsub(" Option B", "", sector_number)]
b <- b[category!="Mature Dairy Cattle"]
b <- b[category!="Cattle"]
bnondairy <- aggregatesector(b, "Non-Dairy Cattle")

bagg <- rbind(bdairy, rbind(bnondairy, b))

################################################################################
# Option C: Dairy: LUX, MLT, SVN Dairy Cows
#                  POL           Other Cattle.Dairy cattle
#           Cattle: As for Option B - both available
#
# 
# ---> The results is: Dairy Cattle 'converted' to 3.A.1.1 (all infor kept)
#                      Non-Dairy Cattle 'aggregated' to 3.A.1.2 (notation missing)
#                      Original non-dairy kept under 3.A.1 (all info kept)
# 
################################################################################

ccdairy <- c("Dairy Cows", "Other Cattle.Dairy cattle")
ccats <- unique(tmpagri[method=="Option C"]$category)
tmpagri[method=="Option C" & party%in%cpart & meastype=="EM" & sector_number=="3.A.1", c(1:4, 7:15)][order(party)]
#           Other Cattle (Option C) and Cattle (no method) are duplicates
tmpagri <- tmpagri[! (method == "Option C" & category == "Other Cattle")]

c <- tmpagri[method=="Option C" & grepl(paste(secs, collapse="|"), sector_number)]
tmpagri <- tmpagri[! (method == "Option C" & grepl(paste(secs, collapse="|"), sector_number))]
cdairy <- c[category%in%ccdairy]
cdairy <- cdairy[, sector_number := paste0(sector_number, ".1")]
cdairy <- cdairy[, category := "Dairy Cattle"]
ccattle <- c[category=="Cattle"] #Don't use
ccattle <- cdairy[, sector_number := gsub(" Option B", "", sector_number)]
c <- c[!category %in% ccdairy]
c <- c[category!="Cattle"]
cnondairy <- aggregatesector(c, "Non-Dairy Cattle")

cagg <- rbind(cdairy, rbind(cnondairy, c))
bcagg <- rbind(bagg, cagg)
bcagg$option <- ''
bcagg$method <- ""
tmpagri <- rbind(tmpagri, bcagg)


################################################################################
# Add missing parameters for Cattle
# - Table3.As1: GE and YM
################################################################################
mt3as <- c("GEav", "YM",                                              # Table3.As1
           "WEIGHT", "Milk", "WORK", "PREGNANT", "DIGEST", "GE")      # Table3.As2
mt3b1 <- c("MASS", "VSEXC", "B0",                                     # Table3.B(a)s1
           "MCF", "CLIMA")                                            # Table3.B(a)s2
mt3b2 <- c("NRATE")                                                   # Table3.B(b)

t3as <- calcparbypop(dtc = tmpagri, chlds = c("Dairy Cattle", "Non-Dairy Cattle"), parent = "Cattle", newsector = "3.A.1",  meastypes = mt3as)
t3b1 <- calcparbypop(dtc = tmpagri, chlds = c("Dairy Cattle", "Non-Dairy Cattle"), parent = "Cattle", newsector = "3.B.1.1",meastypes = mt3b1)
t3b2 <- calcparbypop(dtc = tmpagri, chlds = c("Dairy Cattle", "Non-Dairy Cattle"), parent = "Cattle", newsector = "3.B.2.1",meastypes = mt3b2)                     
tmpagri <- rbind(tmpagri, rbind(t3as, rbind(t3b1, t3b2)))

#Sheep
sheeps <- setdiff(unique(tmpagri[sector_number=="3.A.2", category]), "Sheep")
t3as <- calcparbypop(dtc = tmpagri, chlds = sheeps, parent = "Sheep", newsector = "3.A.2",  meastypes = mt3as)
t3b1 <- calcparbypop(dtc = tmpagri, chlds = sheeps, parent = "Sheep", newsector = "3.B.1.2",meastypes = mt3b1)
t3b2 <- calcparbypop(dtc = tmpagri, chlds = sheeps, parent = "Sheep", newsector = "3.B.1.2",meastypes = mt3b2)                     
tmpagri <- rbind(tmpagri, rbind(t3as, rbind(t3b1, t3b2)))
#Swine
swines <- setdiff(unique(tmpagri[sector_number=="3.A.3", category]), "Swine")
t3as <- calcparbypop(dtc = tmpagri, chlds = swines, parent = "Swine", newsector = "3.A.3",  meastypes = mt3as)
t3b1 <- calcparbypop(dtc = tmpagri, chlds = swines, parent = "Swine", newsector = "3.B.1.3",meastypes = mt3b1)
t3b2 <- calcparbypop(dtc = tmpagri, chlds = swines, parent = "Swine", newsector = "3.B.2.3",meastypes = mt3b2)                     
tmpagri <- rbind(tmpagri, rbind(t3as, rbind(t3b1, t3b2)))

################################################################################
# Create common variableUID for Dairy and Non-Dairy Cattle so that 
# it can be aggregated
################################################################################

dn <- tmpagri[grepl("3.A.1.[12]|3.B.[12].1.[12]", sector_number)]
tmpagri <- tmpagri[! (grepl("3.A.1.[12]|3.B.[12].1.[12]", sector_number))]
dn$method <- ""
dn$option <- ""
dnnames <- names(dn)
# Generate variableUIDs if they do not yet exist
dnv <- unique(dn[, .SD, .SDcols = setdiff(names(dn), c("party", years, "notation", "variableUID"))])
dnv <- dnv[, variableUID := newuid(sector_number,category,meastype,unit,method,source,target,option,gas), by=1:nrow(dnv)]
dnn <- merge(dn[, .SD, .SDcols=setdiff(dnnames, "variableUID")], dnv, by=setdiff(names(dnv), "variableUID"))
tmpagri <- rbind(tmpagri, dnn)

setorder(tmpagri, sector_number, party, meastype)
source("checkaggregates.r")

## Save options
## 

apart <- as.data.table(apart)
apart$option <- "A"
setnames(apart, "apart", "party")
bpart <- as.data.table(bpart)
bpart$option <- "B"
setnames(bpart, "bpart", "party")
cpart <- as.data.table(cpart)
cpart$option <- "C"
setnames(cpart, "cpart", "party")
cattleoptions <- rbind(apart, rbind(bpart, cpart))
setorder(cattleoptions, party)
cattlesoptionc <- ccats
