options(warn=2)
options(error=NULL) 

message("\n\nCalculate aggregate values for 'parent' animal types: ")

#addparentvalues<-function(addparentanimal){
allswines<-unique(allagri$category[grepl("^3.A.3",allagri$sector_number)])
allsheeps<-unique(allagri$category[grepl("^3.A.2",allagri$sector_number)])
alldairy<-c("Dairy Cattle","Mature Dairy Cattle","Dairy Cows","Other Cattle.Dairy cattle")
allnondairy<-unique(allagri[meastype=="POP"&grepl("3.A.1",sector_number)&(!category%in%alldairy)]$category)
allnondairy<-allnondairy[allnondairy!="Cattle"]
allcattle <- c(alldairy, allnondairy)
#if(parent=="Sheep") childs<-allsheeps[!allsheeps%in%parent]

allchilds <- c(allsheeps, allswines, alldairy, allnondairy)
allparents <- c("Dairy Cattle", "Non-Dairy Cattle", "Cattle", "Sheep", "Swine")
family <- data.table(
    child = allchilds,
    parent = c(rep("Sheep", length(allsheeps)),
                 rep("Swine", length(allswines)),
                 rep("Dairy Cattle", length(alldairy)),
                 rep("Non-Dairy Cattle", length(allnondairy))
))

# Remove Options from Cattle
# Option C in sector number is already aggregate - remove to not duplicate
# This is: - Category==Cattle with method 'no option'
#          - Category==Other Cattle with method 'Option C'
#          - Category==Cattle with method 'Option C' and 'Option C' also in sector_number
#          
# Options A and B also have aggregates
#          - Category == Cattle with Option [ABC] in sector+number         
#          
#          Attention not to remove information
infomeasures <- c("Documentation box", "Emission factor information", "Method")
allagri <- allagri[!grepl("Option [ABC]", sector_number) | measure %in% infomeasures]

optionCcountries <- unique(allagri[method=="Option C", party])
allagri <- allagri[! (party %in% optionCcountries & method=="no option" & category=="Cattle") | measure %in% infomeasures]
allagri <- allagri[! (party %in% optionCcountries & method=="Option C" & category=="Other Cattle") | measure %in% infomeasures]
#
#  
allagri <- allagri[option %in% c("Option A", "Option B", "Option C"), option := ""]
allagri <- allagri[method %in% c("Option A", "Option B", "Option C", "no option"), method := ""]
allagri <- allagri[, sector_number := gsub(" Option [ABC]", "", sector_number)]

# Duplicate cattle categories so that aggregation is done to Dairy and Non-Dairy
# at the same time as to 'Cattle'
agriNonParents <- allagri[! category %in% c(allparents)]
agriLivestock <- allagri[category %in% c(allchilds, allparents)]
addparentanimal <- merge(agriLivestock, family, by.x="category", by.y="child")
cattle <- addparentanimal[parent %in% allcattle]
cattle <- cattle[, parent := "Cattle"]

addparentanimal <- rbind(addparentanimal, cattle)
addparentanimal <- addparentanimal[parent=="Dairy Cattle", sector_number := paste0(sector_number, ".1")]
addparentanimal <- addparentanimal[parent=="Non-Dairy Cattle", sector_number := paste0(sector_number, ".2")]

# Remove duplicate time series (if an animal type is given as 'Swine' and 'Swine.unspecified')



# Create data table with variables to weight for aggregation and popolation data
m2w <- c(measta2weight, meastb12weight, meastb22weight)
m2s <- setdiff(addparentanimal$meastype, m2w)
addparentanimal <- addparentanimal[, weigh := ifelse(meastype%in% m2w, 1, 0)]
addpar <- melt.data.table(addparentanimal, measure.vars = years, variable.name = "years")

# Population data are sometimes missing for some sectors, but should be equal
# Therefore, the mean of all existing secor_numbers is used
pop <- melt.data.table(addparentanimal[meastype == "POP"], measure.vars = years, variable.name = "years", value.name = "pop")
pop <- pop[, .(pop=mean(pop)), by=.(parent, category, party, years)]
addpar <- merge(addpar, pop, by=c("parent", "category", "party", "years"))
parents <- addpar[, total := value *  pop]
parents <- parents[, .(value=sum(value, na.rm=TRUE), pop=sum(pop, na.rm=TRUE), total=sum(total, na.rm = TRUE)), 
                   by=setdiff(names(parents), c("category", "variableUID", "value", "pop", "total", "notation"))]
parents <- parents[, value := ifelse(weigh==1, total/pop, value)]
setnames(parents, "parent", "category")

# Calculate total ParPop = Value*Pop
dfrom <- paste0(paste(setdiff(names(parents), c("weigh", "pop", "total", "value", "years")), collapse = " + "), " ~ years")
parentsy <- dcast.data.table(parents, as.formula(dfrom), value.var="value")

keyvalues <- intersect(names(parentsy), c(names(agrimeas)))
parentsy <- merge(parentsy, agrimeas, by=keyvalues, all.x=TRUE)

# Generate variableUIDs if they do not yet exist
parentz <- parentsy[! is.na(variableUID)]
parenty <- parentsy[  is.na(variableUID)]
if(! "method" %in% names(parenty)) {parenty$method <- ""}
if(! "method" %in% names(parentz)) {parentz$method <- ""}

x <- parenty[, variableUID := newuid(sector_number,category,meastype,unit,method,source,target,option,gas), 
              by=1:nrow(parenty)]

parents <- rbind(parentz, x)
parents$notation <- ""
allagri <- rbind(agriNonParents, parents, fill=TRUE)
allagri <- unique(allagri)
allagri <- allagri[, sector_number := gsub("\\.$", "", sector_number)]

