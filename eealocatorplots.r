library(ggplot2)
library(reshape2) #required for melt function - for outls
library(data.table)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)
rm(list=objects())
#dev.off()

#Link with EEA locator tool
#########################################

# Define if data from xls-file need to be updated (1) or not (0)
#        Note that the update requires sign. time.
loadnew <- 0

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
invyear<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
csvfil <-"eealocatortest_2014_AllAllAll.csv"
csvfil <- "EEA_GHG_MMR_locator_20150323.cub_2015.csv"
csvfil <- "eealocator_20150115_20150509"
setwd(invyear)

# CSV file generated as follows:
# - Load EEA locator cube in excel pivot table
#   - Report Filter: Submission Year
#   - Column Label: Inventory Year
#   - Row Labels: Variable name, Party code, Notation key
# Export table to csv eealocatortest.csv
# Process csv with eealocator_csv.bash to alldata_allbrf.csv
#   Main tasks
#   - Replace long names with acronyms (defined in excel file meta_data_dimensions.xlsx
#     which needs to be exported as csv meta_data_dimensions.csv)
#   - Combine dimension-columns and remove unnecessary ones
#     Currently: 1. category and source (categorysource)
#                2. measure and gas (measuregas)
#                3. unit
#                4. party
#                5. values for years 1990ff (not those before 1990)

# ---> Read text-file if no RData file exists of if the text file is more recent
rdatfile<-paste0(csvfil,".RData")
if(!file.exists(rdatfile)){
    print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
    save(alldata,file=rdatfile)
}else if(file.info(paste0(csvfil,".txt"))$mtime>file.info(rdatfile)$mtime){
    print(paste0("Load updated",csvfil,".txt and generate new ",rdatfile))
    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
    save(alldata,file=rdatfile)
}else{
    print(paste0("Retrieve ",rdatfile))
    load(rdatfile)
}

# -----------------------------------------------------------------------------
# Keep long text with the exception of 'measure' which is needed to identify
# if it is an activity data, emissions, emission factor or parameter or other
# -----------------------------------------------------------------------------
#alldata$measurelong<-alldata$measure
measureacronyms<-read.csv("metadim_row8measure.txt",stringsAsFactors=FALSE)
temp1<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
alldata<-subset(temp1,select=-dummy)

# Correction of meastypes for specific emission sources

# -----------------------------------------------------------------------------
#
#  ---> Table 3.B(b) is rather complex...
#
# sector_number is read as factor, which does not allow to define new elements 
#               therefore transformation to 'character' required
# -----------------------------------------------------------------------------
alldata$sector_number<-as.character(alldata$sector_number)
alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kt","meastype"]<-"EM"
alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kg N2O/kg N","meastype"]<-"IEF"

alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="Total N volatilised as NH3 and Nox","sector_number"]<-"3.B.2.5 indirect volatilisation"
alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="N lost through leaching and run-off","sector_number"]<-"3.B.2.5 indirect leaching"
alldata[alldata$sector_number=="3.B.2.5" & grepl("Atmospheric deposition",alldata$measure),"sector_number"]<-"3.B.2.5 indirect volatilisation"
alldata[alldata$sector_number=="3.B.2.5" & grepl("Nitrogen leaching",alldata$measure),"sector_number"]<-"3.B.2.5 indirect leaching"
#View(alldata[grepl("3.B.2.5",alldata$sector_number),]) 
alldata[grepl("3.F.1.",alldata$sector_number) & alldata$measure=="Crop  production","meastype"]<-"PROD"



# Load the current task to do
# See file curplot.csv for information on how to set it up
source("curplot.csv")
dotaskdetails<-unlist(strsplit(curtask,","))

print("# Basic selection: source cagegory and gas")
docateg<-dotaskdetails[1]
dosourc<-dotaskdetails[2]
dogases<-dotaskdetails[3]
domeasu<-dotaskdetails[seq(4,length(dotaskdetails),2)]
if(length(dotaskdetails)>4) {
    selmeasu<-as.numeric(dotaskdetails[seq(5,length(dotaskdetails),2)])
}

print(paste(docateg,dosourc,dogases,domeasu))
# If selection is done by category (docateg) then filter by sector_number
if(docateg!="all"){
    # --> If wildcard is used: select also under-categories
    if(substr(docateg,nchar(docateg),nchar(docateg))=="*"){
        docateg<-substr(docateg,1,nchar(docateg)-1)
        select <- substr(alldata$sector_number,0,nchar(docateg))==docateg
        alldata<-alldata[select,]
        docategmulti<-TRUE
    }else{
        select <- alldata$sector_number==docateg
        alldata<-alldata[select,]
        docategmulti<-FALSE
    }
    docategall<-FALSE
}else{
    docategmulti<-TRUE
    docategall<-TRUE
}
# If selection is done by all emissions, then filter by meastype
if(domeasu[1]!="all"){
    select <- (alldata$meastype %in% unique(c(domeasu,"AD")))
    alldata<-alldata[select,]
    domeasuall<-FALSE
}else{
    domeasuall<-TRUE
}

# Select gases
gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O")
allgases<-sort(unique(alldata$gas))
selectGas<-alldata$gas %in% gases2keep
alldata<-alldata[selectGas,]

# -----------------------------------------------------------------------------
#  ---> Category contains sometimes substring of sector_name (e.g. Dairy Cattle)
# -----------------------------------------------------------------------------
# takes too long
alldata<-alldata
alldata$category<-as.character(alldata$category)
alldata$category<-unlist(lapply(c(1:nrow(alldata)),function(x) 
    if(!is.null(alldata$category[x])){
        if(grepl(alldata$category[x],alldata$sector_number[x])){
            ""
        }else{
            alldata$category[x]
        }
    }else{""}
))

# -----------------------------------------------------------------------------
# ---> there are duplicate UIDs...
# -----------------------------------------------------------------------------
alldata$variableUID<-as.character(alldata$variableUID)
duplicateuids<-read.csv("duplicateUIDs.csv",header=FALSE)
names(duplicateuids)<-c("UID","SEC")
for(changeuid in c(1:nrow(duplicateuids))){
    duplicateUID<-as.vector(duplicateuids[changeuid,"UID"])
    duplicateSEC<-as.vector(duplicateuids[changeuid,"SEC"])
    duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
    alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
}

# duplicateUID<-"11A3B4C7-8D11-499F-831B-F86155A901FD"
# duplicateSEC<-"3.D.1.2.a"
# duplicateNEW<-"11A3B4C7-8D11-499F-831B-sector_3D12a"
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
# 
# duplicateUID<-"F193D7FC-75A8-400C-9887-0C96595E8F4F"
# duplicateSEC<-"3.D.1.2.b"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
# 
# duplicateUID<-"AFACA1DF-2ED5-4200-90F1-15AB55296FBA"
# duplicateSEC<-"3.D.1.2.c"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
#     
# duplicateUID<-"A5D9804F-99F0-49A8-9E3C-5422EF839F39"
# duplicateSEC<-"3.B.1.1 Dairy Cattle"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
#     
# duplicateUID<-"F578FB5F-544E-4FDD-A670-FC7E435435C2"
# duplicateSEC<-"3.B.1.1 Non-Dairy Cattle"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
# 
# duplicateUID<-"024D8C68-9662-4D3E-9CF8-494F4E2D909D"
# duplicateSEC<-"4.A"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
# 
# duplicateUID<-"32BB6F98-9B18-4AB9-9319-D42483F7FF30"
# duplicateSEC<-"4.A.1 Biomass Burning"
# duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
# alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW




# -----------------------------------------------------------------------------
# ---> Remove sector_number "-"
# -----------------------------------------------------------------------------
alldata<-alldata[alldata$sector_number!="-",]


# -----------------------------------------------------------------------------
# Create vectors for categories, years, and unique rows (not considering years)
# allcategories is created as 'factor'
# -----------------------------------------------------------------------------
allcategories<-sort(unique(alldata$sector_number))
# allyears is created as 'integer'
allyears<-sort(unique(alldata$year))
allsources<-sort(unique(alldata$source))
allmethods<-sort(unique(alldata$method))
alltargets<-sort(unique(alldata$target))
alloptions<-sort(unique(alldata$option))
allnotations<-sort(unique(alldata$notation))
alltypes<-sort(unique(alldata$type))
allmeasures<-sort(unique(alldata$measure))
alluids<-sort(unique(alldata$variableUID))

# -----------------------------------------------------------------------------
# Store keys into separate data file
# Keep 'alldatanovalues' for reference in case of problems
# -----------------------------------------------------------------------------
alldatanovalues<-as.data.frame(unique(subset(alldata,select=-c(value,year,party,country_name))))
notationkeys<-c("NO","NE","IE","NA")
alldatanotations<-alldata[alldata$notation %in% notationkeys,]
alldata<-alldata[!(alldata$notation %in% notationkeys),]

# ---> Fields giving additional info on the 'method'
fields2merge<-sort(c("category","source","method","target","option","type"))

fields2keep<-c("sector_number","gas","unit","allmethods","party","meastype")

# ---> Fields that will be needed for information but they are not needed 
#      to identify the cells
fields4info<-sort(c("notation","classification","country_name"))
# ---> Fields that are identical per country 
fields4coun<-c("submission_version","submission_year")
listofuniquefields<-sort(c(fields2keep,fields2merge))

# -----------------------------------------------------------------------------
# Replace all 'method' fields which will not be needed individually with
# a field where the content is concantenated and simplified
# -----------------------------------------------------------------------------
alldatauniq<-lapply(fields2merge,function(x) as.vector(alldata[,x]))
alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))

# ---> Concetenate
alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
# ---> Eleminate the 'no' texts .... they do not add information
no<-"no method_|no option_|no source_|no target_|no type_|Additional Information_|Option A_"
t<-gsub(no,"",alldatauniq)
alldatauniq<-as.data.frame(gsub("_$","",t))
listofoptions<-unique(alldatauniq)
names(alldatauniq)<-"allmethods"
if(nrow(alldata)==0){stop("Stop! alldata contains no data!")}
alldata<-cbind(alldata,alldatauniq)
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2merge])
#View(alldatauniq)
#View(t)

# -----------------------------------------------------------------------------
# Replace all fields which are needed to identify the rows (except year)
# a field where the content is concantenated and simplified
# -----------------------------------------------------------------------------
alldatauniq<-lapply(fields2keep,function(x) as.vector(alldata[,x]))
alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))
alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
alldatauniq<-gsub("__","_",alldatauniq)
alldatauniq<-as.data.frame(gsub("_$","",alldatauniq))
names(alldatauniq)<-"unique"
listofuniq<-unique(alldatauniq)
alldata<-cbind(alldata,alldatauniq)

# -----------------------------------------------------------------------------
# Store away columns not needed immediately together with unique fields
# -----------------------------------------------------------------------------
allnotations<-alldata[alldata[,"meastype"]=="METHOD",]
alldata<-alldata[alldata[,"meastype"]!="METHOD",]
subfields<-c(fields4info[! fields4info %in% "notation"],"value")
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% subfields])
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields4coun])
allnotations<-subset(allnotations,select=names(allnotations)[! names(allnotations) %in% fields2keep])

#allinfos<-alldata[alldata[,"meastype"]=="INFO" | alldata[,"allmethods"]=="Additional Information",]
#alldata <-alldata[alldata[,"meastype"]!="INFO" & alldata[,"allmethods"]!="Additional Information",]
allinfos<-alldata[alldata[,"meastype"]=="INFO",]
alldata <-alldata[alldata[,"meastype"]!="INFO",]
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4info])
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields4coun])
allinfos<-subset(allinfos,select=names(allinfos)[! names(allinfos) %in% fields2keep])

alldata4info<-subset(alldata,select=c(fields4info,"unique"))
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4info])

alldata4coun<-subset(alldata,select=c(fields4coun,"unique"))
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields4coun])

measname<-as.matrix(unique(subset(alldata,select=c("meastype","measnameshort","sector_number","measure","variableUID"))))
#alldata<-subset(alldata,select=-measname)

alldata4uniq<-subset(alldata,select=c(fields2keep,"unique","variableUID"))
alldata4uniq<-unique(alldata4uniq)
alldata<-subset(alldata,select=names(alldata)[! names(alldata) %in% fields2keep])

# -----------------------------------------------------------------------------
#Generate data frame with one columns per year and the value in the corresponding column
#zeros in all other columns
#http://stackoverflow.com/questions/11350537/convert-a-factor-column-to-multiple-boolean-columns
# Alternative method could be: #http://stackoverflow.com/questions/9084439/r-colsums-by-group
# -----------------------------------------------------------------------------
temp1<-as.data.frame(model.matrix(~factor(year)-1,data=alldata)*alldata$value)
temp2<-cbind(alldata,temp1)
names(temp2)<-gsub("factor\\(year\\)","",as.vector(names(temp2)))
temp2<-subset(temp2,select=c("unique",allyears[allyears %in% names(temp2)]))

# seems to work better: https://stat.ethz.ch/pipermail/r-help/2008-March/157414.html
#temp3<-lapply(split(temp1,temp1$category),function(x) colSums(x[,-1]))
alldatanames<-as.vector(sort(unique(temp2$unique)))
temp3<-lapply(split(temp2,temp2$unique,drop=TRUE),function(x) colSums(x[,-1],na.rm=TRUE))
temp4<-as.data.frame(Reduce(cbind,temp3))
names(temp4)<-alldatanames
temp4<-as.data.frame(t(temp4))

alldata<-merge(alldata4uniq,temp4,by.x="unique",by.y=0)

#Restrict the years
years2keep<-c(1990:2013)
years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
allcolumns<-names(alldata)
alldata<-subset(alldata,select=allcolumns[! allcolumns %in% years2delete])


toremove<-c("temp1","temp2","temp3","temp4")
rm(toremove)



# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
source("lists.txt")

# Load the current tasks to do
# See file curplot.csv for information on how to set it up
#curtasks <- readLines("curplot.csv")
cursubm <- "201503"
starttasks<-0
dotrendoutl<-1
adempars<-c("AD","EM")


# ---> Create empyt matrix with individual plots to do
focus<-as.data.frame(matrix(NA, nrow=10, ncol=4))
names(focus)<-c("ad","parameter","datatype","focus")
nfocus<-1

################################################################################
#Clean up memory to avoid
torm1<-c("actcount","categorymatrix","eealocator")
torm2<-torm1[torm1 %in% ls()]
if(length(torm2)>0){rm(list=torm2)}

################################################################################

#---> Copy alldata into new matrix for further processing 
#          alldata remains stable from this point onwards
#          alldata is already filtered for category and measures
categorymatrix<-subset(alldata,select=-c(unique));


#Remove rows which are zero for all parties
categorymatrix<-categorymatrix[categorymatrix[,"meastype"]!="",]
categorymatrix<-categorymatrix[categorymatrix[,"meastype"]!="INFO",]


#First create data frame with AD
curunits<-as.vector(unique(subset(categorymatrix,select=unit))[,1])
curparts<-as.vector(unique(subset(categorymatrix,select=party))[,1])
# Analyse available measures
# List of measures which are not numerical
curmeasu<-as.vector(unique(subset(categorymatrix,select=meastype))[,1])
listnomeasures<-c("INFO","METHOD",NA)
curmeasu<-curmeasu[! curmeasu %in% listnomeasures]

curuids<-as.vector(unique(subset(categorymatrix,select=variableUID))[,1])
curmeasuid<-as.vector(unique(subset(categorymatrix,select=c(variableUID,meastype))))

tmp1<-subset(categorymatrix,select=-c(gas,unit,party,meastype,allmethods,variableUID,sector_number))
years<-as.numeric(gsub("X","",gsub("X","",colnames(tmp1))))
nyears<-length(years)
#colnames(categorymatrix)[7:(6+nyears)]<-years

print("#Generate matrix 'focus' of tasks from curplot.csv indicating: ")
# - Activity data (are required for AD plots and IEF plots for weighting)
# - Parameter to plot (AD, EM, IEF, or other factor)
# - Data type (determined automatically: summable (AD) or averageable (IEF))
# - Focus: value plot (as trend), 
#          trend plot (as inter-annual changes), 
#          country plot (range as values from trend)
#if(dotaskdetails[4]=="all"){
#domeasu<-as.vector(curcatmeas$meastype)
#docateg<-as.vector(curcatmeas$sector_number)

#nfocus<-length(domeasu)
focusmatrix<-as.data.frame(unique(subset(categorymatrix,select=c(sector_number,allmethods,gas,meastype,variableUID)))[,1:5])
if(domeasu[1]=="all") domeasu<-as.vector(unique(focusmatrix$meastype))

nfocus<-nrow(focusmatrix)
names(focusmatrix)<-c("cat","met","gas","par","uid")

# For AD-uid is required to calculate weighted IEFs 
#   Problem: sometimes the 'category' contains the level with the AD
#            sometimes AD is differentiated by 'allmethods'
#            if no AD-uid is found (mainly aggregated levels) - delete the row
#            if there is multiple AD (e.g. population, N excretion per MMS) - this must be corrected (i.e. N exc per MMS ==> EM)
focusmatrix$aduid<-unlist(lapply(c(1:nfocus),function(x) 
    if(length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                         focusmatrix[,"met"]==focusmatrix$met[x] & 
                                         focusmatrix[,"par"]=="AD"]))==1){
        #2
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"met"]==focusmatrix$met[x] & focusmatrix[,"par"]=="AD"])
    }else if(length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                               focusmatrix[,"par"]=="AD"]))==1){
        #3
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"par"]=="AD"])
    } else {
        #4
        length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"met"]==focusmatrix$met[x] & focusmatrix[,"par"]=="AD"]))
    }
))
focusmatrix<-focusmatrix[focusmatrix$aduid!=0,]


# Multiple ADs for a measure detected: generate data frame to investigate the problem
multipleads<-focusmatrix[nchar(focusmatrix[,"aduid"])<=30,]
datamultipleaduids<-alldatanovalues[alldatanovalues$variableUID %in% focusmatrix$uid,]
if(nrow(multipleads)>0){
    totads<-sum(as.numeric(multipleads$aduid))
    totmad<-nrow(multipleads)
    multipleaduids<-multipleads$uid
    
    datamultipleadsuid<-as.data.frame(unlist(lapply(c(1:totmad),function(x) rep(multipleads$uid[x],multipleads$aduid[x]))))
    for(tmpnames in c("cat","met","gas","par")){
        print(tmpnames)
        datamultipleadsuid[,tmpnames]<-unlist(lapply(c(1:totmad),function(x) 
            rep(multipleads[x,tmpnames],multipleads$aduid[x])))
    }
    
    datamultipleadsuid$aduid<-unlist(lapply(c(1:totmad),function(x) 
        multipleads$uid[multipleads[,"cat"]==multipleads$cat[x] & multipleads[,"met"]==multipleads$met[x] & multipleads[,"par"]=="AD"]))
    
    datamultipleaduids<-alldatanovalues[alldatanovalues$variableUID %in% datamultipleadsuid$aduid,]
    
    View(datamultipleadsuid)
    View(datamultipleaduids)
    View(multipleads)
    
    stop("Multiple ADs found for the measures",.call=FALSE)
    #Check with: 
    #View(alldatanovalues[grepl("3.B.1.1 Non-Dairy Cattle",alldatanovalues$sector_number),])
    
}
if(nrow(focusmatrix)==0){stop("Nothing to do!")}

if(docategall){
    focusmatrix$allfoci<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*1 + (sum(adempars %in% x)*1) ))
    focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*1 + (sum(adempars %in% x)*1) ))
}else{ 
    focusmatrix$allfoci<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*7 + (sum(adempars %in% x)*3) ))
    focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*3 + (sum(adempars %in% x)*2) ))
}    

focuscols<-sum(focusmatrix$nrep)
focusmeas<-focusmatrix$par
focuscats<-focusmatrix$cat
focusmets<-focusmatrix$met
focusgass<-focusmatrix$gas

focusrows<-c(1:nrow(focusmatrix))

focusfoc<-focusmatrix$allfoci
print("Generate focus-data frame")
focus<-as.data.frame(matrix(NA,nrow=focuscols,ncol=9))
names(focus)<-c("ad","parameter","datatype","focus","category","method","gas","uid","aduid")
focus[,1]<-"AD"
focus[,2]<-unlist(lapply(focusmeas, function(x) rep(x,focusmatrix$nrep[focusmatrix$par==x][1])))
focus[,5]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$cat[x],focusmatrix$nrep[x])))
focus[,6]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$met[x],focusmatrix$nrep[x])))
focus[,7]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$gas[x],focusmatrix$nrep[x])))
focus[,8]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$uid[x],focusmatrix$nrep[x])))
focus[,9]<-unlist(lapply(focusrows, function(x) rep(focusmatrix$aduid[x],focusmatrix$nrep[x])))
focus[,3]<-unlist(lapply(focusmeas, function(x) rep(if(x %in% adempars){"adem"}else{"ief"},focusmatrix$nrep[focusmatrix$par==x][1])))
focus[,4]<-unlist(lapply(focusfoc,  function(x) 2**((which((as.integer(intToBits(x))[1:3]) %in% 1)-1))))

#Select focus        
for (numrun in 1:(focuscols)){
    #        for (numrun in 5:5){
    #numrun<-1            
    print(paste0("#Copy plot-requests to current plot for numrun=",numrun))
    runad   <-as.vector(focus[numrun,1])
    runpar  <-as.vector(focus[numrun,2])
    rundata <-as.vector(focus[numrun,3])
    runfocus<-as.vector(focus[numrun,4])
    runcateg<-as.vector(focus[numrun,5])
    runmethod<-as.vector(focus[numrun,6])
    rungas<-as.vector(focus[numrun,7])
    runuid<-as.vector(focus[numrun,8])
    runaduid<-as.vector(focus[numrun,9])
    runmatrix<-subset(categorymatrix,variableUID==runuid & sector_number==runcateg)
    admatrix<-subset(categorymatrix,variableUID==runaduid & party %in% as.vector(runmatrix$party))
    runparts<-curparts[curparts %in% runmatrix$party]
    
    runtotal<-sum(runmatrix[,as.character(years)])
    adtotal<-nrow(admatrix)
    if(adtotal>0){adtotal<-sum(admatrix[,as.character(years)])}
    
    if(runfocus==1){runfocus<-"value"}
    if(runfocus==2){runfocus<-"trend"}
    if(runfocus==4){runfocus<-"countries"}

    print(paste("Current task ",numrun,". AD=",runad,". Par=",runpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))
    
    #             if(numrun==1){rundata="adem";runfocus="value"}
    #             if(numrun==2){rundata="adem";runfocus="trend"}
    #             if(numrun==3){rundata="ief";runfocus="value"}
    #             if(numrun==4){rundata="ief";runfocus="trend"}
    #             if(numrun==5){rundata="ief";runfocus="countries"}
    if("AD" %in% curmeasu & runtotal!=0 & adtotal!=0){
        tmp1<-subset(admatrix,meastype=="AD" & party %in% runparts,select=-meastype)
        unitad<-as.vector(unique(subset(tmp1,select=unit))[,1])
        uidad<-as.vector(unique(subset(tmp1,select=variableUID))[,1])
        actcount<-subset(tmp1,sector_number==runcateg,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        acteu28<-colSums(actcount)
        if(nrow(actcount)==0){stop("No activity data defined!")}
        if(length(as.vector(tmp1$party))>nrow(actcount)){
            View(admatrix)
            stop("STOP - there are likely duplicate UIDs for AD")
        }
        rownames(actcount)<-as.vector(tmp1$party)
        colnames(actcount)<-years
    }
    
    if(runpar=="EM"){
        tmp1<-subset(categorymatrix,meastype=="EM",select=-meastype)
        tmp1<-subset(runmatrix,meastype=="EM",select=-meastype)
        unitem<-as.vector(unique(subset(tmp1,select=unit))[,1])
        uidem<-as.vector(unique(subset(tmp1,select=variableUID))[,1])
        emicount<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        emieu28<-colSums(emicount)
        rownames(emicount)<-as.vector(tmp1$party)
        colnames(emicount)<-years
    }
    
    if(runpar=="IEF"){
        tmp1<-subset(runmatrix,meastype=="IEF",select=-meastype)
        unitief<-as.vector(unique(subset(tmp1,select=unit))[,1])
        uidief<-as.vector(unique(subset(tmp1,select=variableUID))[,1])
        iefcount<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        iefeu28<-colSums(iefcount*actcount)/acteu28
        rownames(iefcount)<-as.vector(tmp1$party)
        colnames(iefcount)<-years
    }
    
    # Copy data of relevance to the eealocator data frame
    # Get unit for parameter
    if((runpar=="AD") & (runpar %in% curmeasu)){
        eealocator<-actcount
        curunit<-unitad
    } else if((runpar=="EM") & (runpar %in% curmeasu)){
        eealocator<-emicount
        curunit<-unitem
    } else if((runpar=="IEF") & (runpar %in% curmeasu)){
        eealocator<-iefcount
        curunit<-unitief
    } else {
        
        tmp1<-subset(runmatrix,meastype==runpar,select=-meastype)
        curunit<-as.vector(unique(subset(tmp1,select=unit))[,1])
        parparties<-t(as.vector(subset(tmp1,select=party)))
        eealocator<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        row.names(eealocator)<-parparties                
    }
    
    if(sum(eealocator)!=0){
        #rm(list=c("tmp0","tmp1","tmp2","tmp3"))
        
        #print("# FOR TEST PURPOSE: ADD RANDOM NUMBER TO INCREASE NUMBER OF COUNTRIES")
        addrandomcountries<-0
        if(addrandomcountries==1){
            attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
            allcountries<-row.names(attributes)
            allcountries<-allcountries[allcountries != "Other"]
            restcountries<-allcountries[! allcountries %in% party]
            #Add countries
            nnewc<-25
            newcn<-restcountries[sample(1:length(restcountries),nnewc,replace=FALSE)]
            #Multiplicate with random between 0.7 and 1.3 times values
            randommulp<-matrix(runif(nyears*nnewc,min=0.9,max=1.2),ncol=nyears)
            
            pick<-matrix(sample(0:1,nyears*length(party),replace=T),ncol=nyears)
            pick[2,]<-1-pick[1,]
            pick<-as.vector(t(colSums(eealocator*pick)))
            newcv<-randommulp*pick
            rownames(newcv)<-newcn
            colnames(newcv)<-years
            eealocator2<-rbind(eealocator,newcv)
            eealocator<-eealocator2
        }
        
        #print("#Groth rates calculated as Y_i/Y_(i-1) - 1")
        growth<-eealocator[,2:nyears]/eealocator[,1:nyears-1]-1
        
        # see http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
        is.nan.data.frame <- function(x)
            do.call(cbind, lapply(x, is.nan))
        growth[is.nan(growth)] <- 0
        
        is.infinite.data.frame <- function(x)
            do.call(cbind, lapply(x, is.infinite))
        growth[is.infinite(growth)] <- 1
        growth<-round(growth,3)
        
        abstrend<-eealocator[,2:nyears]-eealocator[,1:nyears-1]
        
        if (rundata=="adem"){eeatrend<-abstrend}
        if (rundata=="ief") {eeatrend<-growth}
        if (runfocus=="trend" && runpar=="EM" && "AD" %in% curmeasu && ("IEF" %in% curmeasu)){
            # From run_emi_EU15_data.sh
            # c:\adrian\data\inventories\ghg\unfccc\inventorysystem\eea_locator_tool\plots\scripts\run_emi_EU15_data.sh
            #  pct_act = sprintf("%.0f",100 * ( ACT[country,endy] * ( vstarty / ACT[country,starty] ) - vstarty ) / ( vendy - vstarty ) )
            pct_act1<-100*(actcount[,nyears]*(sum(eealocator[,1])/actcount[,1])-sum(eealocator[,1]))/(sum(eealocator[,nyears])-sum(eealocator[,1]))
            
            # Alternative:
            # DeltaE = En - Ea = ADn * IEFn - ADa * IEFa
            # DeltaE* = (ADn - ADa) * IEFa
            # DeltaE& = (IEFn - IEFa) * ADn
            # DeltaE* + DeltaE& = DeltaE
            # Share AD: DeltaE*/DeltaE
            
            deltaEM_EU28<-sum(eealocator[,nyears])-sum(eealocator[,1])
            deltaEM<-eealocator[,nyears]-eealocator[,1]
            deltaAD<-actcount[,nyears]-actcount[,1]
            deltaIF<-iefcount[,nyears]-iefcount[,1]
            pct_act2<-100*((actcount[,nyears]-actcount[,1])
                           *iefcount[,1])/
                (sum(eealocator[,nyears])-sum(eealocator[,1]))/1000
            
            # Share of AD and IEF on EU absolute trend
            act_pcteu<-100*(deltaAD*iefcount[,1]/1000)/deltaEM_EU28
            ief_pcteu<-100*(deltaIF*actcount[,nyears]/1000)/deltaEM_EU28
            # Share of AD and IEF on country absolute trend
            act_pctcountry<-100*(deltaAD*iefcount[,1]/1000)/deltaEM
            ief_pctcountry<-100*(deltaIF*actcount[,nyears]/1000)/deltaEM
            act_pctcouneu<-100*((acteu28[nyears]-acteu28[1])*iefeu28[1]/1000)/(emieu28[nyears]-emieu28[1])
            ief_pctcouneu<-100*((iefeu28[nyears]-iefeu28[1])*acteu28[nyears]/1000)/(emieu28[nyears]-emieu28[1])
            
        }
        
        
        if(dotrendoutl==1){
            trendoutlmethod<-2
            
            # Save quantiles for growth rate
            growthquantiles<-t(apply(growth, 1, quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE))
            eeaquantiles<-t(apply(eealocator, 1, quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE))
            
            # Median absolute deviation of the absolute deviations from the median
            growthmad<-apply(growth, 1, mad,  na.rm = TRUE)
            eealocatormad<-apply(eealocator, 1, mad,  na.rm = TRUE)
            
            #print("# Mulitply with factor according to outl Tool")
            #growthmadn<-growthmad/0.6745
            
            if(trendoutlmethod==1){
                # Method 1 in outl tool: Deviation from median
                growthmeddevtest<-abs(growth-growthquantiles[2,])>2*growthmadn
                meddevtest<-abs(eealocator-eeaquantiles[2,])>2*eealocatormadn
                
            } else if (trendoutlmethod==2){
                # Method 2 in outl tool
                bxplf <- 0.953
                #uwhisk = mdian + (1 + bxplf) * (uquart - mdian)
                #lwhisk = mdian + (1 + bxplf) * (lquart - mdian)
                growthuwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,3]-growthquantiles[,2])
                growthlwhisk<-growthquantiles[,2]+(1 + bxplf)*(growthquantiles[,1]-growthquantiles[,2])
                growthmeddevtest<-(growth>growthuwhisk) | (growth<growthlwhisk)
                
                uwhisk<-eeaquantiles[,2]+(1 + bxplf)*(eeaquantiles[,3]-eeaquantiles[,2])
                lwhisk<-eeaquantiles[,2]+(1 + bxplf)*(eeaquantiles[,1]-eeaquantiles[,2])
                meddevtest<-(eealocator>uwhisk) | (eealocator<lwhisk)
            }
            growthoutl<-growthmeddevtest*growth
            eealocatoroutl<-meddevtest*eealocator
            ngrowthoutl<-sum(growthoutl!=0,na.rm=TRUE)
            neealocatoroutl<-sum(eealocatoroutl!=0)
            #print("#unlist(growthoutl)")
            
            if(ngrowthoutl>0){
                listofoutls<-which(unlist(growthoutl)!=0)
                growthoutl$country<-row.names(growthoutl)
                growthoutllist<-melt(growthoutl,na.rm=T)[listofoutls,]
                cntyid<-nrow(growth)-(listofoutls %% nrow(growth))
                #get year-ID
                yearid<-(listofoutls-(cntyid))/2+1
                growthoutllist<-cbind(cursubm,runcateg,dosourc,runpar,"growth",
                                      growthoutllist,growthquantiles[cntyid,])
            }
            #paste0(runcateg,dosourc,runpar)
        }
        
        
        
        
        topn<-10
        
        #print("#Define trend data frame")
        if (runfocus=="trend"){curmatrix<-eeatrend}
        if (runfocus=="value"){curmatrix<-eealocator}
        if (runfocus=="countries"){curmatrix<-eealocator}
        
        if(rundata=="adem"){
            #print("Calculate EU data")
            # Value-plot
            eu28<-colSums(curmatrix,na.rm=T)
            
            temp<-curmatrix
            temp[temp<=0]<-NA
            eu28pos<-colSums(temp,na.rm=T)
            eu28pos[eu28pos==0]<-NA
            
            temp<-curmatrix
            temp[temp>=0]<-NA
            eu28neg<-colSums(temp,na.rm=T)
            eu28neg[eu28neg==0]<-NA
            
            rel<-t(t(curmatrix)/eu28)
            relav<-rowMeans(rel)
            
            # Trend-plot 
            eu28.trend<-colSums(eeatrend)
            rel.trend<-t(t(eeatrend)/eu28.trend)
            relav.trend<-rowMeans(rel.trend)
            
            topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
            topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
            
        }
        
        if(rundata=="ief"){
            eu28mean<-colMeans(curmatrix,na.rm=T)
            if((runfocus=="value") & ("AD" %in% curmeasu)){
                #Some parameter are not reported from all countries
                eu28<-colSums(curmatrix*actcount[rownames(actcount) %in% rownames(curmatrix),])/
                    colSums(actcount[rownames(actcount) %in% rownames(curmatrix),])
            }else{
                eu28<-eu28mean
            }
            
            
            #Calculate relative value
            if(runfocus=="value"){rel<-t(t(curmatrix)/eu28)}
            if(runfocus=="countries"){rel<-t(t(curmatrix)/eu28)}
            if(runfocus=="trend"){rel<-eeatrend}
            
            #Keep only those values to plot which are not overlaying the boxplot
            #Attention: the endrange might make 'disappear' points that would be expected...
            endrange<-0.1
            
            lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
            upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
            lowerok<-(curmatrix<lowerend)
            upperok<-(curmatrix>upperend)
            relna<-lowerok+upperok
            
            #Use absolute relative deviation as sorting criterium
            #Store the maximum absolute relative deviation
            relabs<-abs(1-rel)
            relav<-rowMeans(relabs)
            relav<-apply(relabs,1,max)
            
            #Trend from time series (do not use the mean trend)
            eu28.trendmean<-colMeans(eeatrend,na.rm=T)
            eu28.trend<-eu28[2:nyears]/eu28[1:(nyears-1)]
            
            
            rel.trend<-t(t(eeatrend)/eu28.trend)
            relav.trend<-rowMeans(rel.trend,na.rm=T)
            
            #Keep code below in case such a criterion will be applied later
            #if(runfocus=="value"){reldiff=0.1}
            #if(runfocus=="trend"){reldiff=0.03}
            #if(runfocus=="countries"){reldiff=0.03}
            #rel[is.na(rel)]<- 999
            #relna<-rel
            #relna[relna==-999]<- 999
            ##keep only those which down-deviation larger that relidiff 
            #relna[relna<1-reldiff]<-1
            #relna[relna==999]<--999
            ##keep only those which up-deviation larger that relidiff 
            #relna[relna>1+reldiff]<-1
            #relna[relna!=1]<-0
            ##Now delete all the value with small deviations
            relplot=curmatrix*relna
            
            if(runfocus=="value"){
                relplot[relplot==0]<-NA
                #Criterion for selecting country: largest average deviation from EU average
                # --nr.rm=F keeps only those countries which have large deviations for the whole time series
                #relav<-rowMeans(relplot,na.rm=F)
            }
            
            if(runfocus=="trend"){
                #Criterion for selecting country: largest internnual change in timeseries
                #relav<-apply(relplot,1,max)
                relplot[relplot==0]<-NA
            }  
            
            #---> first determine number of non-NA elements
            topn<-min(10,length(sort(relav,decreasing=F)))
            topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
            topneu28<-sort(topneu28,decreasing=F)
            topother<-tail(relav[order(relav,decreasing=T,na.last=T)],nrow(curmatrix)-topn)
        }
        
        #print("Determine the the top n countries contributing on average most to EU28 values")
        topnnames<-names(topneu28)
        toponames<-names(topother)
        ncountries<-min(topn,length(topnnames))+min(1,length(toponames))
        
        
        #print("Extract top n countries from dataset and group other together for plotting")
        eu28main<-curmatrix[topnnames,]
        eu28rest<-curmatrix[toponames,]
        if(rundata=="ief" && runfocus=="trend"){
            eu28main<-relplot[topnnames,]
            eu28rest<-relplot[toponames,]
        }
        if(rundata=="adem"){
            Other<-colSums(eu28rest,na.rm=T)
        }else{
            Other<-colMeans(eu28rest,na.rm=T)
        }
        
        #print("# ---> combine Main countries with the 'other' countries")
        if(sum(Other,na.rm=TRUE)>0){
            eu28fin<-rbind(eu28main,Other)
        }else{
            eu28fin<-eu28main
        }
        if(length(toponames)>0){
            finnames<-c(row.names(eu28fin)[1:(ncountries-1)],"Other")
        }else{
            finnames<-row.names(eu28fin)
        }
        eu28fin<-as.matrix(eu28fin)
        
        #rownames(eu28fin)<-finnames
        finshares<-round(rowMeans(eu28fin,na.rm=T)/mean(eu28)*100,1)
        
        if(runfocus=="trend"){
            # Calculate the finshares from trend over total time period
            
        }
        source("nirplots.r")
        #nirplotsdone<-nirplots(curmatrix,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }else{
        print("sum(eealocator)==0")
    }
}
#    }
#    if(substr(curtasks[icurtasks],0,3)=="cat"){starttasks=1}
#}

