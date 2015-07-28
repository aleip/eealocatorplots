# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
# File eealocatorplots.r
# Main file required for the EU-GIRP
# Purpose: A: connect to eealocator file (run eealocator_csv.bash first)
#             and generate focus
#          B: loop over focus and prepare for current plot 
#             (selection of parameter, trend/value/country-plot, etc.)
#          C. run nirplots.r for current plot
#
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
# version 3 - 28.07.2015
# 
library(ggplot2)
library(reshape2) #required for melt function - for outls
library(data.table)
# library(mblm)  # needed for Theil Sen outl detection (see outl tool ... but not used in the excel output?)

rm(list=objects())
searchline<-FALSE
#dev.off()

# PART A: Link with EEA locator tool ----

# Define the folder all the process should run, usually the folder of the 
#       current inventory year
invyear<-"c:/adrian/data/inventories/ghg/unfccc/eealocatorplots"
cursubm <- "20150703"
curextr <- "20150722"
years2keep<-c(1990:2013)
csvfil <-"eealocatortest_2014_AllAllAll.csv"
csvfil <- "EEA_GHG_MMR_locator_20150323.cub_2015.csv"
csvfil <- "../2015/eealocator/eealocator_20150115_20150509"
csvfil <- paste0("../2015/eealocator/eealocator_",cursubm,"_",curextr)
setwd(invyear)


# Load the current task to do
# See file curplot.csv for information on how to set it up
source("curplot.csv")
dotaskdetails<-unlist(strsplit(curtask,","))
if(grepl("3*,all,all,all",curtask)){docat3<-TRUE}else{docat3<-FALSE}

multilines<-function(text2split,maxWidth=30){
    #text2split: text
    vtext<-strwrap(text2split,1)
    nchartext<-lapply(c(1:length(vtext)), function(x) 1+ nchar(vtext[x]))
    nchartext<-ceiling(cumsum(nchartext) / maxWidth)
    restext<-c(1:max(nchartext))
    for(i in c(1:max(nchartext))){
        restext[i]<-paste(vtext[nchartext==i],collapse=" ")
    }
    return(restext)
}

print("# Basic selection: source cagegory and gas")
docateg<-dotaskdetails[1]
#dosourc<-dotaskdetails[2]
dogases<-dotaskdetails[2]
domeasu<-dotaskdetails[seq(3,length(dotaskdetails),2)]
#if(length(dotaskdetails)>3) {
#    selmeasu<-as.numeric(dotaskdetails[seq(4,length(dotaskdetails),2)])
#}

if (dogases=="all"){dogases<-7}
dogases<-as.numeric(dogases)
doplots<-2**((which((as.integer(intToBits(dogases))[1:3]) %in% 1)-1))
print(paste(docateg,dogases,domeasu))

# Jump over the generation of 'alldata' in case this has already been done
generatealldata <- 1

rdatallem <- paste0(csvfil,"_clean.RData")
#if(docateg=="all" & dogases=="all" & domeasu=="EM"){
    if(file.exists(rdatallem)){
        if(file.info(paste0(csvfil,".txt"))$mtime<file.info(rdatallem)$mtime){
            print(paste0("Load existing file ",rdatallem))
            load(rdatallem)    
            generatealldata <- 0
        }
    }
#}

if(generatealldata==1){
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
    
    # measureacronyms --------------------------------------------------------------
    # Keep long text with the exception of 'measure' which is needed to identify
    # if it is an activity data, emissions, emission factor or parameter or other
    # alldata$measurelong<-alldata$measure
    measureacronyms<-read.csv("metadim_row8measure.txt",stringsAsFactors=FALSE)
    temp1<-merge(alldata,measureacronyms,by.x="measure",by.y="measname")
    alldata<-subset(temp1,select=-dummy)
    
    # Correction of meastypes for specific emission sources ####
    
    # table 3B(b) ------------------------------------------------------------------
    #
    #  ---> Table 3.B(b) is rather complex...
    #
    # sector_number is read as factor, which does not allow to define new elements 
    #               therefore transformation to 'character' required
    alldata$sector_number<-as.character(alldata$sector_number)
    alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kt","meastype"]<-"EM"
    alldata[alldata$sector_number=="3.B.2.5" & alldata$classification=="Indirect N2O Emissions" & alldata$unit=="kg N2O/kg N","meastype"]<-"IEF"
    
    alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="Total N volatilised as NH3 and Nox","sector_number"]<-"3.B.2.5 indirect volatilisation"
    alldata[alldata$sector_number=="3.B.2.5" & alldata$measure=="N lost through leaching and run-off","sector_number"]<-"3.B.2.5 indirect leaching"
    alldata[alldata$sector_number=="3.B.2.5" & grepl("Atmospheric deposition",alldata$measure),"sector_number"]<-"3.B.2.5 indirect volatilisation"
    alldata[alldata$sector_number=="3.B.2.5" & grepl("Nitrogen leaching",alldata$measure),"sector_number"]<-"3.B.2.5 indirect leaching"
    #View(alldata[grepl("3.B.2.5",alldata$sector_number),]) 
    
    # Biomass burning ------------------------------------------------------------------------
    # Biomass burningd ---
    alldata[grepl("3.F.1.",alldata$sector_number) & alldata$measure=="Crop  production","meastype"]<-"PROD"
    alldata[grepl("3.F.1.",alldata$sector_number) & alldata$measure=="Biomass available","meastype"]<-"PROD"
    alldata[grepl("4*Biomass Burning",alldata$sector_number),"meastype"]<-"PROD"
    remove<-as.vector(unlist(unique(read.csv("aduids_to_remove.txt",header=TRUE))))
    alldata[alldata$variableUID %in% remove,"meastype"]<-"PROD"
    
    
    # Select gases
    gases2keep<-c("Aggregate GHGs","CH4","no gas","CO2","N2O")
    allgases<-sort(unique(alldata$gas))
    selectGas<-alldata$gas %in% gases2keep
    alldata<-alldata[selectGas,]
    
    # Remove UK (use GB) ####
    selectParty<-! alldata$party == "UK"
    alldata<-alldata[selectParty,]
    
    # Remove category substrings -----------------------------------------------------------------------------
    #  ---> Category contains sometimes substring of sector_name (e.g. Dairy Cattle)
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
    
    # Remove duplicate UIDs -----------------------------------------------------------------------------
    # ---> there are duplicate UIDs...
    alldata$variableUID<-as.character(alldata$variableUID)
    duplicateuids<-read.csv("duplicateUIDs.csv",header=FALSE)
    names(duplicateuids)<-c("UID","SEC")
    for(changeuid in c(1:nrow(duplicateuids))){
        duplicateUID<-as.vector(duplicateuids[changeuid,"UID"])
        duplicateSEC<-as.vector(duplicateuids[changeuid,"SEC"])
        duplicateNEW<-gsub(substr(duplicateUID,1+nchar(duplicateUID)-nchar(duplicateSEC),nchar(duplicateUID)),duplicateSEC,duplicateUID)
        alldata$variableUID[alldata$variableUID==duplicateUID & alldata$sector_number==duplicateSEC]<-duplicateNEW
    }
    

    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Remove sector_number "-"  ----
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldata<-alldata[alldata$sector_number!="-",]
    
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Create vectors for categories, years, and unique rows (not considering years) ----
    # allcategories is created as 'factor'
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Store keys into separate data file =======================================
    # Keep 'alldatanovalues' for reference in case of problems
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldatanovalues<-as.data.frame(unique(subset(alldata,select=-c(value,year,party,country_name))))
    notationkeys<-c("NO","NE","IE","NA")
    alldatanotations<-alldata[alldata$notation %in% notationkeys,]
    alldata<-alldata[!(alldata$notation %in% notationkeys),]
    
    # Fields giving additional info on the 'method' ----
    fields2merge<-sort(c("category","source","method","target","option","type"))
    fields2keep<-c("sector_number","gas","unit","allmethods","party","meastype")
    
    # ---> Fields that will be needed for information but they are not needed 
    #      to identify the cells
    fields4info<-sort(c("notation","classification","country_name"))
    # ---> Fields that are identical per country 
    fields4coun<-c("submission_version","submission_year")
    listofuniquefields<-sort(c(fields2keep,fields2merge))
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Replace all 'method' fields which will not be needed individually with ----
    # a field where the content is concantenated and simplified
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Replace all fields which are needed to identify the rows (except year) ----
    # a field where the content is concantenated and simplified
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    alldatauniq<-lapply(fields2keep,function(x) as.vector(alldata[,x]))
    alldatauniq<-lapply(alldatauniq,function(x) paste0(x,"_"))
    alldatauniq<-as.vector(Reduce(paste0,alldatauniq))
    alldatauniq<-gsub("__","_",alldatauniq)
    alldatauniq<-as.data.frame(gsub("_$","",alldatauniq))
    names(alldatauniq)<-"unique"
    listofuniq<-unique(alldatauniq)
    alldata<-cbind(alldata,alldatauniq)
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Store away columns not needed immediately together with unique fields -----
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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
    
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    # Generate data frame with one columns per year ----
    # and the value in the corresponding column ---
    # zeros in all other columns
    # http://stackoverflow.com/questions/11350537/convert-a-factor-column-to-multiple-boolean-columns
    # Alternative method could be: #http://stackoverflow.com/questions/9084439/r-colsums-by-group
    # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
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
    years2delete<-as.vector(as.character(allyears[!(allyears %in% years2keep)]))
    allcolumns<-names(alldata)
    alldata<-subset(alldata,select=allcolumns[! allcolumns %in% years2delete])
    
    # Save alldata for later re-use incase of allem or all3 ####
    toremove<-c("temp1","temp2","temp3","temp4")
    rm(toremove)

    #    if(docateg=="all" & dogases=="all" & domeasu=="EM"){
    
    save(alldata,alldatanovalues,measname,file=rdatallem)
    write.table(alldata[grepl("^3",alldata$sector_number),],file=paste0(csvfil,"_cat3.csv"),sep=",")
    write.table(alldata[grepl("^4",alldata$sector_number),],file=paste0(csvfil,"_cat4.csv"),sep=",")
    
}

    # Delete rows with no meastype defined
    alldata<-alldata[alldata$meastype!="",]
# Filter docateg and domeasu ####
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

alldata$sector_number<-gsub(" \\(please specify\\)","",alldata$sector_number)

# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
#source("lists.txt")

# Load the current tasks to do ####
# See file curplot.csv for information on how to set it up
#curtasks <- readLines("curplot.csv")
starttasks<-0
dotrendoutl<-1
adempars<-c("AD","EM")


# ---> Create empyt matrix with individual plots to do
if(searchline)print(355)
focus<-as.data.frame(matrix(NA, nrow=10, ncol=4))
names(focus)<-c("ad","parameter","datatype","focus")
nfocus<-1

# Clean up memory ####
torm1<-c("actcount","categorymatrix","eealocator")
torm2<-torm1[torm1 %in% ls()]
if(length(torm2)>0){rm(list=torm2)}

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# categorymatrix: Copy alldata into new matrix for further processing ####
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

# focusmatrix ####
#nfocus<-length(domeasu)
focusmatrix<-as.data.frame(unique(subset(categorymatrix,select=c(sector_number,allmethods,gas,meastype,variableUID)))[,1:5])
if(domeasu[1]=="all") domeasu<-as.vector(unique(focusmatrix$meastype))

nfocus<-nrow(focusmatrix)
names(focusmatrix)<-c("cat","met","gas","par","uid")

# AD-uid is required to calculate weighted IEFs 
#   Problem: sometimes the 'category' contains the level with the AD
#            sometimes AD is differentiated by 'allmethods'
#            if no AD-uid is found (mainly aggregated levels) - delete the row
#            if there is multiple AD (e.g. population, N excretion per MMS) - this must be corrected (i.e. N exc per MMS ==> EM)

focusmatrix$met<-as.character(focusmatrix$met)
focusmatrixlength<-subset(focusmatrix,select=c(cat,met,par))
focusmatrixlength$ncat<-unlist(lapply(c(1:nfocus),function(x) 
    length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                      focusmatrix[,"met"]==focusmatrix$met[x] & 
                                      focusmatrix[,"par"]=="AD"]))))
focusmatrixlength$nsec<-unlist(lapply(c(1:nfocus),function(x) 
    length(unlist(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & 
                                      focusmatrix[,"par"]=="AD"]))))

focusmatrix$aduid<-unlist(lapply(c(1:nfocus),function(x) 
    if(focusmatrixlength$ncat[x]==1){
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"met"]==focusmatrix$met[x] & focusmatrix[,"par"]=="AD"])
    }else if(focusmatrixlength$nsec[x]==1){
        as.vector(focusmatrix$uid[focusmatrix[,"cat"]==focusmatrix$cat[x] & focusmatrix[,"par"]=="AD"])
    } else {focusmatrixlength$ncat[x]}
))
focusmatrix<-focusmatrix[focusmatrix$aduid!=0,]

# Check for multiple ADs ####
# Multiple ADs for a measure detected: generate data frame to investigate the problem
if(searchline)print(436)
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
        #(!sum(adempars %in% x))*(dogases) + (sum(adempars %in% x)*3) ))
    focusmatrix$nrep<-unlist(lapply(focusmatrix$par,function(x) 
        (!sum(adempars %in% x))*3 + (sum(adempars %in% x)*2) ))
        #(!sum(adempars %in% x))*(dogases%%4) + (sum(adempars %in% x)*2) ))
}    

if(searchline)print(481)
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

focus<-focus[focus$focus %in% doplots,]
focuscols<-nrow(focus)
zzz
# PART B loop over focuscols ####
#Select focus        
for (numrun in 1:(focuscols)){
#for (numrun in 177:184){
    #        for (numrun in 5:5){
    #B1.Copy run-parameter #### 
    # numrun<-1            
    print(paste0("#Copy plot-requests to current plot for numrun=",numrun))
    stophere<-FALSE
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
    #selectcountries<-curparts[curparts %in% as.vector(runmatrix$party)]
    runparts<-curparts[curparts %in% runmatrix$party]
    admatrix<-subset(categorymatrix,variableUID==runaduid & party %in% curparts)
    
    
    #if(docat3==TRUE & (runpar=="AD" | runpar=="EM")){stophere<-TRUE}

    runtotal<-sum(runmatrix[,as.character(years)])
    adtotal<-nrow(admatrix)
    if(adtotal>0){adtotal<-sum(admatrix[,as.character(years)])}
    
    if(runfocus==1){runfocus<-"value"}
    if(runfocus==2){runfocus<-"trend"}
    if(runfocus==4){runfocus<-"countries"}
    
    print(paste("Current task ",numrun,". AD=",runad,". Par=",runpar,". cudat=",focus[numrun,3],". curplot=",focus[numrun,4],sep=""))

    
    # B2. Retrieve Activity Data #####
    #             if(numrun==1){rundata="adem";runfocus="value"}
    #             if(numrun==2){rundata="adem";runfocus="trend"}
    #             if(numrun==3){rundata="ief";runfocus="value"}
    #             if(numrun==4){rundata="ief";runfocus="trend"}
    #             if(numrun==5){rundata="ief";runfocus="countries"}
    noactivitydata<-FALSE
    if(searchline)print(550)
    if("AD" %in% curmeasu & runtotal!=0 & adtotal!=0){
        tmp1<-subset(admatrix,meastype=="AD",select=-meastype)
        tmp1<-tmp1[!is.na(tmp1$party),]
        unitad<-as.vector(unique(subset(tmp1,select=unit))[,1])
        uidad<-as.vector(unique(subset(tmp1,select=variableUID))[,1])
        actcount<-subset(tmp1,sector_number==runcateg,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        acteu28<-colSums(actcount)
        if(nrow(actcount)==0){
            noactivitydata<-TRUE
            #stop("No activity data defined!")
        }
        if(length(as.vector(tmp1$party))>nrow(actcount)){
            View(admatrix)
            #stop("STOP - there are likely duplicate UIDs for AD")
            #could also be that not all AD are given, e.g. in Sector 1 ... get on
        }
        rownames(actcount)<-as.vector(unique(subset(tmp1,select=-sector_number))$party)
        colnames(actcount)<-years
    }
    if(searchline)print(570)
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
    if(searchline)print(580)
    
    if(runpar=="IEF" | runpar=="EM"){
        if(runpar=="EM"){
            tmp1<-subset(categorymatrix,sector_number==runcateg & gas==rungas 
                              & allmethods==runmethod & meastype=="IEF",select=-meastype)
        }else{
            tmp1<-subset(runmatrix,meastype=="IEF",select=-meastype)
        }
        tmpcountries<-as.vector(tmp1$party)
        actcountries<-row.names(actcount)
        unitief<-as.vector(unique(subset(tmp1,select=unit))[,1])
        uidief<-as.vector(unique(subset(tmp1,select=variableUID))[,1])
        iefcount<-subset(tmp1,select=c(-gas,-unit,-party,-allmethods,-variableUID,-sector_number))
        # In case there are less IEFs identified then ADs select those ADs for 
        # which also the IEF exist for weighted EU-IEF
        iefeu28<-colSums(iefcount*actcount[row.names(actcount)%in%tmpcountries,])/acteu28
        rownames(iefcount)<-as.vector(tmp1$party)
        colnames(iefcount)<-years
    }
    
    # B3. eealocator: Copy data of relevance to the eealocator data frame ####
    # Get unit for parameter
    if(searchline)print(597)
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
    
    # PART C: processing and plotting ####
    if(nrow(eealocator)==0){stophere<-TRUE}else if(sum(eealocator)==0){stophere<-TRUE}

    if(stophere==FALSE){
        #rm(list=c("tmp0","tmp1","tmp2","tmp3"))
        
        #print("# FOR TEST PURPOSE: ADD RANDOM NUMBER TO INCREASE NUMBER OF COUNTRIES")
        # addrandomcountries in case there is none ####
        addrandomcountries<-0
        if(searchline)print(625)
        if(addrandomcountries==1){
            attributes<-as.matrix(read.table("attributes.txt",header=T,row.names=1,check.names=F))
            allcountries<-row.names(attributes)
            allcountries<-allcountries[allcountries != "Other"]
            restcountries<-allcountries[! allcountries %in% curparts]
            #Add countries
            nnewc<-25
            newcn<-restcountries[sample(1:length(restcountries),nnewc,replace=FALSE)]
            #Multiplicate with random between 0.7 and 1.3 times values
            randommulp<-matrix(runif(nyears*nnewc,min=0.9,max=1.2),ncol=nyears)
            
            pick<-matrix(sample(0:1,nyears*length(curparts),replace=T),ncol=nyears)
            pick[2,]<-1-pick[1,]
            pick<-as.vector(t(colSums(eealocator*pick)))
            newcv<-randommulp*pick
            rownames(newcv)<-newcn
            colnames(newcv)<-years
            eealocator2<-rbind(eealocator,newcv)
            eealocator<-eealocator2
        }
        
        #B.2: Growth rates ####
        #print("#Groth rates calculated as Y_i/Y_(i-1) - 1")
        if(searchline)print(649)
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
        
        if(searchline)print(698)
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
                #listofoutls<-which(unlist(subset(growthoutl,select=-country))!=0)
                listofoutls<-which(unlist(growthoutl)!=0)
                growthoutl$country<-row.names(growthoutl)
                growthoutllist<-melt(growthoutl,id.vars="country",na.rm=T)[listofoutls,]
                #al20150727 - why originally 'nrow(growth)-...'?? 
                #cntyid<-nrow(growth)-(listofoutls %% nrow(growth))
                cntyid<-(listofoutls %% nrow(growth))
                cntyid[cntyid==0]<-nrow(growth)
                #get year-ID
                yearid<-ceiling(listofoutls/nrow(growth))
                growthoutllist<-cbind(cursubm,runcateg,runpar,"growth",
                                      growthoutllist,growthquantiles[cntyid,],
                                      row.names=NULL)
            }
            #paste0(runcateg,runpar)
        }
        
        
        
        # B.3-selectcountries ####    
        if(searchline)print(759)
        topn<-10
        
        #print("#Define trend data frame")
        if (runfocus=="trend"){curmatrix<-eeatrend}
        if (runfocus=="value"){curmatrix<-eealocator}
        if (runfocus=="countries"){curmatrix<-eealocator}
        
        # B.3a-selectcountries-adem ####    
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
            rel.trend<-abs(eeatrend)
            relav.trend<-rowSums(rel.trend)/sum(abs(eu28.trend))
            

            if(runfocus=="value"){
                topneu28<-head(relav[order(relav,decreasing=T,na.last=T)],topn)
                topother<-tail(relav[order(relav,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
                topn<-length(topneu28)
                topno<-max(0,nrow(curmatrix)-topn)
    
                textorderadem1<-"Countries are sorted by the average contribution to the sum of EU28 value over the the whole time period. "
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
            
            if(runfocus=="trend"){
                topneu28<-head(relav.trend[order(relav.trend,decreasing=T,na.last=T)],topn)
                topother<-tail(relav.trend[order(relav.trend,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
                topn<-length(topneu28)
                topno<-max(0,nrow(curmatrix)-topn)
                
                textorderadem1<-paste0("Countries are sorted by the magnitude of their (absolute) inter-annual changes over the year ",min(years),"-",max(years),". ")
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
        }
        
        # B.3a-selectcountries-ief ####    
        if(searchline)print(818)
        if(rundata=="ief"){

            eu28pos<-max(curmatrix,na.rm=T)
            eu28neg<-min(curmatrix,na.rm=T)
                
            eu28mean<-colMeans(curmatrix,na.rm=T)
            #if((runfocus=="value") & ("AD" %in% curmeasu)){
            if((runfocus=="value") & sum(acteu28>0)){
                #Some parameter are not reported from all countries
                eu28<-colSums(curmatrix[rownames(curmatrix) %in% rownames(actcount),]
                             *actcount[rownames(actcount) %in% rownames(curmatrix),])/
                      colSums(actcount[rownames(actcount) %in% rownames(curmatrix),])
            }else{
                eu28<-eu28mean
            }
            
            #Calculate relative value
            if(runfocus=="value"){
                rel<-t(t(curmatrix)/eu28)
                #Use absolute relative deviation as sorting criterium
                #Store the maximum absolute relative deviation
                relabs<-abs(1-rel)
                relav<-rowMeans(relabs,na.rm=T)
                relav<-apply(relabs,1,max,na.rm=T)

            }
            if(runfocus=="countries"){
                #Relative value against the mean value used by MS over the years
                rel<-curmatrix/apply(curmatrix,1,mean,na.rm=T)
                #Largest range of values; in case of negative values rel calculate distance vs max value
                relabs<-((apply(curmatrix,1,max,na.rm=T)-apply(curmatrix,1,min,na.rm=T))
                         /apply(curmatrix,1,max,na.rm=T)
                         )
                relav<-relabs
            
            }
            if(runfocus=="trend"){
                rel<-eeatrend
                #Use absolute relative deviation as sorting criterium
                #Store the maximum absolute relative deviation
                relabs<-(abs(rel))
                relav<-rowMeans(relabs,na.rm=T)
                relav<-apply(relabs,1,max,na.rm=T)
                relav<-relav[!is.na(relav)]
            }
            
            #Keep only those values to plot which are not overlaying the boxplot
            #Attention: the endrange might make 'disappear' points that would be expected...
            endrange<-0.1
            
            lowerend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5-endrange),na.rm=T)) 
            upperend<-apply(curmatrix,2,function(x) quantile(x,probs=(0.5+endrange),na.rm=T)) 
            lowerok<-(curmatrix<lowerend)
            upperok<-(curmatrix>upperend)
            relna<-lowerok+upperok
            
            
            #20150726 - commented keep for a while...
            #Trend from time series (do not use the mean trend)
            #eu28.trendmean<-colMeans(eeatrend,na.rm=T)
            #eu28.trend<-eu28[2:nyears]/eu28[1:(nyears-1)]
            #rel.trend<-t(t(eeatrend)/eu28.trend)
            #relav.trend<-rowMeans(rel.trend,na.rm=T)
            
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
            topother<-tail(relav[order(relav,decreasing=T,na.last=T)],max(0,nrow(curmatrix)-topn))
            
            topn<-length(topneu28)
            topno<-max(0,nrow(curmatrix)-topn)

            if(runfocus=="value"){
                textiefval1<-"EU28+IC value is obtained from a weighted average of country-values. "
                textiefval2<-"The relative distance from MS/EU28 value is calculated for each year (e.g. 10% smaller). "
                textiefval3<-"Countries are sorted by average relative distance calculated over the whole time period. "
                textiefval4<-paste0("The top ",topn," countries are displayed. ")
                textiefval5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textiefval1,textiefval2,textiefval3,textiefval4,textiefval5)
            }
            if(runfocus=="countries"){
                textiefcnt3<-"Countries are sorted by their relative range of IEFs over the whole time period. "
                textiefcnt4<-paste0("The top ",topn," countries are displayed. ")
                textiefcnt5<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textiefcnt3,textiefcnt4,textiefcnt5)
            }
            if(runfocus=="trend"){
                textorderadem1<-"Countries are sorted by the average growth rate over the whole time period. "
                textorderadem2<-paste0("The top ",topn," countries are displayed. ")
                textorderadem3<-paste0("The other ",topno," countries with data are lumped to 'other'.")
                textorder<-paste0(textorderadem1,textorderadem2,textorderadem3)
            }
            
        }
        
        #print("Determine the the top n countries contributing on average most to EU28 values")
        if(searchline)print(931)
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
            if((runfocus=="value") & sum(acteu28>0)){
                #Some parameter are not reported from all countries
                Other<-colSums(eu28rest*actcount[rownames(eu28rest),])/
                       colSums(actcount[rownames(eu28rest),])
            }
        }
        
        #print("# ---> combine Main countries with the 'other' countries")
        if(length(toponames)>0){
            eu28fin<-rbind(eu28main,Other)
        }else{
            eu28fin<-eu28main
            #20150727 commented because the '1' for 'other' is already excluded (line 915)
            #ncountries<-ncountries-1
        }
        if(length(toponames)>0){
            finnames<-c(row.names(eu28fin)[1:(ncountries-1)],"Other")
        }else{
            finnames<-row.names(eu28fin)
        }
        eu28fin<-as.matrix(eu28fin)
        
        #rownames(eu28fin)<-finnames
        finshares<-rowMeans(eu28fin,na.rm=T)/mean(eu28)*100
        finshares[is.na(finshares)]<-0
        
        if(runfocus=="trend"){
            # Calculate the finshares from trend over total time period
            
        }
        if(searchline)print(977)
        if(!(sum(eu28fin,na.rm=TRUE)==0)) {source("nirplots.r")}
        
        #nirplotsdone<-nirplots(curmatrix,eu28fin,eu28,rundata,runfocus,runcateg,runpar,curfoc)
    }else{
        print("sum(eealocator)==0")
    }
}

