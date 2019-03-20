# Next line converts values to numeric - should already be done during dcase in eugirpA.1_eealocator.r
allagri[,years]<-apply(allagri[,years],2,function(x) as.numeric(x))

#Slovakia: remove Other.swine.sine
sel<-allagri$party=="SVK"&allagri$category=="Other Swine.swine"
allagri<-allagri[!sel,]

#LUX: remove Other sheep
#View(allagri[allagri$party == "LUX" & grepl("[Ss]heep", allagri$category), ])
if(invyear == 2019){   # Removing 'Other Sheep' as it is the sum of 'Sheep lambs under 1 yr' and 'Other Sheep.Sheep', and it could get confused with the latter
#  sel <- allagri$party == "LUX" & allagri$category == "Other Sheep"
#  allagri<-allagri[!sel,]
  #allagri[allagri$party == "LUX" & allagri$category == "Other Sheep.Sheep", ]$category <- "Other Sheep.Sheep LUX"
  #allagri[allagri$category == "Other Sheep.Sheep", ]$category <- "Other Sheep.Sheep subcat"
}

#Remove end-blank in sector_number
selection<-grepl(" $",allagri$sector_number)
allagri$sector_number[selection]<-gsub(" $","",allagri$sector_number[selection])

# Deal with 'other livestock' ####
print("# Deal with 'other livestock' ####")
# Prepare data frames that link other livestock to category via measures or classification
# NOTE required 'clean' allagri

tables4sect<-unique(allagri[allagri$measure%in%tables4measures & 
                                grepl("^3",allagri$sector_number) &
                                !grepl("Option",allagri$sector_number),c("sector_number","measure")])
tables4sect$sector_number<-gsub("[1-9] *$","",tables4sect$sector_number)
tables4sect$sector_number<-gsub("4 Other *$","",tables4sect$sector_number)
tables4sect<-unique(tables4sect)
#tables4remo<-tables4sect[grepl("Other",tables4sect$sector_number),"sector_number"]
#tables4sect<-tables4sect[!grepl("Other",tables4sect$sector_number),]
allagritemp<-allagri
#allagri[allagri$sector_number%in%tables4remo,"sector_number"]<-""

tables4clas<-unique(allagri[allagri$measure%in%tables4classifi & 
                                grepl("^3.[AB]",allagri$sector_number) &
                                !grepl("Option",allagri$sector_number),c("sector_number","classification")])
tables4clas$sector_number<-gsub("[1-9] *$","",tables4clas$sector_number)
tables4clas$sector_number<-gsub("4 Other *$","",tables4clas$sector_number)
#tables4clas<-tables4clas[!grepl("Other",tables4clas$sector_number),]
tables4clas<-unique(tables4clas)

# Work with sub-data set for other livestock
allother<-allagri[allagri$category%in%otherlivestock,]
nother<-nrow(allother)

# First, link the sub-category to 'other livestock'
allother$sector_number<-unlist(lapply(c(1:nother),function(x)
    otherlive$code[unlist(otherlive$otherlivestock)==as.character(allother$category[x])]))
# Second, link the categroy (3.A, 3.B.1, 3.B.2) to 'other livestock' sub-categories
fillother<-function(x,tables4sect,allother){
    #for(i in 1:nother){print(i)
        sec<-allother$sector_number[x]
        cla<-allother$classification[x]
        mea<-allother$measure[x]
        if(mea%in% tables4measures){
            secn<-paste0(tables4sect$sector_number[tables4sect$measure==mea],sec)
        }else
        {
            if(cla=="Enteric Fermentation"){secn<-paste0("3.A.",sec)}else
                if(cla=="CH4 Emissions"){secn<-paste0("3.B.1.",sec)}else
                    if(grepl("N2O",cla)){secn<-paste0("3.B.2.",sec)}else
                    {secn<-sec}
        }
        #print(secn)
        #print(paste(mea,cla,secn,sep="-"))
        if(length(secn)>1)print(x)
    #}
    return(secn)
}
allother$sector_number<-unlist(lapply(c(1:nother),function(x) fillother(x,tables4sect,allother)))
#Recombine
allagri$sector_number[allagri$category%in%otherlivestock]<-allother$sector_number


# Check for other other livestock (country-defined) ####
print("# Check for other other livestock (country-defined) ####")
assignab<-function(class){
    sect<-""
    for(i in c(1:3)){if(class==mslivestockclass[i]) {sect<-mslivestocksect[i]}}
    return(sect)
}

allother<-allagri[allagri$sector_number=="" & allagri$classification%in%mslivestockclass,]
nother<-nrow(allother)
# Cattle Option C --> belongs to category 1
select<-grepl("Option C",allother$option)
allother$sector_number[select]<-unlist(lapply(c(1:sum(select)),function(x) 
    paste0(assignab(allother$classification[select][x]),".1")))

select<-grepl("sheep|lamb|ewe|ram",tolower(allother$category))
allother$sector_number[select]<-unlist(lapply(c(1:sum(select)),function(x) 
    paste0(assignab(allother$classification[select][x]),".2")))
# Island reports 'Animals for replacement' ?? (checked over population data)... should be put as recommendation
select<-grepl("replacement",tolower(allother$category))
if(sum(select)>0) allother$sector_number[select]<-unlist(lapply(c(1:sum(select)),function(x) 
    paste0(assignab(allother$classification[select][x]),".2")))

select<-grepl("swine|pig|boars|sow|gilt",tolower(allother$category))
if(sum(select)>0) allother$sector_number[select]<-unlist(lapply(c(1:sum(select)),function(x) 
    paste0(assignab(allother$classification[select][x]),".3")))

cntrylive<-unique(allother$category[allother$sector_number=="" & allother$classification%in%mslivestockclass])
#there is still 'General' for belgium which is corrected later
if(length(cntrylive)>1)stop("There are more 'other livestock', check cntrylive")
#Recombine
allagri$sector_number[allagri$sector_number=="" & allagri$classification%in%mslivestockclass]<-allother$sector_number

# Belgium reports swine as 'General' ... should be put as recommendation
#select<-grepl("general",tolower(allother$category))
#allother$sector_number[select]<-unlist(lapply(c(1:sum(select)),function(x) 
#    paste0(assignab(allother$classification[select][x]),".3")))
selection<-allagri$category=="General" & allagri$party=="BEL" & 
    !(allagri$meastype%in%meastb12weight | allagri$meastype%in%meastb22weight | allagri$meastype%in%measta2weight)
allagri<-allagri[!selection,]
selection<-allagri$category=="General"
allagri$category[selection]<-"Swine"
allagri$sector_number[selection & allagri$meastype%in%measta2weight]<-"3.A.3"
allagri$sector_number[selection & allagri$meastype%in%meastb12weight]<-"3.B.1.3"
allagri$sector_number[selection & allagri$meastype%in%meastb22weight]<-"3.B.2.3"

allagri98<-allagri
#allagri <- allagri98
# Method-column gives problems for merge (20160819 - never previously)
allagrimethod<-allagri$method
allagri<-allagri[,-which(names(allagri)=="method")]


#Remove duplicate lines (e.g. 'Swine' and 'Other swine') for agri
#agriselect<-duplicated(allagri[,names(allagri)[!names(allagri)%in%c("category","variableUID")]])
#allagri<-allagri[!agriselect,allfields]
substituteothers<-function(A,check1,check2){
    B<-A
    B$category<-gsub(check1,check2,A$category)
    selectswd<-duplicated(B)
    if(sum(selectswd==0)){A<-B}else{stop(check1)}
    return(A)
}

# Clean up swine and sheep categories ####
print("# Clean up swine and sheep categories ####")
selectsw<-grepl("swine",tolower(allagri$category))
swines<-unique(allagri$category[selectsw])

#allagri_kk <- allagri
#Substitute different terms for 'other Swine'
#if(invyear < 2019){
#  allagri[allagri$party != "MLT", ] <- substituteothers(allagri[allagri$party != "MLT", ],"All swine","Other Swine")
#}else{
allagri<-substituteothers(allagri,"All swine","Other Swine")
allagri<-substituteothers(allagri,"all swine","Other Swine")
#}
allagri<-substituteothers(allagri,"Other Swine.swine","Other Swine")
allagri<-substituteothers(allagri,"Other Swine.Total","Other Swine")
allagri<-substituteothers(allagri,"Other swine","Other Swine")
allagri<-substituteothers(allagri,"Other Swine","Swine")

selectsw<-grepl("swine",tolower(allagri$category))
swines<-unique(allagri$category[selectsw])

#Harmonize UIDs
usefields<-c(sectfields,metafields,measfields)
usefields<-usefields[usefields!="method"]
allfields<-allfields[allfields!="method"]
#allagri$method<-as.character(allagri$method)
swineuids<-unique(allagri[allagri$category=="Swine",c(usefields,"variableUID")])
swineuids<-swineuids[order(swineuids$sector_number,swineuids$measure),]
swineuidsn<-unique(swineuids[,usefields])
swineuidsn$nvariableUID<-unlist(lapply(c(1:nrow(swineuidsn)),function(x) 
    swineuids$variableUID[row.names(swineuids)==row.names(swineuidsn)[x]]))

allagri<-merge(allagri,swineuidsn,by=usefields,all.x=TRUE)
agriselect<-!is.na(allagri$nvariableUID)
allagri$variableUID[agriselect]<-allagri$nvariableUID[agriselect]
allagri<-allagri[,allfields]

#Clean up swine and sheep categories
selectsw<-grepl("sheep",tolower(allagri$category))
sheeps<-unique(allagri$category[selectsw])

#Substitute different terms for 'other sheep'
allagri<-substituteothers(allagri,"All Sheep","Other Sheep")
#if(invyear != 2019){
#  allagri[allagri$party != "LUX", ] <- substituteothers(allagri[allagri$party != "LUX", ],"Other Sheep.Sheep","Other Sheep")
#}#else{
#  allagri<-substituteothers(allagri,"Other Sheep.Sheep","Other Sheep")
#}
#if(invyear == 2019){
##  #allagri[allagri$party == "LUX" & allagri$meastype == "DIGEST" & grepl("[Ss]heep", allagri$category), ]$category <- " Other Sheep"
##  allagri[allagri$party == "LUX", ] <- substituteothers(allagri[allagri$party == "LUX", ], "Other Sheep.Sheep", "Other Sheep")
#  allagri[allagri$party == "LUX", ] <- substituteothers(allagri[allagri$party == "LUX", ], "Other Sheep.Sheep", "Other Sheep")
#} 

#allagri<-substituteothers(allagri,"Other Sheep.Sheep","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep.Total","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep.General","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep","Other Sheep")
#if(invyear == 2019){
#  allagri[allagri$party != "LUX", ] <- substituteothers(allagri[allagri$party != "LUX", ],"Other Sheep", "Sheep")
#}else{
  allagri<-substituteothers(allagri,"Other Sheep","Sheep")
#}

allagri<-substituteothers(allagri,"sheep","Sheep")
allagri152<-allagri
selectsw<-grepl("sheep",tolower(allagri$category))
sheeps<-unique(allagri$category[selectsw])

#Harmonize UIDs
sheepuids<-unique(allagri[allagri$category=="Sheep",c(usefields,"variableUID")])
sheepuids<-sheepuids[order(sheepuids$sector_number,sheepuids$measure),]
sheepuidsn<-unique(sheepuids[,usefields])
sheepuidsn$nvariableUID<-unlist(lapply(c(1:nrow(sheepuidsn)),function(x) 
    sheepuids$variableUID[row.names(sheepuids)==row.names(sheepuidsn)[x]]))

allagri<-merge(allagri,sheepuidsn,by=usefields,all.x=TRUE)
agriselect<-!is.na(allagri$nvariableUID)
allagri$variableUID[agriselect]<-allagri$nvariableUID[agriselect]
allagri<-allagri[,allfields]

if(nrow(unique(allagri[allagri$party == "MLT" & allagri$variableUID == "91817067-8DB6-41D6-A348-57E2C17B655D", years])) > 1){
  sel <- grepl("[Ss]wine", allagri$category) & allagri$party == "MLT" & allagri$meastype == "POP" & allagri$sector_number == "3.A.3"
  allagri[sel, names(allagri) %in% years] <- round(allagri[sel, names(allagri) %in% years], 5)
  #allagri[grepl("[Ss]wine", allagri$category) & allagri$party == "MLT" & allagri$meastype == "POP" & allagri$sector_number == "3.A.3",]
}
allagri<-unique(allagri)


