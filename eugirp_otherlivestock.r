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
tables4sect<-unique(tables4sect)

tables4clas<-unique(allagri[allagri$measure%in%tables4classifi & 
                                grepl("^3.[AB]",allagri$sector_number) &
                                !grepl("Option",allagri$sector_number),c("sector_number","classification")])
tables4clas$sector_number<-gsub("[1-9] *$","",tables4clas$sector_number)
tables4clas<-unique(tables4clas)

# Work with sub-data set for other livestock
allother<-allagri[allagri$category%in%otherlivestock,]
nother<-nrow(allother)

# First, link the sub-category to 'other livestock'
allother$sector_number<-unlist(lapply(c(1:nother),function(x)
    otherlive$code[unlist(otherlive$otherlivestock)==as.character(allother$category[x])]))
# Second, link the categroy (3.A, 3.B.1, 3.B.2) to 'other livestock' sub-categories
fillother<-function(x,tables4sect,allother){
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
    #print(x)
    #print(paste(mea,cla,secn,sep="-"))
    if(length(secn)>1)print(x)
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
selection<-allagri$category=="General" & allagri$party=="BE" & 
    !(allagri$meastype%in%meastb12weight | allagri$meastype%in%meastb22weight | allagri$meastype%in%measta2weight)
allagri<-allagri[!selection,]
selection<-allagri$category=="General"
allagri$category[selection]<-"Swine"
allagri$sector_number[selection & allagri$meastype%in%measta2weight]<-"3.A.3"
allagri$sector_number[selection & allagri$meastype%in%meastb12weight]<-"3.B.1.3"
allagri$sector_number[selection & allagri$meastype%in%meastb22weight]<-"3.B.2.3"

allagri98<-allagri

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

#Substitute different terms for 'other Swine'
allagri<-substituteothers(allagri,"All swine","Other Swine")
allagri<-substituteothers(allagri,"Other Swine.swine","Other Swine")
allagri<-substituteothers(allagri,"Other Swine.Total","Other Swine")
allagri<-substituteothers(allagri,"Other swine","Other Swine")
allagri<-substituteothers(allagri,"Other Swine","Swine")

selectsw<-grepl("swine",tolower(allagri$category))
swines<-unique(allagri$category[selectsw])

#Harmonize UIDs
usefields<-c(sectfields,metafields,measfields)
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
allagri<-substituteothers(allagri,"Other Sheep.Sheep","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep.Total","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep.General","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep","Other Sheep")
allagri<-substituteothers(allagri,"Other Sheep","Sheep")
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
allagri<-unique(allagri)

# Calculate parameter for parent category 'swine' and 'sheep ####
allagri160<-allagri #keep 160 here!

sheepswine<-c("Sheep","Swine")
source("eugirp_aggparentanimal.r")
allagri174<-addparentanimal

curagri<-addparentanimal
source("eugirp_cattle.r")
allagri177<-curagri
allagri<-curagri
allagri$option[allagri$category%in%allcattle]<-""
