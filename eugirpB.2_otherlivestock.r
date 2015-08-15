# Deal with 'other livestock'
# Prepare data frames that link other livestock to category via measures or classification
# NOTE required 'clean' alldata

tables4sect<-unique(alldata[alldata$measure%in%tables4measures & 
                                grepl("^3",alldata$sector_number) &
                                !grepl("Option",alldata$sector_number),c("sector_number","measure")])
tables4sect$sector_number<-gsub("[1-9] *$","",tables4sect$sector_number)
tables4sect<-unique(tables4sect)

tables4clas<-unique(alldata[alldata$measure%in%tables4classifi & 
                                grepl("^3.[AB]",alldata$sector_number) &
                                !grepl("Option",alldata$sector_number),c("sector_number","classification")])
tables4clas$sector_number<-gsub("[1-9] *$","",tables4clas$sector_number)
tables4clas<-unique(tables4clas)

# Work with sub-data set for other livestock
allother<-alldata[alldata$category%in%otherlivestock,]
nother<-nrow(allother)

# First, link the sub-category to 'other livestock'
allother$sector_number<-unlist(lapply(c(1:nother),function(x)
    otherlive$code[otherlive$otherlivestock==allother$category[x]]))
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
    return(secn)
}
allother$sector_number<-unlist(lapply(c(1:nother),function(x) fillother(x,tables4sect,allother)))

#Recombine
alldata$sector_number[alldata$category%in%otherlivestock]<-allother$sector_number

