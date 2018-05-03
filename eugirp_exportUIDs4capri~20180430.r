source("curplot.r")
setfile<-"CAPRI_NIR_Sets.csv"
unitorder<-c("meastype","gas","unit","sector_number","category","method","classification","source","target","type","option","measure")
avmeas<-unique(allagri[,c("variableUID",unitorder)])

# Restrict to aggregate livetock categories
select<-grepl("3.[AB]",avmeas$sector_number) & ! avmeas$category%in%c("Farming",livestock,"Other Livestock")

avmeas<-avmeas[!select,]
avmeas<-convert2char(avmeas)
avmeas$metastring<-sapply(1:nrow(avmeas),function(x) paste(avmeas[x,unitorder],collapse=","))
avmeas<-avmeas[,c("variableUID","metastring")]
#avmeas$metastring<-gsub("Aggregate GHGs","Agg GHG",avmeas$metastring)
#avmeas$metastring<-gsub("Liquid[ ,a-z,A-Z]*","Liquid,",avmeas$metastring)
#avmeas$metastring<-gsub("Solid[ ,a-z,A-Z]*","Solid,",avmeas$metastring)
#avmeas$metastring<-gsub("Pasture[ ,a-z,A-Z]*","Pasture,",avmeas$metastring)
avmeas$metastring<-gsub(",,",",*,",avmeas$metastring)
avmeas$metastring<-gsub(",,",",*,",avmeas$metastring)

curdate<-gsub("-","",Sys.Date())
previousset<-read.csv(setfile,header=FALSE,sep=";",quote="\"",fill=TRUE,comment.char="#",stringsAsFactors = FALSE)
previousset<-convert2char(previousset)
file.copy(from=setfile,to = gsub(".csv",paste0("~",curdate,".csv"),setfile),overwrite = TRUE)
#previousset<-previousset[,c("V2","V3")]
previousset$V4<-gsub(",\\*,\\*,\\*,\\*,\\*,",",*,*,*,*,",previousset$V4)
previousset$V3[previousset$V3==""]<--999
previousset$V4[previousset$V4==""]<--999
previousset$n<-sapply(1:nrow(previousset),function(x) x)
# First look for matching variableUIDs
mergeset<-merge(previousset,avmeas,by.x="V1",by.y = "variableUID",all.x=TRUE,sort=FALSE)
mergeset[is.na(mergeset)]<-""
mergenotOK<-mergeset[mergeset$metastring=="",]
mergenotOK<-mergenotOK[!grepl("^\\*",mergenotOK$V2),]

if(curdate=="20170421"){
    #Check original IDEAg-NG file from Sandra. 
    #This file contains some string-elements that are manually changed 
    #(to make it more intuitive), which make the automatic comparison difficult.
    #Some variableUIDs are 'wrong' or have been changed
    #Also the eugirp-algorithm has changed.
    
    #Need to check remaining one by one
    curstr<-"EM,no gas,kt N/year,3.B.2.3,Swine,"
    curuid<-"3B1A4414-A756-4FC6-9553-0E15F1D76654"
    previousset[previousset$V2==curuid,2]<-"C0CBD1EE-FE26-4671-8D3E-EDD08C6E08DF"
    
    curstr<-"POP,no gas,1000s,3.B.1.3,Swine,*,Manure Management"
    curuid<-"3CCE5B8B-8744-4710-898C-E0EC5BEDE9D1"
    previousset[previousset$V2==curuid,2]<-"BC5A63BA-2DF1-45B8-9B58-3E3AD8C6EE61"
    
    curstr<-"EM,no gas,kt N/year,3.B.2.3,Swine,Pasture,Manure Management"
    curuid<-"4D716761-7CC7-4C90-884A-55659E08CA04"
    previousset[previousset$V2==curuid,2]<-"3FB36E10-9143-4C0C-968B-A2479C3EE2DB"
    
    curstr<-"EM,N2O,kt,3.B.2.3,Swine,*,Manure Management,*,*,*,*,Emissions"
    curuid<-"AB1CC8F6-D71C-46A1-A846-B5E76E2DE3A2"
    previousset[previousset$V2==curuid,2]<-"F56815AC-FE4F-4E37-A7D4-965970F1CF23"
    
    curstr<-"EM,CH4,kt,3.A.1,Cattle,*,Enteric Fermentation,*,*,*,*,Emissions"
    curuid<-"eugirp3A100CattlEM000000000000CH0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3A100CattttleEM00000000000CH0000"
    
    curstr<-"POP,no gas,1000s,3.A.1,Cattle,*,Enteric Fermentation"
    curuid<-"eugirp3A100CattlPOP00000000000no0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3A100CattttlePOP0000000000no0000"
    
    curstr<-"EM,CH4,kt,3.B.1.1,Cattle,*,Manure Management,*,*,*,*,Emissions"
    curuid<-"eugirp3B110CattlEM000000000000CH0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B110CattttleEM00000000000CH0000"
    
    curstr<-"EM,N2O,kt,3.B.2.1,Cattle,*,Manure Management,*,*,*,*,Emissions"
    curuid<-"eugirp3B210CattlEM000000000000N20000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleEM00000000000N20000"
    
    curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,*,Manure Management"
    curuid<-"eugirp3B210CattlEM000000000000no0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleEM00000000000no0000"
    
    curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Liquid,Manure Management"
    curuid<-"eugirp3B210CattlNEX00000Liq000no0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleNEX00000Li000no0000"
    
    curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Pasture,Manure Management"
    curuid<-"eugirp3B210CattlNEX00000Pas000no0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleNEX00000Pa000no0000"
    
    curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Solid,Manure Management"
    curuid<-"eugirp3B210CattlNEX00000Sol000no0000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleNEX00000So000no0000"
    
    curstr<-"NRATE,no gas,kg N/head/year,3.B.2.1,Cattle,*,Manure Management"
    curuid<-"eugirp3B210CattlNRA00000000000N20000"
    previousset[previousset$V2==curuid,2]<-"eugirp3B210CattttleNRA0000000000N20000"
    
    # Second look for matching variableUIDs
    mergeset<-merge(previousset,avmeas,by.x="V2",by.y = "variableUID",all.x=TRUE,sort=FALSE)
    mergeset[is.na(mergeset)]<-""
    mergenotOK<-mergeset[mergeset$metastring=="",]
    mergenotOK<-mergenotOK[!grepl("^\\*",mergenotOK$V2),]
    mergenotOK<-mergenotOK[!grepl("^SET",mergenotOK$V2),]
    mergenotOK<-mergenotOK[!grepl("^\\/",mergenotOK$V2),]
}

if(nrow(mergenotOK)!=0){
    View(mergenotOK)
    stop("There are still problems:")
}


mergeset<-mergeset[order(mergeset$n),]
mergeset<-mergeset[order(mergeset$n),c("V1","V2","metastring","V3")]
#Remove old metadata string
#mergeOK<-mergeOK[,-which(names(mergeOK)=="V3")]

sel<-mergeset$V3==-999
mergeset$new[sel]<-mergeset$V2[sel]
mergeset<-as.data.frame(mergeset[,c("new","V1","V2","metastring","V3")])

sel<-mergeset$metastring!="" & mergeset$V1=="variableUID"
mergeset$new[sel]<-paste0("\"",mergeset$V2[sel],"\" \"",mergeset$metastring[sel],"\"")

sel<-mergeset$V3!=-999 & mergeset$V1!="variableUID"
mergeset$new[sel]<-paste0("\"",mergeset$V2[sel],"\".(",mergeset$V3[sel],") \"",mergeset$metastring[sel],"\"")

sel<-grepl("@author",mergeset$new)
mergeset$new[sel]<-"*   @author   : Sandra Marquardt and Adrian Leip"
sel<-grepl("@date",mergeset$new)
mergeset$new[sel]<-paste0(mergeset$new[sel]," (SM), updated and modified 2017-04-21 (AL)")

write.table(mergeset$new,file="CAPRI_NIR_Sets.gms",quote = FALSE,row.names = FALSE)

