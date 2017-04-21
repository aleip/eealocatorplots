
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

previousset<-read.csv("CAPRI_NIR_Sets.csv",header=FALSE,sep=";",quote="\"",fill=TRUE,comment.char="#",stringsAsFactors = FALSE)
#previousset<-previousset[,c("V2","V3")]
previousset$V3<-gsub(",\\*,\\*,\\*,\\*,\\*,",",*,*,*,*,",previousset$V3)

# First look for matching variableUIDs
mergeset<-merge(previousset,avmeas,by.x="V1",by.y = "variableUID",all.x=TRUE)
mergeset[is.na(mergeset)]<-""
mergeOK<-mergeset[mergeset$metastring!="",]

previousset<-mergeset[mergeset$metastring=="",c("V1","V2","V3")]
previousset<-convert2char(previousset)

#Need to check remaining one by one
curstr<-"EM,no gas,kt N/year,3.B.2.3,Swine,"
curuid<-"3B1A4414-A756-4FC6-9553-0E15F1D76654"
previousset[previousset$V1==curuid,1]<-"C0CBD1EE-FE26-4671-8D3E-EDD08C6E08DF"

curstr<-"POP,no gas,1000s,3.B.1.3,Swine,*,Manure Management"
curuid<-"3CCE5B8B-8744-4710-898C-E0EC5BEDE9D1"
previousset[previousset$V1==curuid,1]<-"BC5A63BA-2DF1-45B8-9B58-3E3AD8C6EE61"

curstr<-"EM,no gas,kt N/year,3.B.2.3,Swine,Pasture,Manure Management"
curuid<-"4D716761-7CC7-4C90-884A-55659E08CA04"
previousset[previousset$V1==curuid,1]<-"3FB36E10-9143-4C0C-968B-A2479C3EE2DB"

curstr<-"EM,N2O,kt,3.B.2.3,Swine,*,Manure Management,*,*,*,*,Emissions"
curuid<-"AB1CC8F6-D71C-46A1-A846-B5E76E2DE3A2"
previousset[previousset$V1==curuid,1]<-"F56815AC-FE4F-4E37-A7D4-965970F1CF23"

curstr<-"EM,CH4,kt,3.A.1,Cattle,*,Enteric Fermentation,*,*,*,*,Emissions"
curuid<-"eugirp3A100CattlEM000000000000CH0000"
previousset[previousset$V1==curuid,1]<-"eugirp3A100CattttleEM00000000000CH0000"

curstr<-"POP,no gas,1000s,3.A.1,Cattle,*,Enteric Fermentation"
curuid<-"eugirp3A100CattlPOP00000000000no0000"
previousset[previousset$V1==curuid,1]<-"eugirp3A100CattttlePOP0000000000no0000"

curstr<-"EM,CH4,kt,3.B.1.1,Cattle,*,Manure Management,*,*,*,*,Emissions"
curuid<-"eugirp3B110CattlEM000000000000CH0000"
previousset[previousset$V1==curuid,1]<-"eugirp3B110CattttleEM00000000000CH0000"

curstr<-"EM,N2O,kt,3.B.2.1,Cattle,*,Manure Management,*,*,*,*,Emissions"
curuid<-"eugirp3B210CattlEM000000000000N20000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleEM00000000000N20000"

curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,*,Manure Management"
curuid<-"eugirp3B210CattlEM000000000000no0000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleEM00000000000no0000"

curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Liquid,Manure Management"
curuid<-"eugirp3B210CattlNEX00000Liq000no0000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleNEX00000Li000no0000"

curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Pasture,Manure Management"
curuid<-"eugirp3B210CattlNEX00000Pas000no0000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleNEX00000Pa000no0000"

curstr<-"EM,no gas,kt N/year,3.B.2.1,Cattle,Solid,Manure Management"
curuid<-"eugirp3B210CattlNEX00000Sol000no0000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleNEX00000So000no0000"

curstr<-"NRATE,no gas,kg N/head/year,3.B.2.1,Cattle,*,Manure Management"
curuid<-"eugirp3B210CattlNRA00000000000N20000"
previousset[previousset$V1==curuid,1]<-"eugirp3B210CattttleNRA0000000000N20000"

# Second look for matching variableUIDs
mergeset<-merge(previousset,avmeas,by.x="V1",by.y = "variableUID",all.x=TRUE)
mergeset[is.na(mergeset)]<-""
mergeOK2<-mergeset[mergeset$metastring!="",]
previousset<-mergeset[mergeset$metastring=="",c("V1","V2","V3")]

mergeOK<-rbind(mergeOK,mergeOK2)

showm<-function(curuid){
    curuid<-gsub("\\*","\\\\*",curuid)
    print(curuid)
    s1<-as.character(filter(avmeas,grepl(curuid,metastring))[2])
    s2<-as.character(unique(filter(previousset,grepl(curuid,V3))[3]))
    cat("\n",s1,"\n",s2)
    s1<-as.character(filter(avmeas,grepl(curuid,metastring))[1])
    s2<-as.character(unique(filter(previousset==,grepl(curuid,V3))[1]))
    cat("\n",s1,"\n",s2)
}



# #Some strings had been constructed without 
# avmeas$metastring<-gsub("Manure Management.*","Manure Management",avmeas$metastring)
# mergeset2<-merge(previousset,avmeas,by.x="V3",by.y = "metastring",all.x=TRUE)
# mergeset2[is.na(mergeset2)]<-""
# 
# addvaruid<-function(x){
#      if(mergeset$variableUID[x]==""){
#             n<-paste0(mergeset$variableUID[x],mergeset2$variableUID[x])
#         }else{
#             n<-mergeset$variableUID[x]
#         }
#    return(n)
# }
# 
# mergeset$variableUID2<-sapply(1:nrow(mergeset),function(x) addvaruid(x))
# 
# #Replace other livestock with other
# avmeas$metastring<-gsub("Other Livestock","Other",avmeas$metastring)
# mergeset2<-merge(previousset,avmeas,by.x="V3",by.y = "metastring",all.x=TRUE)
# mergeset2[is.na(mergeset2)]<-""
# mergeset$variableUID2<-sapply(1:nrow(mergeset),function(x) addvaruid(x))


curuid<-"81698429-9A0A-4581-A5FD-E2E51136F494"
showme<-function(curuid){
    s1<-as.character(filter(avmeas,grepl(curuid,variableUID))[2])
    s2<-as.character(filter(previousset,grepl(curuid,V1))[3])
    cat("\n",s1,"\n",s2)
}

