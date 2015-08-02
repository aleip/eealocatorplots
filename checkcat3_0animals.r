# Clean up Cattle options
# Some animal types are given under 'allmethods', the options either 'sector_number' and/or 'allmethods'
# ---> put them all to 'sector-number': sector option animal
cleanCattle<-function(S,M,type){
    if(type=="sector_number"){
        
        if(grepl("1 Dairy Cattle",S)){S<-gsub("1 ","1 Option A ",S)}else   
            if(grepl("1 Non-Dairy Cattle",S)){S<-gsub("1 ","1 Option A ",S)}else  
                if(grepl("Mature Dairy Cattle",S)){S<-gsub("Mature","Option B Mature",S)}else   
                    if(grepl("Other Mature Cattle",S)){S<-gsub("Other","Option B Other",S)}else    
                        if(grepl("Growing Cattle",S)){S<-gsub("Growing","Option B Growing",S)}else   
                            if(grepl("Option",S)){S<-paste0(S," Cattle")}else
                        {S}   
    } else
        if(type=="allmethods"){
            
           if(grepl("1 Dairy Cattle",S)){M<-gsub("_Option A","",M)}else   
                if(grepl("1 Non-Dairy Cattle",S)){M<-gsub("_Option A","",M)} else    
                    if(grepl("Mature Dairy Cattle",S)){M<-gsub("_Option B","",M)}else   
                        if(grepl("Other Mature Cattle",S)){M<-gsub("_Option B","",M)}else    
                            if(grepl("Growing Cattle",S)){M<-gsub("_Option B","",M)}else    
                                if(grepl("Option",S)){M<-gsub("Cattle","",gsub("_Option .","",M))}else
                            {M}   
        }
}

cleanlivestock<-function(S,M,type){
    j<-0
    for(i in c(1:length(div))){if(grepl(div[i],M)){j<-i}}
    if(type=="sector_number"){if(j!=0){paste0(S," ",div[j])}else{S}
    }else if(type=="allmethods"){if(j!=0){gsub(div[j],"",M)}else{M}}
}
div<-c("Cattle","Sheep","Swine","Livestock")
climateZones<-c("_Cool.*","_Temperate.*","_Warm.*")
manureSystems<-c("Anaerobic lagoon","Composting","Daily spread","Digesters","Liquid system","Solid storage and dry lot","Pasture  range and paddock","Other","Burned for fuel or as waste")
manureSysshrt<-c("anaer","compost","daily","digest","liquid","solid","pasture","other","burned")
cat3names<-c("sector_number","allmethods","climate","gas","meastype","unit","party",years,"variableUID")



# Remove duplicates if 'other' contains just on e element
# ---> use 'sub' instead of 'gsub' so that only the first occurrence is replaced
#      Other Other livestock is retained
# Remove Other Other livestock as no other 'Other' Livestock is reported (horses, poultry, etc.)
cat3all$allmethods<-unlist(lapply(c(1:nrow(cat3all)),function(x) gsub("livestock","Livestock",cat3all$allmethods[x])))
cat3all<-cat3all[! grepl("Other Other Livestock",cat3all$allmethods),]

cat3allnomethods<-unique(subset(cat3all,select=-variableUID))
cat3allnomethods$allmethods<-sub("Other ","",cat3allnomethods$allmethods)
cat3allnomethods<-unique(cat3allnomethods)
cat3allnomethods$variableUID<-cat3all$variableUID[row.names(cat3all)%in%row.names(cat3allnomethods)]
cat3all<-cat3allnomethods

rm(cat3allnomethods)

cat3allclean<-cat3all
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    cleanCattle(cat3allclean$sector_number[x],cat3allclean$allmethods[x],"allmethods")))
cat3allclean$sector_number<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    cleanCattle(cat3allclean$sector_number[x],cat3allclean$allmethods[x],"sector_number")))

cat3allclean$sector_number<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    if(grepl("_Option C",cat3allclean$allmethods[x])){
        paste0(cat3allclean$sector_number[x]," Option C")
    }else{cat3allclean$sector_number[x]}))
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    if(grepl("_Option C",cat3allclean$allmethods[x])){
        gsub("_Option C","",cat3allclean$allmethods[x])
    }else{cat3allclean$allmethods[x]}))



cat3allclean$sector_number<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    if(grepl("Other ",cat3allclean$allmethods[x])){paste0(cat3allclean$sector_number[x]," Other")}else{cat3allclean$sector_number[x]}))
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    if(grepl("Other ",cat3allclean$allmethods[x])){gsub("Other *","",cat3allclean$allmethods[x])}else{cat3allclean$allmethods[x]}))

cat3allclean$sector_number<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    cleanlivestock(cat3allclean$sector_number[x],cat3allclean$allmethods[x],"sector_number")))
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x)
    cleanlivestock(cat3allclean$sector_number[x],cat3allclean$allmethods[x],"allmethods")))

# The word 'Farming' in 'allmethods' not required for upper-level animal types/category
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) gsub("Farming","",cat3allclean$allmethods[x])))

# The work 'Tier 2' not required: differentiation by climate zone by MMS always Tier 2
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) gsub("_Tier 2","",cat3allclean$allmethods[x])))

# Store climate zone in separate column
cat3allclean$climate<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) 
    if(grepl("Cool",cat3allclean$allmethods[x])){"Cool"}else 
        if(grepl("Warm",cat3allclean$allmethods[x])){"Warm"}else 
            if(grepl("Temperate",cat3allclean$allmethods[x])){"Temperate"}else{""}))
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) 
    gsub("_Cool","",gsub("_Warm","",gsub("_Temperate","",cat3allclean$allmethods[x])))))

# Final cleaning
cat3allclean$allmethods<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) gsub("^_","",cat3allclean$allmethods[x])))
cat3allclean$sector_number<-unlist(lapply(c(1:nrow(cat3allclean)),function(x) gsub("\\(please specify\\) ","",cat3allclean$sector_number[x])))

# Remove remaining duplicates
cat3allnomethods<-unique(subset(cat3allclean,select=-variableUID))
cat3allnomethods<-unique(cat3allnomethods)
cat3allnomethods$variableUID<-cat3allclean$variableUID[row.names(cat3allclean)%in%row.names(cat3allnomethods)]

cat3all<-cat3allnomethods[,cat3names]
cat3alltab<-unique(subset(cat3all,select=names(cat3all[!names(cat3all)%in%c("party","unique",years)])))
cat3emp<-as.data.frame(matrix(rep(0,ncol(cat3all)),nrow=1,ncol=ncol(cat3all)))
names(cat3emp)<-names(cat3all)
#rm(cat3allclean)


# Identify the animal types available
#cat3Aanimals<-subset(cat3all,grepl("^3.A",sector_number),select=c(sector_number,allmethods,climate))
cat3Aanimals<-subset(cat3all,grepl("^3.A",sector_number),select=c(sector_number))
cat3Aanimals<-unique(cat3Aanimals)
cat3Aanimals$sector_number<-gsub("3.A","",cat3Aanimals$sector_number)

# Methods are lumped together - here the MMS and climate region must be eliminated again
#cat3B1animals<-subset(cat3all,grepl("^3.B.1",sector_number),select=c(sector_number,allmethods,climate))
cat3B1animals<-subset(cat3all,grepl("^3.B.1",sector_number),select=c(sector_number))
cat3B1animals<-unique(cat3B1animals)
cat3B1animals$sector_number<-gsub("3.B.1","",cat3B1animals$sector_number)

#cat3B2animals<-subset(cat3all,grepl("^3.B.2",sector_number),select=c(sector_number,allmethods,climate))
cat3B2animals<-subset(cat3all,grepl("^3.B.2",sector_number),select=c(sector_number))
cat3B2animals<-unique(cat3B2animals)
cat3B2animals$sector_number<-gsub("3.B.2","",cat3B2animals$sector_number)


