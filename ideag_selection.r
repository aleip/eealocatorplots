ideagdir<-"D:/Users/leipadr/adrian/OneDrive/ideagng/"

c_fertilizers_nationalinventories<-(allagri[grepl("^3.[G-I]",allagri$sector_number),])
write.csv(c_fertilizers_nationalinventories,file="c_fertilizers_nationalinventories")


selection<-allagri$meastype=="NEXC" & (!grepl("Option",allagri$sector_number))
Nexcretion_over_MMS<-(allagri[selection,])
r<-c("measure","party","sector_number","category","source",years)

o<-order(Nexcretion_over_MMS$party,Nexcretion_over_MMS$sector_number,Nexcretion_over_MMS$category)
Nexcretion_over_MMS<-Nexcretion_over_MMS[o,r]
write.csv(Nexcretion_over_MMS,file=paste0(ideagdir,"Nexcretion_over_MMS.csv"))


selection<-allagri$meastype=="CLIMA" & (!grepl("Option",allagri$sector_number))
clima_allocation<-(allagri[selection,])
r<-c("measure","party","sector_number","category","source","target",years)

o<-order(clima_allocation$party,clima_allocation$sector_number,clima_allocation$category)
clima_allocation<-clima_allocation[o,r]
write.csv(Nexcretion_over_MMS,file=paste0(ideagdir,"clima_allocation.csv"))
View(clima_allocation)

