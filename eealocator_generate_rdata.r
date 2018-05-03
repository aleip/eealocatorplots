
# ---> Read text-file if no RData file exists of if the text file is more recent
rdatfile<-paste0(csvfil,".RData")
if(!file.exists(rdatfile)){
    print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999", quote = "")
    names(alldata)[1] <- "party_2char"
    alldata$party_3char <- substring(alldata$filename, 1, 3)
    party_2_3char <- unique(alldata[,names(alldata) %in% c("party_2char", "party_3char", "country_name")])

    party_2_3char <- merge(country4sub, party_2_3char, by.x = c("code2"), by.y = c("party_2char"), all = TRUE)
    party_2_3char <- party_2_3char[(!party_2_3char$country_name == "" | is.na(party_2_3char$country_name)) & !is.na(party_2_3char$long), ]
    party_2_3char <- party_2_3char[!as.vector(apply(party_2_3char, 1, function(x) sum(is.na(x))) == ncol(party_2_3char)), ]
    party_2_3char$party_3char <- ifelse(is.na(party_2_3char$party_3char) == TRUE, party_2_3char$code3, party_2_3char$party_3char) 
    party_2_3char$country_name <- ifelse(is.na(party_2_3char$country_name) == TRUE, party_2_3char$long, as.vector(party_2_3char$country_name)) 
    party_2_3char$code3 <- party_2_3char$party_3char
    party_2_3char$long <- party_2_3char$country_name
    party_2_3char <- party_2_3char[, !names(party_2_3char) %in% c("party_3char", "country_name")]
    
    View(party_2_3char)
    write.csv(party_2_3char, paste0(csvfil1, "party_2_3char.csv"), row.names = FALSE)

    answ <- readline(prompt = "Please, check country codes in 'party_2_3char'.\nIf you find errors, quit by pressing [n]+[enter]; \notherwise, press [enter] or any other key to update 'country4sub' and to continue...")
    
    if (answ == "n") stop("killing...")

    country4sub <- party_2_3char
    
    alldata <- alldata %>% select(party_3char, names(alldata)[-c(1,3)])
    names(alldata)[1] <- "party"
    
    print(paste0("saving  ", rdatfile))
    save(alldata,file=rdatfile)
    #}else if(file.info(paste0(csvfil,".txt"))$mtime>file.info(rdatfile)$mtime){
    #    print(paste0("Load updated",csvfil,".txt and generate new ",rdatfile))
    #    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
    #    save(alldata,file=rdatfile)
}else{
    print(paste0("Retrieve ",rdatfile))
    load(rdatfile)
}
