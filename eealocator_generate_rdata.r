
# ---> Read text-file if no RData file exists of if the text file is more recent
rdatfile<-paste0(csvfil,".RData")
if(!file.exists(rdatfile)){
    print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999", quote = "")
    names(alldata)[1] <- "party_2char"
    alldata$party_3char <- substring(alldata$filename, 1, 3)
    party_2_3char <- unique(alldata[,names(alldata) %in% c("party_2char", "party_3char", "country_name")])
    View(party_2_3char)
    write.csv(party_2_3char, paste0(csvfil1, "party_2_3char.csv"), row.names = FALSE)
    
    answ <- readline(prompt = "Please, check country codes in 'party_2_3char', \n(notice that weird countries like 'no gas', etc., will be cleaned later). \nIf you find errors, quit by pressing [n]+[enter]; any other key to continue...")
    
    if (answ == "n") stop("killing...")

    
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
