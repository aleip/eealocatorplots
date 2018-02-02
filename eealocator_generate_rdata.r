
# ---> Read text-file if no RData file exists of if the text file is more recent
rdatfile<-paste0(csvfil,".RData")
if(!file.exists(rdatfile)){
    print(paste0("Load ",csvfil,".txt and generate new ",rdatfile))
    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999", quote = "")
    save(alldata,file=rdatfile)
    #}else if(file.info(paste0(csvfil,".txt"))$mtime>file.info(rdatfile)$mtime){
    #    print(paste0("Load updated",csvfil,".txt and generate new ",rdatfile))
    #    alldata<-read.csv(paste0(csvfil,".txt"),na.string="-999")
    #    save(alldata,file=rdatfile)
}else{
    print(paste0("Retrieve ",rdatfile))
    load(rdatfile)
}
