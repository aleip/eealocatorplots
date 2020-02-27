nirfolder <- '\\\\ies-ud01.jrc.it/D5_agrienv/Projects\\eughginventory\\2019 submission\\MS_NIRs\\MS_NIR_March2019'
nirzips <- list.files(nirfolder, "zip")

for (x in 1:length(nirzips)){
  nirzfile <- nirzips[x]
  cnt <- substr(nirzfile, 1, 3)
  cntfiles <- unzip(paste0(nirfolder, "/", nirzfile), exdir=nirfolder, list=TRUE)
  unzip(paste0(nirfolder, "/", nirzfile), exdir=nirfolder)
  for(fn in cntfiles){
    file.rename(from=paste0(nirfolder, "/", fn), to=paste0(nirfolder, "/", cnt, "_", fn))
    
  }
}


