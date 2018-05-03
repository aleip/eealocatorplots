
path <- "E:\\ghginventory\\eealocatorplots"

r_scripts <- list.files(path = path, pattern = "\\.r$", full.names = TRUE)
r_scripts <- r_scripts[-60]
r_scripts <- r_scripts[!grepl("eugirp_definitions", r_scripts)]

eugirp_attributes <- paste0(path, "/eugirp_attributes.txt")
solvedfile <- paste0("E:\\ghginventory\\ecir\\checks/solvedissues14122017.xlsx")
faocountries <- paste0(path, "/faocountries.csv")


r_scripts <- c(r_scripts, eugirp_attributes, solvedfile, faocountries)

ctries <- read.csv("E:\\ghginventory\\2018\\eealocator\\party_2_3char.csv")
nrow(ctries)
ctries <- ctries[!ctries$party_2char %in% c("2018", "no gas", "20180319", "no type", "no method"),]
ctries


for (ct in as.vector(ctries$party_2char)){
  print(ct)
  ct_2 <- ct
  ct_3 <- as.vector(ctries[ctries$party_2char %in% ct, 3])

  for(r in r_scripts){
    print(r)
    if(grepl("*.xlsx", r)){
      library(readxl)
      library(xlsx)
      solvedfile <- as.data.frame(read_excel(path=paste0("E:\\ghginventory\\ecir/checks/","solvedissues14122017.xlsx"), sheet = 1))
      solvedfile$party <- gsub(paste0("\\<",ct_2,"\\>"), ct_3, solvedfile$party)
      solvedfile$issuenr <- gsub(paste0("\\<",ct_2), ct_3, solvedfile$issuenr)
      write.xlsx(solvedfile, paste0("E:\\ghginventory\\ecir/checks/","solvedissues14122017.xlsx"),showNA = FALSE, row.names = FALSE)
      #unq(solvedfile$issuenr)
      
    }else{
      x <- readLines(r)
      y <- gsub(paste0("\\<",ct_2,"\\>"), ct_3, x)
      cat(y, file=r, sep="\n")
      
    }

    }
}
