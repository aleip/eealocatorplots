#' This file indicates the results generated by the EU-GIRP library
#' based on the submissions of national greenhouse gas inventories by
#' EU memeber states.
#' See https://github.com/aleip/eealocatorplots
# 
#' The program runs in several steps performing various tasks from 
#' data cleaning over data aggregation, data consistency and correction,
#' plotting, data quality checks, and comparisons with other data sources.
#' After each step the file 'eealocator_cursubm_clean.RData' is generated.
# 
# cursubm indicates the date of the national submissions to the EU GHG inventory system:
# Note: if this file is sources it returns two vectors:
#       - cursubm indicating the date of the current submissions.
#       - files2upload indicating the 
#cursubm <- '20190115'
if(nrow(drive_find(paste0("eugirp"))) == 0) drive_mkdir("eugirp")
if(!cursubm %in% drive_ls("eugirp/")$name) drive_mkdir(cursubm, "eugirp")

replacecursubm <- function(files, cursubm1 = cursubm){
  x <- gsub("cursubm", cursubm1, files)
  return(x)
}

uploadfile <- function(rpfile, lpfile, nfile){
  drive_rm(file = paste0(rpfile, "/",replacecursubm(nfile)), verbose = TRUE)
  drive_upload(path = as_dribble(rpfile), media = replacecursubm(paste0(lpfile, nfile)), verbose = TRUE)
}
uploadfile(rpfile = "eugirp/rdatabase", lpfile = paste0("../", invyear, "/eealocator/"), nfile = "eealocator_cursubm_clean.RData")
uploadfile(rpfile = "eugirp/rdatabase", lpfile = paste0("../", invyear, "/eealocator/"), nfile = "eealocator_cursubm_clean.RData")



# The following gives a list of files that should be uploaded to 
# the googledrive under the subfolder cursubm
# Files that are generated with the date appended (e.g. ~20190320)
# should be uploaded without this appendix (so that only the last version is kept there).

files2upload <- replacecursubm(c(

  # Last update of the data is done in step 7, therefore the latest generation of step-7 is 
  # added to the data repository as well
  #paste0("../", invyear, "/eealocator/eealocator_cursubm_clean.RData"),
  #paste0("../", invyear, "/eealocator/eealocator_cursubm_clean_s7~", figdate, ".RData"),
  
  # Main agri-data in tabular format
  paste0("../", invyear, "/eealocator/eealocator_cursubm_agri.csv"),
  
  # Files for the EU CRF Tables
  #  --- from ecir/eealocator
  paste0("../ecir/tables4eu/tablett3_cursubm", ".RData"),
  paste0("../ecir/tables4eu/tablett3bb_cursubm", ".csv"),
  paste0("../ecir/tables4eu/tablett3bas1_cursubm", ".csv"),
  paste0("../ecir/tables4eu/tablet3as2_cursubm", ".csv"),
  paste0("../ecir/tables4eu/tablet3as1_cursubm", ".csv"),
  paste0("../ecir/tables4eu/table3s1_cursubm", ".csv"),

  # Files serving for the CAPRI comparison should
  # go to a sub-folder 'capri'
  # 
  #  --- from ecir/eealocator
  paste0("../ecir/tables4eu/agridet_emissions4capri_cursubm", ".csv"),
  paste0("../ecir/tables4eu/agridet_emissions4capri_cursubm", ".csv"),
  
  # --- from eealocatorplots
  "../eealocatorplots/eugirp_exportUIDs4capri.r",
  "../eealocatorplots/CAPRI_NIR_Sets.gms",
  "../eealocatorplots/caprieugirpinterface.r",
  "../eealocatorplots/curplot.r",
  "../eealocatorplots/curunit.r",
  "../eealocatorplots/eugirp_attributes.r",
  "../eealocatorplots/eugirp_attributes.txt",
  "../eealocatorplots/eugirp_definitions.r",
  "../eealocatorplots/eugirp_functions.r",
  "../eealocatorplots/eugirp_funnirplots.r",
  "../eealocatorplots/eugirp_prepareplots.r",
  "../eealocatorplots/eugirp_texts.r",
  "../eealocatorplots/eugirp_writeissues.r",
#  "../../CAPRImodel/epnf/gams/comparisonplots/load_eea_data.r",
#  "../../CAPRImodel/epnf/gams/comparisonplots/plotdata.rdata",
  "../eealocatorplots/plots_sec1.txt",
  "../eealocatorplots/plots_sec2.txt",
  "../eealocatorplots/plots_sec2othergases.txt",
  "../eealocatorplots/plots_sec4.txt",
  "../eealocatorplots/plots_sec5.txt",
  "../eealocatorplots/plots_sec125.txt",
  "../eealocatorplots/rel.rdata",
  "../eealocatorplots/uid_to_ghg.set",
  "../eealocatorplots/variableuid.set"
))

for(f2d in files2upload){
  drive_rm(file = paste0(cursubm, "/", sub('.*\\/', '', f2d)), verbose = TRUE)
  #drive_update(file = paste0(cursubm, "/", sub('.*\\/', '', f2d)), media = f2d, verbose = TRUE)
  drive_upload(path = as_dribble(paste0("eugirp/", cursubm, "/")), media = f2d, verbose = TRUE)
}

