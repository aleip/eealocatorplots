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
replacecursubm <- function(files, cursubm1 = cursubm){
  x <- gsub("cursubm", cursubm1, files)
  return(x)
}

# The following gives a list of files that should be uploaded to 
# the googledrive under the subfolder cursubm
# Files that are generated with the date appended (e.g. ~20190320)
# should be uploaded without this appendix (so that only the last version is kept there).

files2upload <- replacecursubm(c(

  # Last update of the data is done in step 7, therefore the latest generation of step-7 is 
  # added to the data repository as well
  paste0(csvfil1, "eealocator_cursubm_clean.RData"),
  paste0(csvfil1, "eealocator_cursubm_clean_s7~", figdate, ".RData"),
  
  # Main agri-data in tabular format
  paste0(csvfil1, "eealocator_20190315_agri.csv"),
  
  # Files for the EU CRF Tables
  #  --- from ecir/eealocator
  paste0("../ecir/eealocator/tablett3_cursubm~", figdate, ".RData"),
  paste0("../ecir/eealocator/tablett3bb_cursubm~", figdate, ".csv"),
  paste0("../ecir/eealocator/tablett3bas1_cursubm~", figdate, ".csv"),
  paste0("../ecir/eealocator/tablet3as2_cursubm~", figdate, ".csv"),
  paste0("../ecir/eealocator/tablet3as1_cursubm~", figdate, ".csv"),
  paste0("../ecir/eealocator/table3s1_cursubm~", figdate, ".csv"),

  # Files serving for the CAPRI comparison should
  # go to a sub-folder 'capri'
  # 
  #  --- from ecir/eealocator
  paste0("../ecir/eealocator/agridet_emissions4capri", figdate, ".csv"),
  
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
  "../../CAPRImodel/epnf/gams/comparisonplots/load_eea_data.r",
  "../../CAPRImodel/epnf/gams/comparisonplots/plotdata.rdata",
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