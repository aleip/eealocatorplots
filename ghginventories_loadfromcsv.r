# Definitions
csvfil <-"sector4_allbrf.csv"


# CSV file generated as follows:
# - Load EEA locator cube in excel pivot table
#   - Report Filter: Submission Year
#   - Column Label: Inventory Year
#   - Row Labels: Variable name, Party code, Notation key
# Export table to csv sector4_all.csv
# Process csv with eealocator_csv.bash to sector4_allbrf.csv
#   Main tasks
#   - Replace long names with acronyms (defined in excel file meta_data_dimensions.xlsx
#     which needs to be exported as csv meta_data_dimensions.csv)
#   - Combine dimension-columns and remove unnecessary ones
#     Currently: 1. category and source (categorysource)
#                2. measure and gas (measuregas)
#                3. unit
#                4. party
#                5. values for years 1990ff (not those before 1990)
sector4<-read.csv(csvfil,na.string="-999")

# Lists of all categorysources, measuregases, units, partys 
# - In the following partys are rows, units are not required (now)
#   and categorysources and measuregases are distributed over individual tables
source("lists.txt")

# Restrict here number of tables to generate (for testing)
meas<-meas[1:2]
cats<-cats[2:2]


for (mea in meas) {
  tmp<-subset(sector4,measuregas==mea,select=-measuregas);assign(paste("ghginv.",mea,sep=""),tmp)
  for (cat in cats) {
    tmp1<-subset(tmp,categorysource==cat,select=c(-categorysource,-unit));
    p<-subset(tmp1,select=party)
    x<-subset(tmp1,select=-party)
    y<-as.matrix(sapply(x,as.numeric))
    row.names(y)<-p$party
    assign(paste("ghgx.",mea,".",cat,sep=""),y)
  }}


