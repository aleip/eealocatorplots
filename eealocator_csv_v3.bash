ifil=$1
ofil=eealocator_$(date +%Y%m%d).txt

submission=20150115
ofilsub=eealocator_${submission}_$(date +%Y%m%d).txt


cut -d$'\t' -f1,2,6-10,13,14 $ifil | \
  sed -e 's/\t/,/g' | \
  sed -e 's/\]\[/,/g' | \
  sed -e 's/\[//g' | \
  sed -e 's/\]//g' | \
  sed -e 1's/,name,/,classification,category,source,method,target,option,type,measure,gas,unit,/g' \
  > $ofil

grep ^party $ofil > $ofilsub
grep $submission $ofil >>$ofilsub



cat > meta_data_dimensions.txt <<eof
dimension,acronym
# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
# File meta_data_dimensions.txt
# Created from the script eealocator_csv.bash on $timestamp
#         using the file meta_data_dimensions.csv
# Using EEA locator result file: $fil
#                   main script output: $filo
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
#
# Content of file: 
#   - dimensions extracted from the EEA-locator cube
#   - corresponding acronyms
#
eof

cat > lists.txt <<eof
# EU-GIRP (EU-Greenhouse gas Inventory Reporting Plots; eealocatorplots)
# File lists.txt
# Created from the script eealocator_csv.bash on $timestamp
#         using the file meta_data_dimensions.csv
# Using EEA locator result file: $fil
#                   main script output: $filo
# Adrian Leip <adrian.leip@jrc.ec.europa.eu>
#
# Content of file: 
#   - list of dimensions to be used in the EU-GIRP R script:
#   - Categories, sources, measures, gases, units, parties
#
eof
