timestamp=$(date "+%Y%m%d %H:%M:%s")

i=0
fil=lulucftest
fil=eealocatortest
#input file
fili=${fil}.csv

# Extract submission year, EF Used, Notation key, Method Applied
xx1=$(grep ^Submission $fili)
xx2=${xx1/Submission Year,/}
subyear=${xx2//,/}

xx1=$(grep ^EF $fili)
xx2=${xx1/EF Used,/}
efused=${xx2//,/}

xx1=$(grep ^Notation $fili)
xx2=${xx1/Notation key,/}
notation=${xx2//,/}

xx1=$(grep ^Method $fili)
xx2=${xx1/Method Applied,/}
method=${xx2//,/}

#output file data
filo=${fil}_${subyear}_${efused}${notation}${method}.csv

cat > meta_data_dimensions.txt <<eof
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

sed -e 's/"//g'  meta_data_dimensions.csv        > tmp0
sed -e 's/#//g'                            tmp0  > tmp1
sed -e 's/^[A-Z]*,/i=$((i+1));var[$i]="/g' tmp1  > tmp2
sed -e 's/^\(.*\),/\1";brf[$i]="/g'        tmp2  > tmp3
sed -e 's/$/"/g'                           tmp3  > tmp4
sed -e 's/\//\\\//g'                       tmp4  > tmp5
sed -e 's/^Dim/#Dim/g'                     tmp5  > meta_data_dimensions.bash
sed -e 's/^[A-Z]*,/"/g'                    tmp1  > tmp2a
sed -e 's/^\(.*\),/\1","/g'                tmp2a > tmp3a
sed -e 's/$/"/g'                           tmp3a > tmp4a
sed -e 's/Dimension type,Dimension instance name/"dimension/g' tmp4a >> meta_data_dimensions.txt
. meta_data_dimensions.bash
numacronyms=$i




o=$i;i=0;cp $fili file$i

# First remove the "
i=0;o=1;sed -e 's/\"//g' file$i > file$o

# Then remove the non-wanted gases
i=$o;o=$((o+1)); grep -v '^Grand Total\|NMVOC\|,CO,\|SO2\|Please specify gas\|Reference Approach\|^,' file$i > file$o
i=$o;o=$((o+1)); grep -v '^Submission\|^EF\|^Method\|^Notation\|^,\|^Value' file$i > file$o

# Move the first rows into the meta-model file

# Now extract those info that is required
# Emissions at high aggregation level for all categories
# All information for sector 4
#keep='^Submission'
#keep=${keep}'\|^EF'
#keep=${keep}'\|^Notation'
#keep=${keep}'\|^Method'
keep=""
keep=${keep}'^Row'
keep=${keep}'\|^-Total'
keep=${keep}'\|^1\.AA-.*Emissions,Aggregate'
keep=${keep}'\|^1\.AB-.*Emissions,Aggregate'
keep=${keep}'\|^[12356].[A-Z]-.*Emissions,Aggregate'
keep=${keep}'\|^|[12356]-.*Emissions,Aggregate'
keep=${keep}'\|^4'

i=$o;o=$((o+1)); grep "${keep}" file$i > file$o

#Before replacing make sure the commas are only used as delimiter
i=$o;o=$((o+1));sed -e 's/CO2, CH4, N2O, HFCs, PFCs, SF6/CO2+CH4+N2O+HFCs+PFCs+SF6/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/CO2, CH4, N2O/CO2+CH4+N2O/g' file$i > file$o

acronym=1
until [ $acronym == $((numacronyms+1)) ] ; do

  tmpsed="s/${var[acronym]}/${brf[acronym]}/g"
  i=$o;o=$((o+1));sed -e "$tmpsed" file$i > file$o
  echo "sed -e '$tmpsed' file$i > file$o"
  acronym=$((acronym+1))
  rm file$i

done
#
i=$o;o=$((o+1));sed -e 's/Row Labels,Party code/category,,source,measure,gas,,unit,party/g' file$i > file$o

#Group Sector - classification(s)
i=$o;o=$((o+1))
awk -F "," '{
  printf "%s,",$1; 
  if($3 != "") printf "%s",$3; 
  if($6 != "") printf "%s",$6;
  printf ",%s,",$4;
  if($5 != "") printf "%s",$5;
  printf ",%s",$7;
  printf ",%s",$8;
  for(i=10;i<NF;i=i+1) printf ",%s",$i;
  printf "\n"
}' file$i > file$o

#missing values
i=$o;o=$((o+1));sed -e 's/,,/,-999,/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/,,/,-999,/g' file$i > file$o



#Keep only years from 1990 onwards
i=$o;o=$((o+1));cut -d "," -f1-6,12-   file$i > file$o

#List of categories
filelist=file$o
categ=$(grep -v ^category $filelist | cut -d "," -f1 | sort -u)

#List of sources
filelist=file$o
sourc=$(grep -v ^category $filelist | cut -d "," -f2 | sort -u)

#List of measures
measu=$(grep -v ^category $filelist | cut -d "," -f3 | sort -u)

#List of gases
gases=$(grep -v ^category $filelist | cut -d "," -f4 | sort -u)

  #List of units
units=$(grep -v ^category $filelist | cut -d "," -f5 | sort -u)

#List of partys
party=$(grep -v ^category $filelist | cut -d "," -f6 | sort -u)

echo "categ<-c(" $categ ")" > tmplist.txt
echo "sourc<-c(" $sourc ")" >> tmplist.txt
echo "measu<-c(" $measu ")" >> tmplist.txt
echo "gases<-c(" $gases ")" >> tmplist.txt
echo "units<-c(" $units ")" >> tmplist.txt
echo "party<-c(" $party ")" >> tmplist.txt

  sed -e 's/ /","/g' tmplist.txt | \
  sed -e 's/(","/("/g' | \
  sed -e 's/",")/")/g' >> lists.txt


cp file$o $filo
rm tmp*
rm file*
rm meta_data_dimensions.bash

