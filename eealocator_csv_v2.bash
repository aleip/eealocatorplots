timestamp=$(date "+%Y%m%d %H:%M:%s") 
i=0
fil=lulucftest
fil=eealocatortest
fil=EEA_GHG_MMR_locator_20150318.cub
fil=EEA_GHG_MMR_locator_20150323.cub
#input file
fili=${fil}.csv

# Extract submission year, EF Used, Notation key, Method Applied
xx1=$(grep ^Submission $fili)
xx2=${xx1/Submission Year,/}
subyear=${xx2//,/}

#xx1=$(grep ^EF $fili)
#xx2=${xx1/EF Used,/}
#efused=${xx2//,/}

#xx1=$(grep ^Notation $fili)
#xx2=${xx1/Notation key,/}
#notation=${xx2//,/}

#xx1=$(grep ^Method $fili)
#xx2=${xx1/Method Applied,/}
#method=${xx2//,/}

#output file data
filo=${fil}_${subyear}_${efused}${notation}${method}.csv
filo=${fil}_${subyear}.csv

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

# Extract Parameters from the eealocator file
# (this needs to be done only if the parameter change)
# Note: the file must be manually updated then to insert the acronyms
# Note2: some manual editing so that Dairy CATT is looked for instead fo Dairy Cattle as Cattle is replaced earlier.
makenewlists=0
if [ $makenewlists == 1 ] ; then
  cut -d "]" -f1 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/ROW1,/' | sed -e 's/$/,/' > metadim_row1.txt
  cut -d "]" -f2 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/ROW2,/' | sed -e 's/$/,/' > metadim_row2.txt
  cut -d "]" -f3 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/ROW3,/' | sed -e 's/$/,/' > metadim_row3.txt
  cut -d "]" -f4 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/ROW4,/' | sed -e 's/$/,/' > metadim_row4.txt
  cut -d "]" -f5 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/ROW5,/' | sed -e 's/$/,/' > metadim_row5.txt
  cut -d "]" -f6 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/option,/' | sed -e 's/$/,/' > metadim_row6option.txt
  cut -d "]" -f7 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/type,/' | sed -e 's/$/,/' > metadim_row7type.txt
  cut -d "]" -f8 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/measure,/' | sed -e 's/$/,/' > metadim_row8measure.txt
  cut -d "]" -f9 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/gas,/' | sed -e 's/$/,/' > metadim_row9gas.txt
  cut -d "]" -f10 $fili | sed -e 's/,//g' | \
    grep -v '^Row\|^Submission\|^Value\|^$' | \
    sed -e 's/\[//g' | sed -e 's/\"//g' | sort -u | \
    sed -e 's/^/unit,/' | sed -e 's/$/,/' > metadim_row10unit.txt
  
  sed -e 's/,\(.*\),/,\1,\1/g' metadim_row10unit.txt > unit1.txt
  sed -e 's/\(CO2.*\)CO2 equivalent/\1CO2eq/g' unit1.txt > unit2.txt
  sed -e 's/\(k[gt] .*,.*k[gt]\) /\1/g' unit2.txt | \
    sed -e 's/\(k[gt] .*,.*k[gt]\) /\1/g'         | \
    sed -e 's/\(metric t .*,.*metric\) t/\1t/g'   | \
    sed -e 's/year/yr/g' \
    > unit3.txt
  
  grep -v ,,$ unit3.txt > metadim_row10unit.txt


  sed -e 's/gas,\(.*\),/gas,\1,\1/g' metadim_gas.txt > gas1.txt
  sed -e 's/Aggregate //g' gas1.txt > gas2.txt
  sed -e 's/Unspecified mix of /Mix/g' gas2.txt > gas3.txt
  sed -e 's/no gas/NO/g' gas3.txt > gas4.txt
  sed -e 's/-//g' gas4.txt > gas5.txt
  sed -e 's/ and /_/g' gas5.txt >  metadim_gas.txt
  abort
fi

cat metadim_row1.txt metadim_row2.txt metadim_row3.txt \
    metadim_row4.txt metadim_row5.txt metadim_row6option.txt \
    metadim_row7type.txt metadim_row8measure.txt \
    metadim_row9gas.txt metadim_row10unit.txt \
    > tmp0

#sed -e 's/"//g'  meta_data_dimensions.csv        > tmp0
sed -e 's/#//g'                            tmp0  > tmp1
#sed -e 's/^[A-Z,a-z,0-9]*,/i=$((i+1));var[$i]="/g' tmp1  > tmp2
#sed -e 's/^\(.*\),/\1";brf[$i]="/g'        tmp2  > tmp3
#sed -e 's/$/"/g'                           tmp3  > tmp4
sed -e 's/^[A-Z,a-z,0-9]*,\(.*\),\(.*\)/i=$((i+1));var[$i]="\1";brf[$i]="\2"/g' tmp1  > tmp4
sort -u -t";" -i tmp4 > tmp5
sed -e 's/\//\\\//g'                       tmp5  > tmp6
sed -e 's/^Dim/#Dim/g'                     tmp6  > meta_data_dimensions.bash
sed -e 's/^[A-Z,a-z,0-9]*,\(.*\),\(.*\)/"\1","\2"/g' tmp1  >> meta_data_dimensions.txt
i=0
. meta_data_dimensions.bash
numacronyms=$i




o=$i;i=0;cp $fili file$i

# First remove the "
i=0;o=1;sed -e 's/\"//g' file$i > file$o

i=$o;o=$((o+1)); sed -e 's/\]\[/,/g' file$i > file$o
i=$o;o=$((o+1)); sed -e 's/\]//g' file$i > file$o
i=$o;o=$((o+1)); sed -e 's/\[//g' file$i > file$o


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
#keep=${keep}'\|^-Total'
#keep=${keep}'\|^1\.AA-.*Emissions,Aggregate'
#keep=${keep}'\|^1\.AB-.*Emissions,Aggregate'
#keep=${keep}'\|^[12356].[A-Z]-.*Emissions,Aggregate'
#keep=${keep}'\|^|[12356]-.*Emissions,Aggregate'
#keep=${keep}'\|^4'


keep=${keep}'\|^Total,'
keep=${keep}'\|^Total (with LULUCF),'
keep=${keep}'\|^Total (without LULUCF),'
keep=${keep}'\|^Total (with LULUCF, with indirect),'
keep=${keep}'\|^Total (without LULUCF, with indirect),'
#list below obtained after assigning Categories in metadim_row1.txt:
# list=$(cut -d, -f3 metadim_row1.txt | sort -u)  
# for a in $list; do grep  $a metadim_row1.txt | cut -d, -f2; done

keep=${keep}'\|^Enteric Fermentation,'
keep=${keep}'\|^Manure Management,'
keep=${keep}'\|^Continuously Flooded,'
keep=${keep}'\|^Intermittently Flooded,'
keep=${keep}'\|^Irrigated,'
keep=${keep}'\|^Multiple aeration,'
keep=${keep}'\|^Single Aeration,'
keep=${keep}'\|^Continuously Flooded,'
keep=${keep}'\|^Intermittently Flooded,'
keep=${keep}'\|^Multiple aeration,'
keep=${keep}'\|^Single Aeration,'
keep=${keep}'\|^Single Aeration,'
keep=${keep}'\|^Multiple aeration,'
keep=${keep}'\|^Drought Prone,'
keep=${keep}'\|^Flood Prone,'
keep=${keep}'\|^Rainfed,'
keep=${keep}'\|^Flood Prone,'
keep=${keep}'\|^Drought Prone,'
keep=${keep}'\|^Deep Water,'
keep=${keep}'\|^Water Depth > 100 cm,'
keep=${keep}'\|^Water Depth 50-100 cm,'
keep=${keep}'\|^Water Depth 50-100 cm,'
keep=${keep}'\|^Water Depth > 100 cm,'
keep=${keep}'\|^Other Rice Cultivation,'
keep=${keep}'\|^Field Burning of Agricultural Residues,'
keep=${keep}'\|^Liming,'
keep=${keep}'\|^Urea Application,'
keep=${keep}'\|^Other Carbon-containing Fertilizers,'
keep=${keep}'\|^Rice Cultivation,'
keep=${keep}'\|^Agricultural Soils,'
keep=${keep}'\|^Agriculture,'
keep=${keep}'\|^CH4 Emissions,'
keep=${keep}'\|^CO2 Emissions,'



i=$o;o=$((o+1)); grep "${keep}" file$i > file$o

#Before replacing make sure the commas are only used as delimiter
i=$o;o=$((o+1));sed -e 's/CO2, CH4, N2O, HFCs, PFCs, SF6/CO2+CH4+N2O+HFCs+PFCs+SF6/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/CO2, CH4, N2O/CO2+CH4+N2O/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/LULUCF, with indirect/LULUCF with indirect/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/N excretion on pasture, range and paddock/N excretion on pasture range and paddock/g' file$i > file$o

acronym=1
until [ $acronym == $((numacronyms+1)) ] ; do

  tmpsed="s/${var[acronym]},/${brf[acronym]},/g"
  i=$o;o=$((o+1));sed -e "$tmpsed" file$i > file$o
  echo "sed -e '$tmpsed' file$i > file$o"
  acronym=$((acronym+1))
  rm file$i

done
#
i=$o;o=$((o+1));sed -e 's/Row Labels,Party code/category,source,row3,row4,row5,option,type,measure,gas,unit,party/g' file$i > file$o

#Group Sector - classification(s)
# Row 3 land categories (not required for now)
# Row 4 can be omitted 
# Row 5 indirect emissions
# Row 6 options (e.g. for cattle)
# Row 7 type - mainly for lulucf? omit for now
# Row 8-10: measure, gas, unit important extra column
i=$o;o=$((o+1))
awk -F "," '{
  printf "%s,",$1; 
  if($2 != "") printf "%s",$2; 
  if($5 != "") printf "%s",$5; 
  if($7 != "") printf "%s",$7;
  if($6 != "") printf "_%s",$6;
  printf ",%s",$8;
  printf ",%s",$9;
  printf ",%s",$10;
  for(i=11;i<=NF;i=i+1) printf ",%s",$i;
  printf "\n"
}' file$i > file$o

#missing values
i=$o;o=$((o+1));sed -e 's/,,/,-999,/g' file$i > file$o
i=$o;o=$((o+1));sed -e 's/,,/,-999,/g' file$i > file$o



#Keep only years from 1990 onwards
i=$o;o=$((o+1));cut -d "," -f1-6,7-   file$i > file$o

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
#rm meta_data_dimensions.bash

