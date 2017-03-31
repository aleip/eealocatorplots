ifil=$1
ofil=eealocator_$(date +%Y%m%d).txt

# To be adapted: ifil, direct and submission
submission=20170317

ifil=CRF_MMR_20160202.txt
ifil=CRF_MMR_20160322.txt
ifil=CRF_MMR_20160420.txt
ifil=CRF_MMR_20160810.txt
ifil=CRF_MMR_20170123.txt
ifil=CRF_MMR_${submission}.txt
direct=../2017/eealocator/

ifil=${direct}/${ifil}

ofilsub=eealocator_${submission}.txt
      #party   country_name    filename        generationtimestamp     variableUID     year    value   notation        sector_number   name    submission_version      submission_year
cut -d$'\t' -f1,2,5-12 $ifil | \
  sed -e 's/,/ /g' | \
  sed -e 's/\t/,/g' | \
  sed -e 's/\]\[/,/g' | \
  sed -e 's/\[//g' | \
  sed -e 's/\]//g' | \
#  sed -e 1's/,name,/,classification,category,source,method,target,option,type,measure,gas,unit,/g' \
  sed -e 1's/,name,/,classification,category,measure,gas,unit,source,method,target,option,type,/g' \
  > $ofil

grep ^party $ofil > ${direct}/$ofilsub
grep $submission $ofil >>${direct}/$ofilsub
rm $ofil
