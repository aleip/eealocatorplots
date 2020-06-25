ifil=$1
ofil=eealocator_$(date +%Y%m%d).txt

# To be adapted: ifil, direct and submission

ifil=CRF_MMR_20160202.txt
ifil=CRF_MMR_20160322.txt
ifil=CRF_MMR_20160420.txt
ifil=CRF_MMR_20160810.txt
subyear=2018
subyear=2019
subyear=2020
submission=20170123
submission=20170317
submission=20170509
submission=20171011
submission=${subyear}0122
submission=${subyear}0319
submission=${subyear}0508
submission=${subyear}0115
submission=${subyear}0315
submission=${subyear}0508
submission=${subyear}0115
submission=${subyear}0315
submission=${subyear}0508

ifil=CRF_MMR_${submission}.txt
direct=../${subyear}/eealocator/

ifil=${direct}/${ifil}

ofilsub=eealocator_${submission}.txt
      #party   country_name    filename        generationtimestamp     variableUID     year    value   notation        sector_number   name    submission_version      submission_year
cut -d$'\t' -f1,2,3,5-12 $ifil | \
  sed -e 's/,/ /g' | \
  sed -e 's/\t/,/g' | \
  sed -e 's/\]\[/,/g' | \
  sed -e 's/\[//g' | \
  sed -e 's/\]//g' | \
#  sed -e 1's/,name,/,classification,category,source,method,target,option,type,measure,gas,unit,/g' \
  sed -e 1's/,name,/,classification,category,measure,gas,unit,source,method,target,option,type,/g' \
  > $ofil

echo 'grepping to $ofilsub'
grep ^party $ofil > ${direct}/$ofilsub
echo 'redirecting to ${direct}/$ofilsub '
grep $submission $ofil >>${direct}/$ofilsub
rm $ofil
