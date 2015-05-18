ifil=$1
ofil=eealocator_$(date +%Y%m%d).txt

submission=20150115
ofilsub=eealocator_${submission}_$(date +%Y%m%d).txt

cut -d$'\t' -f1,2,5-12 $ifil | \
  sed -e 's/\t/,/g' | \
  sed -e 's/\]\[/,/g' | \
  sed -e 's/\[//g' | \
  sed -e 's/\]//g' | \
  sed -e 1's/,name,/,classification,category,source,method,target,option,type,measure,gas,unit,/g' \
  > $ofil

grep ^party $ofil > $ofilsub
grep $submission $ofil >>$ofilsub
rm $ofil
