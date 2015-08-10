ifil=$1
ofil=eealocator_$(date +%Y%m%d).txt

ifil=CRF_MMR_20150807.txt
direct=../2015/eealocator/
submission=20150807

ifil=${direct}/${ifil}

ofilsub=eealocator_${submission}.txt

cut -d$'\t' -f1,2,5-12 $ifil | \
  sed -e 's/,/ /g' | \
  sed -e 's/\t/,/g' | \
  sed -e 's/\]\[/,/g' | \
  sed -e 's/\[//g' | \
  sed -e 's/\]//g' | \
  sed -e 1's/,name,/,classification,category,source,method,target,option,type,measure,gas,unit,/g' \
  > $ofil

grep ^party $ofil > ${direct}/$ofilsub
grep $submission $ofil >>${direct}/$ofilsub
rm $ofil
