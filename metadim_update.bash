files="row1 row2 row3 row4 row5 row6option row7type row8measure row9gas row10unit"

for file in $files ; do 
oldd=20150322
newd=20150323

old=metadim_${file}~${oldd}.txt
oldup=metadim_${file}_upd~${oldd}.txt
new=metadim_${file}~${newd}.txt

#First eliminate acronyms for diff
olda=new.txt
newa=old.txt

cut -d, -f1,2 $old > $olda
cut -d, -f1,2 $new > $newa

diff $olda $newa > diff.txt


# Add new rows to old file and save as updated
cp $old $oldup
grep "^>" diff.txt | sed -e 's/> \(.*\)$/\1,/g' >> $oldup

done

