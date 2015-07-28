today=$(date +%Y%m%d)
tomin=$(date +%Y%m%d-%H%M)
echo $today
echo $tomin

cats="1.B 2.A 2.B 2.C 2.D 2.E 3.A 3.B 3.C 3.D 3.E 3.F 4.A 4.B 4.C 4.D 4.E 4.F"
for  cat in $cats 
	do lsdo=$(ls ${today}/${cat}*${today}.pdf 2>lserr)
	lsok=$?
	#echo $cat ${lsok}
	pdftk $today/${cat}*~${today}.pdf cat output $today/${cat}_plots~${tomin}.pdf 2> eea_err

done

mkdir $today/$tomin
for sec in 1 2 3 4; do
	mkdir $today/$tomin/$sec
	mv $today/${sec}*plot*pdf $today/$tomin 2>eea_err
	mv $today/${sec}*pdf $today/$tomin/$sec 2>eea_err
done


#rm $today/*~${today}.pdf
