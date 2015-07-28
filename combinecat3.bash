today=$(date +%Y%m%d)
tomin=$(date +%Y%m%d-%H%M)
echo $today
echo $tomin

cattles="
3.A.1Cattle
3.A.1OptionA-Cattle
3.A.1OptionA-DairyCattle-
3.A.1OptionA-Non-DairyCattle-
3.A.1OptionB-Cattle_OptionB
3.A.1OptionB-GrowingCattle-
3.A.1OptionB-MatureDairyCattle-
3.A.1OptionB-OtherMatureCattle-
3.A.1OptionC-Cattle_OptionC
3.A.1OptionC-OtherCattle"

parameters="
AD-Population
CH4-EM
CH4-IEF
"
types="1VAL 2TRD 3CNT"

cd $today
for parameter in $parameters  ; do
	for type in $types ; do
		allcattles=""
		for cattle in $cattles  ; do
			
			test -e ${cattle}.${parameter}-${type}~${today}.pdf && allcattles="$allcattles ${cattle}.${parameter}-${type}~${today}.pdf"
			
		done
		if [ "x${allcattles}" != "x" ] ; then pdftk ${allcattles} cat output 3.A.1Cattle_${parameter}-${type}.pdf ; fi
	done
done

#allcattles=""
#for cattle in $cattles
#	do allcattles="$allcattles $(ls $cattle*Population*VAL*pdf)"
#done

cd ..

#rm $today/*~${today}.pdf
