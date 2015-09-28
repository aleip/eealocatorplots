curfold=test
curfold=20150824

for a in ${curfold}/*\.pdf ; do
  . convertpdf2png.bash ${a}
done

test -e ${curfold}/png || mkdir ${curfold}/png
test -e ${curfold}/pdf || mkdir ${curfold}/pdf

mv ${curfold}/*\.pdf ${curfold}/pdf
mv ${curfold}/*\.png ${curfold}/png


