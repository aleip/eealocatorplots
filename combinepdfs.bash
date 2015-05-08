today=$(date +%Y%m%d)
tomin=$(date +%Y%m%d-%H%M)
echo $today
echo $tomin
pdftk *_*~${today}.pdf cat output ghgplots~${tomin}.pdf
rm *_*~${today}.pdf
