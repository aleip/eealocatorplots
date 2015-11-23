includeoverview=1
focus=3d2
focus=3a
focus=none
focus=all

if [ x${1}x != "xx" ] ; then focus=$1 ; fi

header=nir0header.Rmd
intro=nir0intro.Rmd
nir3over=nir0overview.Rmd
if [ $includeoverview == 1 ] ; then cat $header $intro $nir3over > nir0.Rmd ; else cp $header nir0.Rmd ; fi


nir3a="nir3aintro.Rmd nir3atrends.Rmd nir3aief.Rmd"
nir3b1="nir3b1.Rmd nir3b1trends.Rmd nir3b1ief.Rmd"
nir3b2="nir3b2.Rmd nir3b2trends.Rmd nir3b2ief.Rmd"
nir3d1="nir3d1.Rmd nir3d1trends.Rmd nir3d1ief.Rmd"
nir3d2="nir3d2.Rmd nir3d2trends.Rmd nir3d2ief.Rmd"
unc="nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd"

if [ $focus == "3a" ]; then
	cat nir0.Rmd ${nir3a} > nirfull.Rmd
elif [ $focus == "3b1" ]; then
	cat nir0.Rmd ${nir3b1} > nirfull.Rmd
elif [ $focus == "3b2" ]; then
	cat nir0.Rmd ${nir3b2} > nirfull.Rmd
elif [ $focus == "3d" ]; then
	cat nir0.Rmd nir3d1.Rmd nir3d2.Rmd > nirfull.Rmd
elif [ $focus == "3d1" ]; then
	cat nir0.Rmd ${nir3d1} > nirfull.Rmd
elif [ $focus == "3d2" ]; then
	cat nir0.Rmd ${nir3d2} > nirfull.Rmd
elif [ $focus == "ief" ]; then
	cat nir0.Rmd nir3aief.Rmd nir3b1ief.Rmd nir3b2ief.Rmd nir3d1ief.Rmd nir3d2ief.Rmd > nirfull.Rmd
elif [ $focus == "last" ]; then
	cat nir0.Rmd ${unc} > nirfull.Rmd
elif [ $focus == "all" ]; then
	cat nir0.Rmd ${nir3a} ${nir3b1} ${nir3b2} ${nir3d1} ${nir3d2} ${unc} > nir${focus}.Rmd
elif [ $focus == "none" ]; then
	cat nir0.Rmd > nir${focus}.Rmd
fi

. nirknit.bash nir${focus}

