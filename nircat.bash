focus=3d2
focus=none
focus=last
focus=3a
focus=faonir
focus=all
includeoverview=1
focus=none

if [ x${1}x != "xx" ] ; then focus=$1 ; fi
echo $focus
header=nir0header.Rmd
intro=nir0intro.Rmd
nir3over=nir0overview.Rmd
if [ $includeoverview == 1 ] ; then cat $header $intro $nir3over > nir0.Rmd ; else cp $header nir0.Rmd ; fi

nir3a="nir3aintro.Rmd nir3atrends.Rmd nir3aief.Rmd"
unc="nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd"

if [ $focus == "last" ]; then
	cat nir0.Rmd ${unc} nirfaocomparison.Rmd > tmp0
elif [ $focus == "faonir" ]; then
	cat nir0.Rmd nirfaocomparison.Rmd > tmp0
elif [ $focus == "all" ]; then
#	cat nir0.Rmd nir3a.Rmd nir3b1.Rmd nir3b2.Rmd nir3d.Rmd ${unc} nirfaocomparison.Rmd> tmp0
	cat nir0.Rmd nir3a.Rmd nir3b1.Rmd nir3b2.Rmd nir3d.Rmd  nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd nirfaocomparison.Rmd> tmp0
elif [ $focus == "none" ]; then
	cat nir0.Rmd > tmp0
else #focus: 3a,3b1,3b2,3d
	cat nir0.Rmd nir${focus}.Rmd > tmp0
fi

#. nirknit.bash nir${focus}out



# Generate markdown file
if [ "$HOSTNAME" = "marsbl1bhl" ]; then
    echo Now replace the placeholders at $HOSTNAME
    sed -e 's/\$eugirpplots\$/..\/ecir\/plots/g' tmp0 > tmp1
    sed -e 's/\$cursubm\$/20180319/g' tmp1 > tmp2

    mv tmp2 nir${focus}out.Rmd


    rexe="/c/Program\ Files/R/R-3.4.1/bin/Rscript.exe"
    wexe="/c/Program\ Files/Microsoft\ Office/Office16/WINWORD.EXE"
    pexe="pandoc"
    
    /c/Program\ Files/R/R-3.4.1/bin/Rscript.exe -e "library(knitr);knit('nir${focus}out.Rmd')"

    

else
    # Now replace the placeholders
    sed -e 's/\$eugirpplots\$/..\/..\/..\/..\/..\/google\/projects\/ecir\/plots/g' tmp0 > tmp1
    sed -e 's/\$cursubm\$/20170317/g' tmp1 > tmp2

    mv tmp2 nir${focus}out.Rmd

    rexe="/x/Program\ Files/R/R-3.3.1/bin/x64/Rscript.exe"
    rexe="/c/Program\ Files/R/R-3.4.0/bin/x64/Rscript.exe"
    #Attention to have the path to the Rscript.exe in the PATH
    rexe="Rscript.exe"
    wexe="/c/Program Files/Microsoft Office/Office16/WINWORD.EXE"
    pexe="pandoc"
fi
echo Rscript executable $rexe
echo Word executable $wexe
echo Pandoc executable $pexe


#${rexe} -e "library(knitr);knit('nir${focus}out.Rmd')"

test -e nir${focus}out.Rmd && {
	#. nirpandoc.bash nir${focus}out

	# Attention! This kills all word documents!!
	test "x$(tasklist | grep WINWORD)" == "x" || taskkill.exe -IM WINWORD.EXE

	# Convert to word
	#/cygdrive/c/Program\ Files/RStudio/bin/pandoc/pandoc nir${focus}out.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output nir${focus}out.docx --highlight-style tango 
	#${pexe} nir${focus}out.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output nir${focus}out.docx --highlight-style tango
	
	/c/Program\ Files/RStudio/bin/pandoc/pandoc nir${focus}out.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output nir${focus}out.docx --highlight-style tango 

	mv nir${focus}out* nir/
  mv nir/nir${focus}out.docx nir/nir${focus}out~$(date +%Y%m%d).docx

	# Open word
  test -e nir/nir${focus}out~$(date +%Y%m%d).docx && /c/Program\ Files/Microsoft\ Office/Office16/WINWORD.EXE nir/nir${focus}out~$(date +%Y%m%d).docx &

}
