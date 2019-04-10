focus=3d2
focus=none
focus=last
focus=faonir
includeoverview=1
focus=none
focus=3d
focus=all

# Annual things to do manually
#================================
# 1. Update the path to the plots for the sed-commands below (eugirpplots, ubaimages, cursubm, )
# 2. Update in section Quality checks 
#    -- Number of ERT recommendations
#    -- Number of notation key issues (or leave the numbers out)
# 3. Table 1.54 with issues (section quality check) (list, table, and text below table with date)
# 4. Once the word document is created:
#    - Open nir/niragritemplate.dotm
#    - 


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
	cat nir0.Rmd nir3a.Rmd nir3b1.Rmd nir3b2.Rmd nir3d.Rmd  nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd nircapricomparison.Rmd nirfaocomparison.Rmd > tmp0
elif [ $focus == "none" ]; then
	cat nir0.Rmd > tmp0
else #focus: 3a,3b1,3b2,3d
	cat nir0.Rmd nir${focus}.Rmd > tmp0
fi

#. nirknit.bash nir${focus}out

echo Now replace the placeholders at $HOSTNAME
echo Note that the sed command does not accept parameter, therefore any changes must be implemented here
sed -e 's/\$eugirpplots\$/..\/ecir\/plots/g' tmp0 > tmp1
sed -e 's/\$ubaimages\$/..\/ecir\/ubaimages/g' tmp1 > tmp0
sed -e 's/\$cursubm\$/20190315/g' tmp0 > tmp2

mv tmp2 nir${focus}out.Rmd


# Generate markdown file
if [ "$HOSTNAME" = "s-jrciprap246p" ]; then


    rexe="/c/Apps/R/R-3.5.0/bin/Rscript.exe"
    wexe="/c/Program\ Files/Microsoft\ Office/Office16/WINWORD.EXE"
    pexe="pandoc"
    
    /c/Apps/R/R-3.5.0/bin/Rscript.exe -e "library(knitr);knit('nir${focus}out.Rmd')"


elif [ "$HOSTNAME" = "l01ri1203587" ] ; then
	echo $HOSTNAME
    wexe="/c/Program\ Files/Microsoft\ Office/Office16/WINWORD.EXE"
    pexe="pandoc"
	rexe="/c/Program\ Files/R/R-3.4.4/bin/Rscript.exe"
    /c/Program\ Files/R/R-3.4.4/bin/Rscript.exe -e "library(knitr);knit('nir${focus}out.Rmd')"

elif [ "$HOSTNAME" = "d01ri1701864" ] ; then

    wexe="/c/Program\ Files/Microsoft\ Office/Office16/WINWORD.EXE"
    pexe="pandoc"
	rexe="/c/Program\ Files/R/R-3.4.3/bin/Rscript.exe"
    /c/Program\ Files/R/R-3.4.3/bin/x64/Rscript.exe -e "library('jpeg', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('png', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('RColorBrewer', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('compare', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('data.table', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('reshape2', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('png', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('dplyr', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('pander', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('captioner', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('ggplot2', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('evaluate', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('stringr', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4');library('knitr', lib.loc = 'C:/Users/rotllxa/Documents/R/win-library/3.4'); knit('nir${focus}out.Rmd')"

else

    rexe="/x/Program\ Files/R/R-3.3.1/bin/x64/Rscript.exe"
    rexe="/c/Program\ Files/R/R-3.4.0/bin/x64/Rscript.exe"
    #Attention to have the path to the Rscript.exe in the PATH
    rexe="Rscript.exe"
    wexe="/c/Program Files/Microsoft Office/Office16/WINWORD.EXE"
    pexe="pandoc"
	${rexe} -e "library(knitr);knit('nir${focus}out.Rmd')"

fi
echo Rscript executable $rexe
echo Word executable $wexe
echo Pandoc executable $pexe



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
