focus=3d2
focus=none
focus=3d
focus=faonir
focus=last
focus=all
includeoverview=1

if [ x${1}x != "xx" ] ; then focus=$1 ; fi

header=nir0header.Rmd
intro=nir0intro.Rmd
nir3over=nir0overview.Rmd
if [ $includeoverview == 1 ] ; then cat $header $intro $nir3over > nir0.Rmd ; else cp $header nir0.Rmd ; fi

nir3a="nir3aintro.Rmd nir3atrends.Rmd nir3aief.Rmd"
unc="nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd"

if [ $focus == "last" ]; then
	cat nir0.Rmd ${unc} nirfaocomparison.Rmd > nir${focus}out.Rmd
elif [ $focus == "faonir" ]; then
	cat nir0.Rmd nirfaocomparison.Rmd > nir${focus}out.Rmd
elif [ $focus == "all" ]; then
#	cat nir0.Rmd nir3a.Rmd nir3b1.Rmd nir3b2.Rmd nir3d.Rmd ${unc} nirfaocomparison.Rmd> nir${focus}out.Rmd
	cat nir0.Rmd nir3a.Rmd nir3b1.Rmd nir3b2.Rmd nir3d.Rmd  nir3uncertainty.Rmd nir3workshops.Rmd nir3verification.Rmd nirfaocomparison.Rmd> nir${focus}out.Rmd
elif [ $focus == "none" ]; then
	cat nir0.Rmd > nir/nir${focus}out.Rmd
else #focus: 3a,3b1,3b2,3d
	cat nir0.Rmd nir${focus}.Rmd > nir${focus}out.Rmd
fi

#. nirknit.bash nir${focus}out

# Generate markdown file
/cygdrive/c/Program\ Files/R/R-3.1.2/bin/Rscript -e "library(knitr);knit('nir${focus}out.Rmd')"

test -e nir${focus}out.Rmd && {
#. nirpandoc.bash nir${focus}out

# Attention! This kills all word documents!!
taskkill.exe /IM WINWORD.EXE

# Convert to word
/cygdrive/c/Program\ Files/RStudio/bin/pandoc/pandoc nir${focus}out.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output nir${focus}out.docx --highlight-style tango 

mv nir${focus}out* nir/

# Open word
test -e nir/nir${focus}out.docx && /cygdrive/c/Program\ Files\ \(x86\)/Microsoft\ Office/Office14/WINWORD.EXE nir/nir${focus}out.docx &

}