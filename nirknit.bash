
# Generate markdown file
/cygdrive/c/Program\ Files/R/R-3.1.2/bin/Rscript -e "library(knitr);knit('${1}.Rmd')"

test -e ${1}.Rmd && . nirpandoc.bash ${1}

