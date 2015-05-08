# See http://stackoverflow.com/questions/6605006/convert-pdf-to-image-with-high-resolution
#
# Better quanlity apparently with pdftoppm
# See http://linux.die.net/man/1/pdftoppm
#     http://askubuntu.com/questions/50170/how-to-convert-pdf-to-image
#     ... but not (yet) installed in cygwin
# 
density=300
quality=100

inputf=$1
outputf=${inputf/.pdf/${density}.png}

echo $inputf

convert -density $density -trim $inputf -quality $quality $outputf
