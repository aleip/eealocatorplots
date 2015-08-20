# See http://stackoverflow.com/questions/6605006/convert-pdf-to-image-with-high-resolution
#
# Better quanlity apparently with pdftoppm
# See http://linux.die.net/man/1/pdftoppm
#     http://askubuntu.com/questions/50170/how-to-convert-pdf-to-image
#     ... but not (yet) installed in cygwin
# 
inputf=$1
qualitys=$2

density=300
quality=100
qual=h
if [ "x${qualitys}" == "x" ] ; then 
  density=150
  quality=100
  qual=l
fi
outputf=${inputf/.pdf/${density}.png}

echo $inputf

convert -density $density -trim $inputf -quality $quality $outputf
