chapter=nir3d

sed -e 's/..\/..\/..\/..\/..\/google\/projects\/ecir\/plots/$eugirpplots$/g' $chapter.Rmd > test
mv test $chapter.Rmd

sed -e 's/20170317/$cursubm$/g' $chapter.Rmd > test
mv test $chapter.Rmd


sed -e 's/..\/2016\/eureport/$ubaimages$/g' $chapter.Rmd > test
mv test $chapter.Rmd

