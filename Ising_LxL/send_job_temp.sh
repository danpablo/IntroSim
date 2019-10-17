#!/bin/bash
# Bash script for running njobs time the same physics and saving each run in a directory

change_temp()
{
awk  -v tt=$1 'NF!=3 {print $0} NF==3 {$1=tt;print $0}' input.dat > out
mv out input.dat

}

for temp in $(LANG=en_US.UTF-9 seq -f "%g" 4.00 -0.05 0.05)
do
    dir=${temp}_temp
    if [ ! -e $dir ] ; then
        mkdir $dir
    fi

    cp input.dat matrix_in.dat send_job.sh seed.dat $dir
    cd $dir
    change_temp $temp
    touch RUN_1

    ./send_job.sh # acá va el ejecutable

    cp matrix_in.dat ../matrix_in.dat
    cp seed.dat ../seed.dat

    cd ../
done
