#!/bin/bash

#PBS -q tqueue
#PBS -N UNDEF_REPLACE
#PBS -j oe
#PBS -l nodes=1:ppn=1

VAR="VDFHR"
NOW=$(date "+%Y%m%d_%H%M%S")
RESULT="../output/result_${VAR}_${NOW}.txt"

NAMELIST="../nml/input_${VAR}.nml"

ulimit -s unlimited

cd /mnt/jet11/kosei/JRA/undef_filter/src

./EXE < ${NAMELIST} > ${RESULT} 2>&1

