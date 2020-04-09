#!/bin/bash

# This file runs experiments for blocks world with non-determinisitc actions
# timeout 30 minutes = 1800 seconds
# how to execute this script?
# sh runner.sh {Det/NonDet} {foldername}, e.g. sh runner.sh Det Feb6/

det=$1
expfolder=$2
resultfolder=$expfolder$det"Results/"
#rm -r $resultfolder
mkdir -p $resultfolder

for engine in  "expl" #"sparse" #"expl" #"sparse" "dd"  "hybrid"  "abs"
do

    for n_block in 10
    do
        echo "Experiments: "$n_block" blocks with engine "$engine

        MClogfile=$resultfolder$det$engine"BW"$n_block".log"
        memorylogfile=$resultfolder"mem"$det$engine"BW"$n_block".log"

        #sh monitor.sh $memorylogfile &

        storm --prism ../generatedmodels/"$det"BW"$n_block".nm \
            -prop "Pmax=? [F \"goal\"]" \
            --engine $engine
            > $MClogfile

        #pid=$(pgrep -f "sh monitor.sh")
        #kill -9 $pid
    done


done
