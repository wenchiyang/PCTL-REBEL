#!/bin/bash

#rm -r experiments/
mkdir experiments

#swipl -g experiment1 -g halt main_singlethread.pl
swipl -g experiment1 -g halt main_multithread.pl
#swipl -g experiment2 -g halt main_multithread.pl
#swipl -g experiment3 -g halt main_multithread.pl
#swipl -g experiment1 -g halt main_multithread.pl
