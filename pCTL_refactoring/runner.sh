#!/bin/bash

rm -r experiments/
mkdir experiments

swipl -g experiment1 -g halt properties.pl
# swipl -g experiment2 -g halt properties.pl
# swipl -g experiment5_outer1 -g halt properties.pl
# swipl -g ex -g halt properties.pl
