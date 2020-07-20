#!/bin/bash

#rm -r experiments/
mkdir experiments

# swipl -g experiment2 -g halt properties.pl
swipl -g experiment1 -g halt properties.pl
