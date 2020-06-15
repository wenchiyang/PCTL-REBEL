#!/bin/bash

#rm -r experiments/
mkdir experiments

swipl -g ex -g halt properties.pl
