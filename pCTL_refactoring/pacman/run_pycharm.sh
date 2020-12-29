#!/bin/sh

swipl -s /home/wenchi/Desktop/pCTL-REBEL/pCTL_refactoring/pacman/properties.pl \
      -g 'query([], states([[pacman(6,3),ghost(0,6,3)], [pacman(5,3),ghost(0,5,3)] ]))' \
      -t halt

swipl -s /home/wenchi/Desktop/pCTL-REBEL/pCTL_refactoring/pacman/properties.pl \
      -g 'query([], states([[pacman(6,3),ghost(0,6,3)], [pacman(5,3),ghost(0,5,3)] ]))' \
      -t halt















