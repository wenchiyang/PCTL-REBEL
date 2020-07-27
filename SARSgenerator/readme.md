#### To get started, please see ../pCTL_refactoring/readme.md

## Getting started

This extends the pCTL-REBEL implementation (i.e. ../pCTL_refactoring/).

q(R,A,S,S') is the q-function where

- R is the reward
- A is the action
- S is current state
- S' is the next state

After running,
```console
$ python run.py
```

the abstract value function in the SARS form is in
- experiments/[tastname].txt (text format)
- visualization/[tastname].png (graphical format)

## Notes for developers
### Differences between SARSgenerator and pCTL_refactoring

- SARSgenerator abuses the partialQ/2 notation and extends it to partialQ/4
- OI states are forced to be used, regardless to the block number upper bound (i.e. **blocks_limit/1**). This can be turned off by setting **oi_option(flexible)**, which gives the same oi effect as pCTL_refactoring




###
