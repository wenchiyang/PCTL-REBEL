#### To get started, please see ../pCTL_refactoring/readme.md

This extends the pCTL-REBEL implementation (i.e. ../pCTL_refactoring/).

q(R,A,S,S') is the q-function where

- R is the reward
- A is the action
- S is current state
- S' is the next state

## Simulate all SARS tuples in a RMDP

Specify the transition function and reward function in simulate.pl

```console
$ python generateSARS.py
```


## Generating a partial plan from a value function

By running
```console
$ python generatePartialPlan.py
```

the abstract value function in the SARS form is in
- experiments/[tastname].txt (text format)
- experiments/[tastname].png (graphical format)

the ground "partial plan" is in
- experiments/ground.txt (text format)

Each line is of the form of vf(S, A, R, S') where
- S, A, S' are ground
- if the transition (S, A, S') is made, the expected reward in the long run is R (This is not the real reward but from a value function)


This has only be tested with a reachability property so far.


## Notes for developers
### Differences between SARSgenerator and pCTL_refactoring

- SARSgenerator abuses the partialQ/2 notation and extends it to partialQ/4
- OI states are forced to be used, regardless to the block number upper bound (i.e. **blocks_limit/1**). This can be turned off by setting **oi_option(flexible)**, which gives the same oi effect as pCTL_refactoring
-




###
