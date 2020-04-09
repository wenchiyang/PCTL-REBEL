:- module(setting, [nonDetActions/1, blocks_limit/1, discountfactor/1, convergence_threshold/1]).

nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1). %
convergence_threshold(0.0001). % residual for the VI algorithm to stop
