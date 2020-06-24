:- module(setting, [nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    mydif/2]).

nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1). %
convergence_threshold(0.01). % residual for the VI algorithm to stop


% transition(
%     action,
%     the ith consequece,
%     probability,
%     head_i,
%     body)
transition(move(X,Y,Z), 1, 0.9,
        [cl(X), cl(Z), on(X,Y)],
        [cl(X), cl(Y), on(X,Z)]):-
            mydif(X,Y), mydif(Y, Z), mydif(X,Z).
transition(move(X,Y,Z), 2, 0.1,
        [cl(X), cl(Y), on(X,Z)],
        [cl(X), cl(Y), on(X,Z)]):-
            mydif(X,Y), mydif(Y, Z), mydif(X,Z).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).
