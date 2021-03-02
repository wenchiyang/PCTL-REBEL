:- module(setting, [nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    oi_option/1,
                    mydif/2,
                    highway/2
                    ]).


nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1.0). %
convergence_threshold(0.01). % residual for the VI algorithm to stop
oi_option(flexible).

% transition(
%     action,
%     the ith consequece,
%     probability,
%     head_i,
%     body)
transition(unload(B,T), 1, 0.9,
        [bin(B,C), tin(T,C)],
        [tin(T,C), on(B,T)]):-
            mydif(T,C), mydif(T,B), mydif(C,B).

transition(unload(B,T), 2, 0.1,
        [tin(T,C), on(B,T)],
        [tin(T,C), on(B,T)]):-
            mydif(T,C), mydif(T,B), mydif(C,B).

transition(load(B,T), 1, 0.9,
        [tin(T,C), on(B,T)],
        [bin(B,C), tin(T,C)]):-
            mydif(T,C), mydif(T,B), mydif(C,B).

transition(load(B,T), 2, 0.1,
        [bin(B,C), tin(T,C)],
        [bin(B,C), tin(T,C)]):-
            mydif(T,C), mydif(T,B), mydif(C,B).

transition(drive(T,C1), 1, 1.0,
        [tin(T,C1)],
        [tin(T,C2)]):-
            highway(C1,C2),
            mydif(C1,C2), mydif(T,C1), mydif(T,C2).

transition(drive(T,C1), 2, 0.0,
        [tin(T,C2)],
        [tin(T,C2)]):-
            highway(C1,C2),
            mydif(C1,C2), mydif(T,C1), mydif(T,C2).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

highway(city0,city4).
highway(city0,city3).
highway(city0,city2).
highway(city0,city5).
highway(city0,city7).
highway(city1,city6).
highway(city1,city3).
highway(city1,city4).
highway(city2,city0).
highway(city2,city7).
highway(city2,city5).
highway(city2,city3).
highway(city2,city4).
highway(city3,city0).
highway(city3,city1).
highway(city3,city4).
highway(city3,city2).
highway(city3,city6).
highway(city4,city0).
highway(city4,city1).
highway(city4,city3).
highway(city4,city2).
highway(city4,city6).
highway(city5,city2).
highway(city5,city7).
highway(city5,city0).
highway(city6,city1).
highway(city6,city4).
highway(city6,city3).
highway(city7,city2).
highway(city7,city5).
highway(city7,city0).

