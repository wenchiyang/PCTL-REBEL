:- module(setting, [nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    oi_option/1,
                    mydif/2,
                    highway/2,
                    policy/2
                    ]).


nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1.0). %
convergence_threshold(0.01). % residual for the VI algorithm to stop
oi_option(flexible).

%%%%%%%%% MAP %%%%%%%%%%%%
highway(city0,city1).
%highway(city0,city2).
highway(city1,city0).
%highway(city1,city1).
%highway(city2,city0).
%highway(city2,city1).

%%%%%%%%% POLICY %%%%%%%%%%%%

% if parcel B is on truch T, then unload B from T
policy(State, unload(B,T)) :-
    member(on(B,T), State), !.

% else if: truck T is empty and is in city0, drive to city1
policy(State, drive(T,city1)) :-
    member(tin(T,city0), State),
    \+ member(on(_, T)), !.

% else if: truck T is empty and is in city1, drive to city0
policy(State, drive(T,city0)) :-
    member(tin(T,city1), State),
    \+ member(on(_, T)), !.

% else: choose a random action (this rule should never be activated if the policy is complete)
policy(_,_) :- !.

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


