:- module(setting, [nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    oi_option/1,
                    mydif/2
%                    highway/2
                    ]).


nonDetActions(nondet). % nondet/det
blocks_limit(100). % non/an integer > 3
discountfactor(0.9). %
convergence_threshold(0.0001). % residual for the VI algorithm to stop
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
%            highway(C1,C2),
            mydif(C1,C2), mydif(T,C1), mydif(T,C2).

transition(drive(T,C1), 2, 0.0,
        [tin(T,C2)],
        [tin(T,C2)]):-
%            highway(C1,C2),
            mydif(C1,C2), mydif(T,C1), mydif(T,C2).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).


%highway(brussels, paris).
%highway(brussels, rome).
%
%highway(paris, brussels).
%
%highway(rome, brussels).
% highway(rome, berlin).
% highway(berlin, rome).
% highway(brussels, berlin).
% highway(berlin, brussels).
% highway(moscow, berlin).
% highway(berlin, moscow).
% highway(moscow, brussels).
% highway(brussels, moscow).


% highway(paris, madrid).
% highway(madrid, paris).
% highway(berlin, prague).
% highway(prague, berlin).
% highway(brussels, gent).
% highway(gent, brussels).
% highway(brussels, leuven).
% highway(leuven, brussels).
% highway(paris, barcelona).
% highway(barcelona, paris).
% highway(barcelona, madrid).
% highway(madrid, barcelona).


% highway(leuven, rotterdam).
% highway(rotterdam, leuven).
% highway(rotterdam, amsterdam).
% highway(amsterdam, rotterdam).
% highway(copenhagen, amsterdam).
% highway(amsterdam, copenhagen).
% highway(amsterdam, brussels).
% highway(brussels, amsterdam).








%
% highway(stockholm, copenhagen).
% highway(copenhagen, stockholm).
% highway(madrid, lisbon).
% highway(lisbon, madrid).
% highway(rome, naples).
% highway(naples, rome).
% highway(moscow, helsinki).
% highway(helsinki, moscow).
