:- module(setting, [nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    oi_option/1,
                    mydif/2,
                    reward/4,
                    constructAbsorbingVFs/2,
                    constructAbsorbingQs/2]).

% file_search_path(domain, 'blocksworld').

nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1.0). %
convergence_threshold(0.01). % residual for the VI algorithm to stop
oi_option(flexible). % flexible : create oi states when needed
                  % force : always create oi states


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


constructAbsorbingVFs(S, v(10.0, S)).
constructAbsorbingQs(S, q(10.0, _, S, S)).

% constructAbsorbingVFs([cl(c)], v(5.0, [cl(c)])).
% constructAbsorbingVFs([on(a,b)], v(10.0, [on(a,b)])).
% constructAbsorbingQs([cl(c)], q(5.0, _, [cl(c)], [cl(c)])).
% constructAbsorbingQs([on(a,b)], q(10.0, _, [on(a,b)], [on(a,b)])).

% !Important! The rewards are ordered. one transition is assigned
%             a reward from the first reward rule it matches
% reward(
%       action,
%       reward,
%       precondition,
%       postcondition
% )
% TODO: Do we need dif(A,a)?
% reward(_, 0,[on(a,b)],_).

% reward(_, 0,_,[cl(c)]).

% reward(move(A,B,C), 8,
%     [cl(A), cl(C), on(A,B), on(a,b)],
%     [cl(A), cl(B), on(A,C), on(a,b)]) :-
%         mydif(A,B), mydif(A,C), mydif(B,C).
%
% reward(move(A,B,C), 9,
%     [cl(A), cl(B), on(A,C), on(a,b)],
%     [cl(A), cl(B), on(A,C), on(a,b)]) :-
%         mydif(A,B), mydif(A,C), mydif(B,C).

% reward(_, 10,_, [on(a,b)]).

% reward(_, 5,
%     [cl(c)],
%     _).

reward(move(A,B,C), -2,
    [cl(A), cl(B), on(A,C)],
    [cl(A), cl(C), on(A,B)]) :-
        mydif(A,B), mydif(A,C), mydif(B,C).

reward(move(A,B,C), 2,
    [cl(A), cl(B), on(A,C)],
    [cl(A), cl(B), on(A,C)]) :-
        mydif(A,B), mydif(A,C), mydif(B,C).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).
