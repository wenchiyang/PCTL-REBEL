:- module(sorting, [sortByQValue/2, list_to_set1/2,
                    subsumesort/2, predInList/3, stateGroup/2]).



% This is for a newer version of swi prolog
predInList(OldState, Type, NewState):-
    include([X]>>functor(X, Type, _), OldState, NewState), !.

% This is assumed to be sorted by relationsort/2
relations([pacman,ghost]).

%
stateGroup(State, GroupedState):-
    relations(Relations),
    maplist(predInList(State), Relations, GroupedState).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
relationsort(Relations, OrderedRelations):-
    predsort(relationcompare, Relations, OrderedRelations).

sortByQValue(List, Sorted):-
    predsort(qcompare, List, Sorted), !.

subsumesort(List, Sorted):-
    predsort(rcompare, List, Sorted), !.

%%%
% rcompare
%%%
relationcompare(=, A, B) :- A == B, !.


%%%%%
% rcompare: relation compare : cl/1 and on/2
%         : used to sort a state (list of relations)
%%%%%
% cl/1 before on/2
rcompare(=, A, B):- A == B, !.

rcompare(<, pacman(_,_), ghost(_,_,_)) :- !.
rcompare(>, ghost(_,_,_), pacman(_,_)) :- !.

% compare first variable, then the second one
rcompare(<, pacman(A,_), pacman(C,_)) :-
%    writeln([pacman(A,B), pacman(C,D)]),
%    term_variables(pacman(A,B), []), term_variables(pacman(C,D), []),
    A @< C, !.
rcompare(>, pacman(A,_), pacman(C,_)) :-
%    term_variables(pacman(A,B), []), term_variables(pacman(C,D), []),
    A @> C, !.
rcompare(<, pacman(A,B), pacman(C,D)) :-
%    term_variables(pacman(A,B), []), term_variables(pacman(C,D), []),
    A == C, B @< D, !.
rcompare(>, pacman(A,B), pacman(C,D)) :-
%    term_variables(pacman(A,B), []), term_variables(pacman(C,D), []),
    A == C, B @> D, !.
%
rcompare(<, ghost(ID1,_,_), ghost(ID2,_,_)) :-
    ID1 @< ID2,
    !.
rcompare(>, ghost(ID1,_,_), ghost(ID2,_,_)) :-
    ID1 @> ID2,
    !.
rcompare(<, ghost(ID1,A,_), ghost(ID2,C,_)) :-
    ID1 == ID2, A @< C,
    !.
rcompare(>, ghost(ID1,A,_), ghost(ID2,C,_)) :-
    ID1 == ID2, A @> C,
    !.
rcompare(<, ghost(ID1,A,B), ghost(ID2,C,D)) :-
    ID1 == ID2, A == C, B @< D,
    !.
rcompare(>, ghost(ID1,A,B), ghost(ID2,C,D)) :-
    ID1 == ID2, A == C, B @> D,
    !.


%%%%%
% scompare: state compare : state1 and state2 are ORDERED lists
%%%%%
scompare(=, S1, S2) :- S1 == S2, !.

scompare(<, S1, S2) :-
    length(S1, L), length(S2, L),
    term_variables(S1, Vars1), term_variables(S2, Vars2),
    length(Vars1, GL), length(Vars2, GL),
    S1 = [E1|_], S2 = [E2|_],
    rcompare(<, E1, E2), !.
scompare(>, S1, S2) :-
    length(S1, L), length(S2, L),
    term_variables(S1, Vars1), term_variables(S2, Vars2),
    length(Vars1, GL), length(Vars2, GL),
    S1 = [E1|_], S2 = [E2|_],
    rcompare(>, E1, E2), !.
scompare(R, S1, S2) :-
    S1 \== S2, length(S1, L), length(S2, L),
    term_variables(S1, Vars1), term_variables(S2, Vars2),
    length(Vars1, GL), length(Vars2, GL),
    S1 = [E1|RS1], S2 = [E2|RS2],
    rcompare(=, E1, E2),
    scompare(R, RS1, RS2), !.


% %%%%
% acompare: action compare :
% %%%%
acompare(=, A1, A2):-
    A1 == A2, !.
% compare one by one
acompare(<, pacman_move(north), pacman_move(A)):-A \= north.
acompare(>, pacman_move(A), pacman_move(north)):-A \= north.
acompare(<, pacman_move(south), pacman_move(west)).
acompare(>, pacman_move(west), pacman_move(south)).
acompare(<, pacman_move(south), pacman_move(east)).
acompare(>, pacman_move(east), pacman_move(south)).
acompare(<, pacman_move(east), pacman_move(west)).
acompare(>, pacman_move(west), pacman_move(east)).

%%%
%
%%%

%
qcompare(=, QRule1, QRule2):- QRule1 == QRule2, !.
qcompare(<, q(Q1,_,_,_), q(Q2,_,_,_)):- Q1 > Q2, !.
qcompare(>, q(Q1,_,_,_), q(Q2,_,_,_)):- Q1 < Q2, !.
qcompare(<, q(Q1,_,S1,_), q(Q2,_,S2,_)):-
    Q1 == Q2, scompare(<, S1, S2), !.
qcompare(>, q(Q1,_,S1,_), q(Q2,_,S2,_)):-
    Q1 == Q2, scompare(>, S1, S2), !.
qcompare(<, q(Q1,A1,S1,_), q(Q2,A2,S2,_)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(<, A1, A2), !.
qcompare(<, q(Q1,A1,S1,_), q(Q2,A2,S2,_)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(=, A1, A2), !.
qcompare(>, q(Q1,A1,S1,_), q(Q2,A2,S2,_)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(>, A1, A2), !.

qcompare(<, partialQ(Q1,_,_,_), partialQ(Q2,_,_,_)):- Q1 > Q2, !.
qcompare(>, partialQ(Q1,_,_,_), partialQ(Q2,_,_,_)):- Q1 < Q2, !.
qcompare(<, partialQ(Q,_,S1,_), partialQ(Q,_,S2,_)):-
    scompare(<, S1, S2), !.
qcompare(>, partialQ(Q,_,S1,_), partialQ(Q,_,S2,_)):-
    scompare(>, S1, S2), !.
qcompare(<, partialQ(Q,A1,S1,_), partialQ(Q,A2,S2,_)):-
    scompare(=, S1, S2),
    acompare(<, A1, A2), !.
qcompare(<, partialQ(Q,A1,S1,_), partialQ(Q,A2,S2,_)):-
    scompare(=, S1, S2),
    acompare(=, A1, A2), !.
qcompare(>, partialQ(Q,A1,S1,_), partialQ(Q,A2,S2,_)):-
    scompare(=, S1, S2),
    acompare(>, A1, A2), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list_to_set1(L1, L2): L2 is a set of a list L1
% after trying to bind elements
list_to_set1([], []) :- !.
list_to_set1([V|T0], [V|T]) :-
    remove_same_key(T0, V, T1),
    list_to_set1(T1, T), !.

% remove unifiable partialQ and q rules
remove_same_key([], _, []) :- !.
remove_same_key([V1|T0], V, T) :-
    V1 = V, % binding
    remove_same_key(T0, V, T), !.

remove_same_key([V1|T0], V, [V1|T]) :-
    remove_same_key(T0, V, T).
