:- module(sorting, [sortByQValue/2, list_to_set1/2,
                    subsumesort/2, predInList/3, stateGroup/2,
                    termsInState/2]).



% This is for a newer version of swi prolog
predInList(OldState, Type, NewState):-
    include([X]>>functor(X, Type, _), OldState, NewState), !.

% This is assumed to be sorted by relationsort/2
relations([bin, tin, on]).

%
stateGroup(State, GroupedState):-
    relations(Relations),
    maplist(predInList(State), Relations, GroupedState).

% termsInState([], []) :- !.
% termsInState([cl(X)|S], [X|B]):-
%     termsInState(S, B), !.
% termsInState([on(X,Y)|S], [X,Y|B]):-
%     termsInState(S, B), !.

termsInState(State, Terms):-
    maplist([Pred, Args]>> =..(Pred,[_|Args]), State, ArgsList),
    flatten(ArgsList, TermsList),
    list_to_set(TermsList, Terms).

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
relationcompare(<, bin, _) :- !.
relationcompare(>, _, bin) :- !.
relationcompare(<, _, on) :- !.
relationcompare(>, on, _) :- !.

% load, unload, drive
relationcompare(<, load, _) :- !.
relationcompare(>, _, load) :- !.
relationcompare(<, drive, _) :- !.
relationcompare(>, _, drive) :- !.


%%%%%
% rcompare: relation compare : cl/1 and on/2
%         : used to sort a state (list of relations)
%%%%%
rcompare(=, A, B):- A == B, !.
rcompare(<, R1, R2) :-
    functor(R1, F1, _),
    functor(R2, F2, _),
    relationcompare(<, F1, F2), !.

rcompare(>, R1, R2) :-
    functor(R1, F1, _),
    functor(R2, F2, _),
    relationcompare(>, F1, F2), !.

% more ground vars before less ground vars
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, Vars1), term_variables(R2, Vars2),
    length(Vars1, NAB), length(Vars2, NCD),
    NAB < NCD, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, Vars1), term_variables(R2, Vars2),
    length(Vars1, NAB), length(Vars2, NCD),
    NAB > NCD, !.
% both with one ground var:
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    ground(A), \+ground(B), \+ground(C), ground(D), !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    \+ground(A), ground(B), ground(C), \+ground(D), !.


rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _), % TODO : can be removed
    term_variables(R1, []), term_variables(R2, []),
    arg(1, R1, A), arg(1, R2, C),
    A @< C, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, []), term_variables(R2, []),
    arg(1, R1, A), arg(1, R2, C),
    A @> C, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, []), term_variables(R2, []),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    A == C, B @< D, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, []), term_variables(R2, []),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    A == C, B @> D, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, [A,_]), term_variables(R2, [C,_]),
    A @< C, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, [A,_]), term_variables(R2, [C,_]),
    A @> C, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, [A,B]), term_variables(R2, [C,D]),
    A == C, B @< D, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    term_variables(R1, [A,B]), term_variables(R2, [C,D]),
    A == C, B @> D, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    ground(A), \+ground(B), ground(C), \+ground(D),
    A @< C, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    ground(A), \+ground(B), ground(C), \+ground(D),
    A @> C, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    ground(A), \+ground(B), ground(C), \+ground(D),
    A == C, B @< D, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    ground(A), \+ground(B), ground(C), \+ground(D),
    A == C, B @> D, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    \+ground(A), ground(B), \+ground(C), ground(D),
    B @< D, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    \+ground(A), ground(B), \+ground(C), ground(D),
    B @> D, !.
rcompare(<, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    \+ground(A), ground(B), \+ground(C), ground(D),
    B == D, A @< C, !.
rcompare(>, R1, R2) :-
    functor(R1, F, _), functor(R2, F, _),
    arg(1, R1, A), arg(2, R1, B), arg(1, R2, C), arg(2, R2, D),
    \+ground(A), ground(B), \+ground(C), ground(D),
    B == D, A @> C, !.



%%%%%
% scompare: state compare : state1 and state2 are ORDERED lists
%%%%%
scompare(=, S1, S2) :- S1 == S2, !.
scompare(<, S1, S2) :-
    length(S1, LS1), length(S2, LS2), LS1 < LS2, !.
scompare(>, S1, S2) :-
    length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
scompare(<, S1, S2) :-
    length(S1, L), length(S2, L),
    term_variables(S1, Vars1), term_variables(S2, Vars2),
    length(Vars1, GL1), length(Vars2, GL2), GL1 < GL2, !.
scompare(>, S1, S2) :-
    length(S1, L), length(S2, L),
    term_variables(S1, Vars1), term_variables(S2, Vars2),
    length(Vars1, GL1), length(Vars2, GL2), GL1 > GL2, !.
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
acompare(=, Act1, Act2):-
    Act1 == Act2, !.
acompare(<, Act1, Act2):-
    functor(Act1, F1, _),
    functor(Act2, F2, _),
    relationcompare(<, F1, F2), !.
acompare(>, Act1, Act2) :-
    functor(Act1, F1, _),
    functor(Act2, F2, _),
    relationcompare(>, F1, F2), !.

% more ground terms before less ground terms
acompare(<, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L1), length(Vars2, L2),
    L1 < L2, !.
acompare(>, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L1), length(Vars2, L2),
    L1 > L2, !.

acompare(<, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    A1 @< A2, !.
acompare(>, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    A1 @> A2, !.
acompare(<, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    arg(2, Act1, B1), arg(2, Act2, B2),
    A1 == A2, B1 @< B2, !.
acompare(>, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    arg(2, Act1, B1), arg(2, Act2, B2),
    A1 == A2, B1 @> B2, !.
acompare(<, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    arg(2, Act1, B1), arg(2, Act2, B2),
    arg(3, Act1, C1), arg(3, Act2, C2),
    A1 == A2, B1 == B2, C1 @< C2, !.
acompare(>, Act1, Act2):-
    functor(Act1, F, _),functor(Act2, F, _),
    term_variables(Act1, Vars1),
    term_variables(Act2, Vars2),
    length(Vars1, L), length(Vars2, L),
    arg(1, Act1, A1), arg(1, Act2, A2),
    arg(2, Act1, B1), arg(2, Act2, B2),
    arg(3, Act1, C1), arg(3, Act2, C2),
    A1 == A2, B1 == B2, C1 @> C2, !.

%%%
%
%%%

qcompare(=, QRule1, QRule2):- QRule1 == QRule2, !.
qcompare(<, q(Q1,_,_), q(Q2,_,_)):- Q1 > Q2, !.
qcompare(>, q(Q1,_,_), q(Q2,_,_)):- Q1 < Q2, !.
qcompare(<, q(Q1,_,S1), q(Q2,_,S2)):-
    Q1 == Q2, scompare(<, S1, S2), !.
qcompare(>, q(Q1,_,S1), q(Q2,_,S2)):-
    Q1 == Q2, scompare(>, S1, S2), !.
qcompare(<, q(Q1,A1,S1), q(Q2,A2,S2)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(<, A1, A2), !.
qcompare(<, q(Q1,A1,S1), q(Q2,A2,S2)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(=, A1, A2), !.
qcompare(>, q(Q1,A1,S1), q(Q2,A2,S2)):-
    Q1 == Q2, scompare(=, S1, S2), acompare(>, A1, A2), !.

qcompare(<, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 > Q2, !.
qcompare(>, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 < Q2, !.
qcompare(<, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
    scompare(<, S1, S2), !.
qcompare(>, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
    scompare(>, S1, S2), !.
qcompare(<, partialQ(Q,A1,S1), partialQ(Q,A2,S2)):-
    scompare(=, S1, S2),
    acompare(<, A1, A2), !.
qcompare(<, partialQ(Q,A1,S1), partialQ(Q,A2,S2)):-
    scompare(=, S1, S2),
    acompare(=, A1, A2), !.
qcompare(>, partialQ(Q,A1,S1), partialQ(Q,A2,S2)):-
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
