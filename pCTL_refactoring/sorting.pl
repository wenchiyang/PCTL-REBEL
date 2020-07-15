:- module(sorting, [sortByQValue/2, list_to_set1/2,
                    subsumesort/2, rcompare/3, scompare/3, acompare/3,
                    qcompare/3, predInList/3]).


% This is for a newer version of swi prolog
predInList(OldState, Type, NewState):-
    include([X]>>functor(X, Type, _), OldState, NewState), !.

subsumesort(List, Sorted):-
    predsort(rcompare, List, Sorted), !.

count_ground([] , 0):- !.
count_ground([E|R], Num):-
    ground(E),
    count_ground(R, RNum),
    Num is RNum+1, !.

count_ground([E|R], Num):-
    \+ground(E),
    count_ground(R, Num), !.


%%%%%
% rcompare: relation compare : cl/1 and on/2
%         : used to sort a state (list of relations)
%%%%%
% cl/1 before on/2
rcompare(<, cl(_), on(_,_)) :- !.
rcompare(>, on(_,_), cl(_)) :- !.
% ground before unground
rcompare(<, cl(A), cl(B)) :-
    ground(A), \+ground(B), !.
rcompare(>, cl(A), cl(B)) :-
    \+ground(A), ground(B), !.
% both are ground: compare both constant
rcompare(<, cl(A), cl(B)) :-
    ground(A), ground(B), compare(<, A, B), !.
rcompare(>, cl(A), cl(B)) :-
    ground(A), ground(B), compare(>, A, B), !.
% both are nonground: compare both constant
rcompare(<, cl(A), cl(B)) :-
    \+ground(A), \+ground(B), compare(<, A, B), !.
rcompare(>, cl(A), cl(B)) :-
    \+ground(A), \+ground(B), compare(>, A, B), !.
rcompare(=, cl(A), cl(B)) :-
    compare(=, A, B), !.

% more ground vars before less ground vars
rcompare(<, on(A,B), on(C,D)) :-
    term_variables(on(A,B), Vars1), term_variables(on(C,D), Vars2),
    length(Vars1, NAB), length(Vars2, NCD),
    % count_ground([A,B], NAB), count_ground([C,D], NCD),
    NAB > NCD, !.
rcompare(>, on(A,B), on(C,D)) :-
    term_variables(on(A,B), Vars1), term_variables(on(C,D), Vars2),
    length(Vars1, NAB), length(Vars2, NCD),
    % count_ground([A,B], NAB), count_ground([C,D], NCD),
    NAB < NCD, !.
% both with one ground var:
rcompare(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), \+ground(C), ground(D), !.
rcompare(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), ground(C), \+ground(D), !.


rcompare(<, on(A,B), on(C,D)) :-
    count_ground([A,B], 2), count_ground([C,D], 2), compare(<, A, C), !.
rcompare(>, on(A,B), on(C,D)) :-
    count_ground([A,B], 2), count_ground([C,D], 2), compare(>, A, C), !.
rcompare(<, on(A,B), on(C,D)) :-
    count_ground([A,B], 2), count_ground([C,D], 2), compare(=, A, C), compare(<, B, D),!.
rcompare(>, on(A,B), on(C,D)) :-
    count_ground([A,B], 2), count_ground([C,D], 2), compare(=, A, C), compare(>, B, D),!.

rcompare(<, on(A,B), on(C,D)) :-
    count_ground([A,B], 0), count_ground([C,D], 0), compare(<, A, C), !.
rcompare(>, on(A,B), on(C,D)) :-
    count_ground([A,B], 0), count_ground([C,D], 0), compare(>, A, C), !.
rcompare(<, on(A,B), on(C,D)) :-
    count_ground([A,B], 0), count_ground([C,D], 0), compare(=, A, C), compare(<, B, D),!.
rcompare(>, on(A,B), on(C,D)) :-
    count_ground([A,B], 0), count_ground([C,D], 0), compare(=, A, C), compare(>, B, D),!.

rcompare(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(<, A, C), !.
rcompare(>, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(>, A, C), !.
rcompare(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(=, A, C), compare(<, B, D),!.
rcompare(>, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(=, A, C), compare(>, B, D),!.

rcompare(<, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(<, B, D), !.
rcompare(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(>, B, D), !.
rcompare(<, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(=, B, D), compare(<, A, C),!.
rcompare(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(=, B, D), compare(>, A, C),!.

rcompare(=, on(A,B), on(C,D)) :- compare(=, A, C), compare(=, B, D), !.





%%%%%
% scompare: state compare : state1 and state2 are lists
%%%%%
scompare(=, S1, S2) :- S1 == S2, !.
scompare(<, S1, S2) :-
    length(S1, LS1), length(S2, LS2), LS1 < LS2, !.
scompare(>, S1, S2) :-
    length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
scompare(<, S1, S2) :-
    length(S1, L), length(S2, L),
    count_ground(S1, GL1), count_ground(S2, GL2),
    GL1 > GL2, !.
scompare(<, S1, S2) :-
    S1 \== S2,
    length(S1, L), length(S2, L),
    count_ground(S1, GL), count_ground(S2, GL), !.
scompare(>, S1, S2) :-
    length(S1, L), length(S2, L),
    count_ground(S1, GL1), count_ground(S2, GL2),
    GL1 < GL2, !.


% %%%%
% acompare: action compare :
% %%%%
acompare(=, A1, A2):-
    A1 == A2, !.
acompare(<, move(A1,B1,C1), move(A2,B2,C2)):-
    count_ground([A1,B1,C1], L1),
    count_ground([A2,B2,C2], L2),
    L1 > L2, !.
acompare(<, move(A1,B1,C1), move(A2,B2,C2)):-
    move(A1,B1,C1) \== move(A2,B2,C2),
    count_ground([A1,B1,C1], L),
    count_ground([A2,B2,C2], L), !.
acompare(>, move(A1,B1,C1), move(A2,B2,C2)):-
    count_ground([A1,B1,C1], L1),
    count_ground([A2,B2,C2], L2),
    L1 < L2, !.




sortByQValue(List, Sorted):-
    predsort(qcompare, List, Sorted), !.

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



% qcompare(<, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 > Q2, !.
% qcompare(>, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 < Q2, !.
% qcompare(<, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
%     length(S1, LS1), length(S2, LS2), LS1 =< LS2, !.
% qcompare(>, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
%     length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
% qcompare(=, QRule1, QRule2):-
%     QRule1 = QRule2, !.

% qcompare(=, partialQ(Q1,A1,S1), partialQ(Q,A,S)) :- !.
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


mypredsort(P, L, R) :-
    '$skip_list'(N, L, Tail),
    (   Tail == []
    ->  mypredsort(P, N, L, _, R1),
        R = R1
    ;   must_be(L, list)
    ).

mypredsort(P, 2, [X1, X2|L], L, R) :-
    !,
    call(P, Delta, X1, X2),
    sort2(Delta, X1, X2, R).
mypredsort(_, 1, [X|L], L, [X]) :- !.
mypredsort(_, 0, L, L, []) :- !.
mypredsort(P, N, L1, L3, R) :-
    N1 is N // 2,
    plus(N1, N2, N),
    mypredsort(P, N1, L1, L2, R1),
    mypredsort(P, N2, L2, L3, R2),
    mypredmerge(P, R1, R2, R).

sort2(<, X1, X2, [X1, X2]).
% sort2(=, X1, _, [X1]).
sort2(=, X1, X2, [X1, X2]).
sort2(>, X1, X2, [X2, X1]).

mypredmerge(_, [], R, R) :- !.
mypredmerge(_, R, [], R) :- !.
mypredmerge(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2),
    !,
    mypredmerge(Delta, P, H1, H2, T1, T2, Result).

mypredmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
    mypredmerge(P, [H1|T1], T2, R).
mypredmerge(=, P, H1, _, T1, T2, [H1|R]) :-
    mypredmerge(P, T1, T2, R).
mypredmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
    mypredmerge(P, T1, [H2|T2], R).

% sortByQValue(List, Sorted):-
%     predsort(qcompare, List, Sorted), !.
%
% %%
% qcompare(<, q(Q1,_), q(Q2,_)):- Q1 > Q2, !.
% qcompare(<, q(Q,S1), q(Q,S2)):-
%     length(S1, LS1), length(S2, LS2), LS1 =< LS2, !.
% qcompare(>, q(Q1,_), q(Q2,_)):- Q1 < Q2, !.
% qcompare(>, q(Q,S1), q(Q,S2)):-
%     length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
% qcompare(=, QRule1, QRule2):-
%     QRule1 = QRule2, !.


%%
% sort_numbers(List, Sorted):
% List is a list of sta([ClL,OnL], State)
% sort_numbers(List, Sorted):-
%     predsort(numbercompare, List, Sorted), !.
% % compare first On, then Cl
% numbercompare(>, sta([_,On1],_), sta([_,On2],_)):- On2 > On1, !.
% numbercompare(>, sta([Cl1,On],_), sta([Cl2,On],_)):-
%     Cl1 =< Cl2, !.
% numbercompare(<, sta([_,On1],_), sta([_,On2],_)):- On1 > On2, !.
% numbercompare(<, sta([Cl1,On],_), sta([Cl2,On],_)):-
%     Cl1 > Cl2, !.


% sortByQValuep(List, Sorted):-
%     predsort(partialqcompare, List, Sorted), !.
% partialqcompare(>, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q2 > Q1, !.
% partialqcompare(>, partialQ(Q,_,Size1,_,_,_), partialQ(Q,_,Size2,_,_,_)):-
%     length(Size1, LSize1), length(Size2, LSize2), LSize1 >= LSize2, !.
% partialqcompare(<, partialQ(Q1,_,_,_,_,_), partialQ(Q2,_,_,_,_,_)):- Q1 > Q2, !.
% partialqcompare(<, partialQ(Q,_,Size1,_,_,_), partialQ(Q,_,Size2,_,_,_)):-
%     length(Size1, LSize1), length(Size2, LSize2), LSize1 < LSize2, !.

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
