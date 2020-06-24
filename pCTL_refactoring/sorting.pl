:- module(sorting, [sortByQValue/2, sort_numbers/2, list_to_set1/2,
                    subsumesort/2, ssort/3]).

subsumesort(List, Sorted):-
    predsort(ssort, List, Sorted), !.


ssort(<, cl(_), on(_,_)) :- !.
ssort(>, on(_,_), cl(_)) :- !.

ssort(<, cl(A), cl(B)) :-
    ground(A), \+ground(B), !.
ssort(>, cl(A), cl(B)) :-
    \+ground(A), ground(B), !.
ssort(<, cl(A), cl(B)) :-
    ground(A), ground(B), compare(<, A, B), !.
ssort(>, cl(A), cl(B)) :-
    ground(A), ground(B), compare(>, A, B), !.
ssort(<, cl(A), cl(B)) :-
    \+ground(A), \+ground(B), compare(<, A, B), !.
ssort(>, cl(A), cl(B)) :-
    \+ground(A), \+ground(B), compare(>, A, B), !.

ssort(<, on(A,B), on(C,D)) :-
    count_ground([A,B], NAB), count_ground([C,D], NCD),
    NAB > NCD, !.
ssort(>, on(A,B), on(C,D)) :-
    count_ground([A,B], NAB), count_ground([C,D], NCD),
    NAB < NCD, !.
ssort(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), \+ground(C), ground(D), !.
ssort(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), ground(C), \+ground(D), !.


ssort(<, on(A,B), on(C,D)) :-
    ground(A), ground(B), ground(C), ground(D),
    compare(<, A, C), !.
ssort(>, on(A,B), on(C,D)) :-
    ground(A), ground(B), ground(C), ground(D),
    compare(>, A, C), !.
ssort(<, on(A,B), on(C,D)) :-
    ground(A), ground(B), ground(C), ground(D),
    compare(=, A, C), compare(<, B, D),!.
ssort(>, on(A,B), on(C,D)) :-
    ground(A), ground(B), ground(C), ground(D),
    compare(=, A, C), compare(>, B, D),!.

ssort(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(<, A, C), !.
ssort(>, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(>, A, C), !.
ssort(<, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(=, A, C), compare(<, B, D),!.
ssort(>, on(A,B), on(C,D)) :-
    ground(A), \+ground(B), ground(C), \+ground(D),
    compare(=, A, C), compare(>, B, D),!.

ssort(<, on(A,B), on(C,D)) :-
    \+ground(A), \+ground(B), \+ground(C), \+ground(D),
    compare(<, A, C), !.
ssort(>, on(A,B), on(C,D)) :-
    \+ground(A), \+ground(B), \+ground(C), \+ground(D),
    compare(>, A, C), !.
ssort(<, on(A,B), on(C,D)) :-
    \+ground(A), \+ground(B), \+ground(C), \+ground(D),
    compare(=, A, C), compare(<, B, D),!.
ssort(>, on(A,B), on(C,D)) :-
    \+ground(A), \+ground(B), \+ground(C), \+ground(D),
    compare(=, A, C), compare(>, B, D),!.

ssort(<, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(<, B, D), !.
ssort(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(>, B, D), !.
ssort(<, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(=, B, D), compare(<, A, C),!.
ssort(>, on(A,B), on(C,D)) :-
    \+ground(A), ground(B), \+ground(C), ground(D),
    compare(=, B, D), compare(>, A, C),!.

ssort(>, X, Y) :- X = Y, !.



count_ground([] , 0):- !.
count_ground([E|R], Num):-
    ground(E),
    count_ground(R, RNum),
    Num is RNum+1, !.

count_ground([E|R], Num):-
    \+ground(E),
    count_ground(R, Num), !.


sortByQValue(List, Sorted):-
    predsort(qcompare, List, Sorted), !.

qcompare(<, q(Q1,_,_), q(Q2,_,_)):- Q1 > Q2, !.
qcompare(>, q(Q1,_,_), q(Q2,_,_)):- Q1 < Q2, !.
qcompare(<, q(Q,_,S1), q(Q,_,S2)):-
    length(S1, LS1), length(S2, LS2), LS1 =< LS2, !.
qcompare(>, q(Q,_,S1), q(Q,_,S2)):-
    length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
qcompare(=, QRule1, QRule2):-
    QRule1 = QRule2, !.


qcompare(<, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 > Q2, !.
qcompare(>, partialQ(Q1,_,_), partialQ(Q2,_,_)):- Q1 < Q2, !.
qcompare(<, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
    length(S1, LS1), length(S2, LS2), LS1 =< LS2, !.
qcompare(>, partialQ(Q,_,S1), partialQ(Q,_,S2)):-
    length(S1, LS1), length(S2, LS2), LS1 > LS2, !.
qcompare(=, QRule1, QRule2):-
    QRule1 = QRule2, !.




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
sort_numbers(List, Sorted):-
    predsort(numbercompare, List, Sorted), !.
% compare first On, then Cl
numbercompare(>, sta([_,On1],_), sta([_,On2],_)):- On2 > On1, !.
numbercompare(>, sta([Cl1,On],_), sta([Cl2,On],_)):-
    Cl1 =< Cl2, !.
numbercompare(<, sta([_,On1],_), sta([_,On2],_)):- On1 > On2, !.
numbercompare(<, sta([Cl1,On],_), sta([Cl2,On],_)):-
    Cl1 > Cl2, !.


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
