:- module(sorting, [sortByQValue/2, sort_numbers/2, sortByQValuep/2, list_to_set1/2]).

sortByQValue(List, Sorted):-
    predsort(qcompare, List, Sorted), !.

%%
qcompare(>, q(Q1,_,_,_,_), q(Q2,_,_,_,_)):- Q2 > Q1, !.
% qcompare(>, q(Q,_,_,_,Sta), q(Q,_,_,_,_)):-
%     length(Size1, LSize1), length(Size2, LSize2), LSize1 >= LSize2, !.
% qcompare(>, q(Q,_,Size1,_,_), q(Q,_,Size2,_,_)):-
%     length(Size1, LSize1), length(Size2, LSize2), LSize1 >= LSize2, !.
qcompare(<, q(Q1,_,_,_,_), q(Q2,_,_,_,_)):- Q1 >= Q2, !.
% qcompare(<, q(Q,_,Size1,_,_), q(Q,_,Size2,_,_)):-
%     length(Size1, LSize1), length(Size2, LSize2), LSize1 < LSize2, !.
% qcompare(=, QRule1, QRule2):-
%     QRule1 = QRule2, !.


%%
% sort_numbers(List, Sorted):
% List is a list of sta([ClL,OnL], State)
sort_numbers(List, Sorted):-
    predsort(numbercompare, List, Sorted), !.
% compare first On, then Cl
numbercompare(>, sta(_,[Bin1,_,_],_), sta(_,[Bin2,_,_],_)):- Bin2 > Bin1, !.
numbercompare(>, sta(_,[Bin,Tin1,_],_), sta(_,[Bin,Tin2,_],_)):- Tin2 > Tin1, !.
numbercompare(>, sta(_,[Bin,Tin,On1],_), sta(_,[Bin,Tin,On2],_)):- On2 > On1, !.

numbercompare(<, sta(_,[Bin1,_,_],_), sta(_,[Bin2,_,_],_)):- Bin2 < Bin1, !.
numbercompare(<, sta(_,[Bin,Tin1,_],_), sta(_,[Bin,Tin2,_],_)):- Tin2 < Tin1, !.
numbercompare(<, sta(_,[Bin,Tin,On1],_), sta(_,[Bin,Tin,On2],_)):- On2 =< On1, !.


sortByQValuep(List, Sorted):-
    predsort(partialqcompare, List, Sorted), !.
partialqcompare(>, partialQ(Q1,_,_,_,_,_), partialQ(Q2,_,_,_,_,_)):- Q2 > Q1, !.
partialqcompare(>, partialQ(Q,_,Size1,_,_,_), partialQ(Q,_,Size2,_,_,_)):-
    length(Size1, LSize1), length(Size2, LSize2), LSize1 >= LSize2, !.
partialqcompare(<, partialQ(Q1,_,_,_,_,_), partialQ(Q2,_,_,_,_,_)):- Q1 > Q2, !.
partialqcompare(<, partialQ(Q,_,Size1,_,_,_), partialQ(Q,_,Size2,_,_,_)):-
    length(Size1, LSize1), length(Size2, LSize2), LSize1 < LSize2, !.

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
