:- module(util, [subsetgen/2, extract/1, constructAbsorbingVFs/2,
constructAbsorbingQs/2, structsubset/3, filter/4,
getVFStates/2, andstate/3, oi_qrule/1, legalstate/1,
thetasubsumes/2, getstate/1, cartesian_dif/2,
 generateOIstate/2]).

:- use_module(sorting).
:- use_module(setting).


%%
cartesian_dif([], _):- !.
cartesian_dif([E|L1], L2):-
    maplist(mydif(E), L2), !,
    cartesian_dif(L1,L2), !.


thetasubsumes(S1,S2):-
    % predInList(S1, cl, ClS1),length(ClS1, LClS1),
    % predInList(S1, on, OnS1),length(OnS1, LOnS1),
    % predInList(S2, cl, ClS2),length(ClS2, LClS2),
    % predInList(S2, on, OnS2),length(OnS2, LOnS2),
    % LClS1 =< LClS2, LOnS1 =<LOnS2,
    \+(\+((
        numbervars(S2,999,_,[attvar(bind)]),
        mysubset(S1,S2)
    ))).

mysubset([], _) :- !.
mysubset([E|R], Set) :-
    member(E, Set),
    mysubset(R, Set).



% subsetgen(set, subset): this predicate use backtracking to generate
% all possible subsets
subsetgen([], []):- !.
subsetgen([E|Tail], [E|NTail]):-
    subsetgen(Tail, NTail).
subsetgen([_|Tail], NTail):-
    subsetgen(Tail, NTail).

% extract factored state
extract([]) :- !.
extract([E|L]):-
   E,
   extract(L), !.

% create a subset SubS2 of S2 that has the
% same structure as SubH
% SubH is an state template (list of nonground cl/1 and on/2)
% S2 is a state
% output: SubS2
structsubset(SubH, S2, SubS2):-
    stateGroup(SubH, GroupedSubH),
    maplist(length, GroupedSubH, LGroupedSubH),
    stateGroup(S2, GroupedS2),
    maplist(length, GroupedSubS2, LGroupedSubH),
    maplist(subsetgen, GroupedS2, GroupedSubS2),
    % create all possible \theta
    maplist(permutation, GroupedSubH, GroupedSubHp),
    maplist(=, GroupedSubHp, GroupedSubS2),
    flatten(GroupedSubS2, SubS2).

getstate(Glb):-
    collect,
    findstate(Glb1), clean, !,
    subsumesort(Glb1,Glb).


% This is for an older version of swi prolog
% filter(VN, Op, Threshold, NewVN): NewVN contains the v(Q,S)
% that satisfy "Q Op Threshold" in VN
filter(VN, Op, Threshold, NewVN) :-
    include([v(Q,_)]>>apply(Op, [Q, Threshold]), VN, NewVN), !.

% getVFState: get the states in the value functions
getVFState(v(_,State), State):- !.
getVFStates(VFs, States):-
    maplist(getVFState, VFs, States).

%%
constructAbsorbingVFs(S, v(1.0, S)).
constructAbsorbingQs(S, q(1.0, _, S)).

% andstate/2:
% get "E1 and E2"
andstate(E1, E2, Result):-
    extract(E1), extract(E2),
    getstate(Result), !.
% if E1 and E2 cannot be combined, return []
andstate(_, _, []) :- !.


legalstate(S):-
  extract(S), clean, !.


%%%%%%%%%%%%%%%OIOIOIOI%%%%%%%%%%%%%%%
termsInState(State, Terms):-
    maplist([Pred, Args]>> =..(Pred,[_|Args]), State, ArgsList),
    flatten(ArgsList, TermsList),
    list_to_set(TermsList, Terms).

% generateOIstate(NonOIState, OIState)
% this uses backtracking to create all oi specifications
generateOIstate([] , []) :- !.
generateOIstate([E|Es] , [E|Set] ) :-
    maplist(mydif(E), Es), % TODO test the order
    generateOIstate(Es , Set).

generateOIstate([E|Es] , Set) :-
    member(E, Es),
    generateOIstate(Es , Set).

generateBoundedStates(_, _, LargestObjectNum, ObjectBound):-
    LargestObjectNum =< ObjectBound.

generateBoundedStates(State, TermSet, LargestObjectNum, ObjectBound):-
    LargestObjectNum > ObjectBound,
    generateOIstate(TermSet, TermSetSet),
    legalstate(State),
    length(TermSetSet, LTermSetSet),
    LTermSetSet =< ObjectBound.

oi_qrule(q(_,_,_)):-
    blocks_limit(non), !.

oi_qrule(q(_,_,S)):-
    termsInState(S, Terms),
    length(Terms, LargestObjectNum),
    blocks_limit(ObjectBound),
    generateBoundedStates(S, Terms, LargestObjectNum, ObjectBound).

oi_qrule(partialQ(_,_,_)):-
    blocks_limit(non), !.

oi_qrule(partialQ(_,_,S)):-
    termsInState(S, Terms),
    list_to_set(Terms, TermSet),
    length(TermSet, LargestObjectNum),
    blocks_limit(ObjectBound),
    generateBoundedStates(S, TermSet, LargestObjectNum, ObjectBound).
%%%%%%%%%%%%%%%OIOIOIOI%%%%%%%%%%%%%%%
