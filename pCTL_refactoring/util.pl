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

% % checkstacknums/2: succeeds iff StaNum1 subsumes StaNum2
% % this serves as an optimization (not necessary)
% checkstacknums([], _) :- !.
% checkstacknums([[Cl1,On1]|StaNum1], StaNum2):-
%     select([Cl2,On2], StaNum2, StaNum2Tail),
%     On1 =< On2, Cl1 =< Cl2, !,
%     checkstacknums(StaNum1, StaNum2Tail), !.

% % smartsubset([[Cl1,OnL1]], [Str1], [sta([Cl2, On2], Str2)]):
% %    succeeds if state1 subsumes state2
% % Uses backtracking to find possible mappings for two states
% smartsubset([], [], _) :- !.
% smartsubset([[Cl1,On1]|SizeS1], [S1|Str1], StaS2) :-
%     % given a stack from S1, select a stack from S2
%     select(sta([Cl2,On2], S2), StaS2, StaS2Tail),
%     On1 =< On2, Cl1 =< Cl2,
%     % check if the stack from S1 subsumes the selected stack from S2
%     take(S2, [Cl1, On1], SubS2),
%     subsumes_term(S1, SubS2),
    %
    % % check if the other stacks can be subsumed.
    % % This is an optimization to fail early
    % % TODO does this optimization work?
    % getSizeFromSta(StaS2Tail, NewSizeS2),
    % (checkstacknums(SizeS1, NewSizeS2)->true;writeln(dd)),
    %
    % smartsubset(SizeS1, Str1, StaS2Tail), !.

% from a list of sta([Cl,On], _) get a list of [Cl,On]
% getSizeFromSta([], []):- !.
% getSizeFromSta([sta([Cl,On], _)|RSta], [[Cl,On]|RSize]):-
%     getSizeFromSta(RSta, RSize), !.

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


% take(OldState, [ClL, OnL], NewState): NewState contains the first
% ClL cl/1 and the first OnL on/2 of OldState
% OldState/NewState are sorted that cl/1 are before on/2
% TODO: this can be optimized to avoid list manipulation twice
% take([ClNew|_], [1, 0], [ClNew]):- !.
% take(Old, [0, OnL], OnNew) :-
%     predInList(Old, on, OnOld), % take all on/2
%     length(OnNew, OnL),
%     prefix(OnNew, OnOld), !. % take the first OnL1 on/2
%
% take(Old, [1, OnL], [ClNew|OnNew]):-
%     Old = [ClNew|_],
%     predInList(Old, on, OnOld),
%     length(OnNew, OnL),
%     prefix(OnNew, OnOld), !.

%%
% getstruct(S, StaSize, StaStr, StackSorted):-
%     extract(S),
%     stateMetaData(StaSize, StaStr, StackSorted), !.
% getstruct(S, _,_,_).

% can we optimize find_stacks(Stacks) s.t. it returns StaSize, StaStr directly
% to get rid of getstrnum/3? No, because we sort_numbers that sorts
% stateMetaData(StaSize, StaStr, StackSorted):-
%     collect, collect_unclear,
%     collectstack, find_stacks(Stacks), !,
%     % Stacks/StackSorted is a list of sta([ClL,OnL], State)
%     sort_numbers(Stacks, StackSorted), !,
%     getstrnum(StackSorted, StaSize, StaStr), !,
%     clean, !.

getstate(Glb):-
    collect,
    findstate(Glb1), clean, !,
    subsumesort(Glb1,Glb).

% mergestacks([], [], []) :- !.
% mergestacks([Stack|RStacks], Cls, Ons):-
%     predInList(Stack, cl, ClStack),
%     predInList(Stack, on, OnStack),
%     % TODO Why append? is it necessary?
%     append(ClStack, RCls, Cls),
%     append(OnStack, ROns, Ons),
%     mergestacks(RStacks, RCls, ROns), !.
% mergestacks(Stacks, Glb): sort the stacks into one big state where
% cl/1 are before on/2
% Stacks is a list of stacks containing cl/1 and on/2
% mergestacks(Stacks, Glb):-
%     mergestacks(Stacks, Cls, Ons),
%     append(Cls, Ons, Glb), !.

% another version (not better)
% mergestacks([], [], []) :- !.
% mergestacks([Stack|RStacks], [ClStack|RCls], [OnStack|ROns]):-
%     predInList(Stack, cl, ClStack),
%     predInList(Stack, on, OnStack),
%     mergestacks(RStacks, RCls, ROns), !.
%
% mergestacks(Stacks, Glb):-
%     mergestacks(Stacks, Cls, Ons), !,
%     flatten(Cls, FlatCls), !,
%     flatten(Ons, FlatOns), !,
%     append(FlatCls, FlatOns, Glb), !.

% getstrnum(Stas,StrNums,Strs):
% Stas is a list of sta([ClL,OnL], State)
% StrNums is the [ClL,OnL] part
% Strs is the State part
% getstrnum([], [], []) :- !.
% getstrnum([sta(L,S)|Str], [L|StrNum], [S|StrS]):-
%     getstrnum(Str,StrNum,StrS), !.

% This is for an older version of swi prolog
% predInList(OldState, ClOn, NewState): NewState contains all ClOn
% elements of OldState.
% OldState is sorted that Cl elements are before On elements
% predInList([], _, []):- !. % base cases
% predInList([on(_,_)|_], cl, []):- !. % stop taking cl/1 when encountering on/2
% predInList([on(X,Y)|ROns], on, [on(X,Y)|ROns]):- !. % take all on/2
% predInList([cl(X)|S], cl, [cl(X)|SubS]):- % take the head cl/1
%     predInList(S, cl, SubS), !.
% predInList([cl(_)|S], on, SubS):- % skip cl/1 when targetting on/2
%     predInList(S, on, SubS), !.



% This is for an older version of swi prolog
% filter(VN, Op, Threshold, NewVN): NewVN contains the v(Q,S)
% that satisfy "Q Op Threshold" in VN
% filter([], _, _, []) :- !.
% filter([v(Q,S)|VN], Op, Threshold, [v(Q,S)|NewVN]):-
%     apply(Op, [Q, Threshold]),
%     filter(VN, Op, Threshold, NewVN), !.
% filter([_|VN], Op, Threshold, NewVN):-
%     filter(VN, Op, Threshold, NewVN), !.

% This is for a newer version of swi prolog
filter(VN, Op, Threshold, NewVN) :-
    include([v(Q,_)]>>apply(Op, [Q, Threshold]), VN, NewVN), !.

% getVFState: get the states in the value functions
getVFState(v(_,State), State):- !.
getVFStates(VFs, States):-
    maplist(getVFState, VFs, States).

%%
constructAbsorbingVFs(S, v(1.0, S)).
% constructAbsorbingQs(S, q(1.0, S, Size, Str, Sta)):-
%     getstruct(S, Size, Str, Sta).
constructAbsorbingQs(S, q(1.0, _, S)).

% andstate/2:
% get "E1 and E2"
% andstate(E1, E2, Result):-
%     extract(E1), extract(E2),
%     stateMetaData(_, StaStr, _),
%     mergestacks(StaStr, Result), !.
andstate(E1, E2, Result):-
    extract(E1), extract(E2),
    getstate(Result), !.
% if E1 and E2 cannot be combined, return []
andstate(_, _, []):-!.


legalstate(S):-
  extract(S), clean, !.


%%%%%%%%%%%%%%%OIOIOIOI%%%%%%%%%%%%%%%
% get number of terms
termsInState([], []) :- !.
termsInState([cl(X)|S], [X|B]):-
    termsInState(S, B), !.
termsInState([on(X,Y)|S], [X,Y|B]):-
    termsInState(S, B), !.


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
    list_to_set(Terms, TermSet),
    length(TermSet, LargestObjectNum),
    blocks_limit(ObjectBound),
    generateBoundedStates(S, TermSet, LargestObjectNum, ObjectBound).

oi_qrule(partialQ(_,_,_)):-
    blocks_limit(non), !.

oi_qrule(partialQ(_,_,S)):-
    termsInState(S, Terms),
    list_to_set(Terms, TermSet),
    length(TermSet, LargestObjectNum),
    blocks_limit(ObjectBound),
    generateBoundedStates(S, TermSet, LargestObjectNum, ObjectBound).
%%%%%%%%%%%%%%%OIOIOIOI%%%%%%%%%%%%%%%
