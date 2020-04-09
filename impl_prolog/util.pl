:- module(util, [subsumess/4, subsetgen/2, predInList/3, extract/1,
stateMetaData/3, mergestacks/2, constructAbsorbingVFs/2,
constructAbsorbingQs/2, structsubset/4, printall_format/1, filter/4,
getVFStates/2, printall/1, andstate/3, legalstate/2, oi/1, legalstate/1,
boundedstate/2]).

:- use_module(setting).

% subsumess/4: check whether S1 subsumes S2
% S2 needs to be given, otherwise this enters an infinite loop
% Size1/Size2: a list of stack structures, e.g. [[0,2],[1,3]] represents
% two stacks, first stack with 2 blocks without a clear top block,
% second stack with 3 stacks with a clear top block
% Size1/Size2 is sorted
subsumess(Size1,Str1,Size2,Sta2):-
    checkstacknums(Size1, Size2), !,
    smartsubset(Size1, Str1, Sta2), !.

% checkstacknums/2: succeeds iff StaNum1 subsumes StaNum2
% this serves as an optimization (not necessary)
checkstacknums([], _) :- !.
checkstacknums([[Cl1,On1]|StaNum1], StaNum2):-
    select([Cl2,On2], StaNum2, StaNum2Tail),
    On1 =< On2, Cl1 =< Cl2, !,
    checkstacknums(StaNum1, StaNum2Tail), !.

% smartsubset([[Cl1,OnL1]], [Str1], [sta([Cl2, On2], Str2)]):
%    succeeds if state1 subsumes state2
% Uses backtracking to find possible mappings for two states
smartsubset([], [], _) :- !.
smartsubset([[Cl1,On1]|SizeS1], [S1|Str1], StaS2) :-
    % given a stack from S1, select a stack from S2
    select(sta([Cl2,On2], S2), StaS2, StaS2Tail),
    On1 =< On2, Cl1 =< Cl2,
    % check if the stack from S1 subsumes the selected stack from S2
    take(S2, [Cl1, On1], SubS2),
    subsumes_term(S1, SubS2),

    % check if the other stacks can be subsumed.
    % This is an optimization to fail early
    % TODO does this optimization work?
    getSizeFromSta(StaS2Tail, NewSizeS2),
    (checkstacknums(SizeS1, NewSizeS2)->true;writeln(dd)),

    smartsubset(SizeS1, Str1, StaS2Tail), !.

% from a list of sta([Cl,On], _) get a list of [Cl,On]
getSizeFromSta([], []):- !.
getSizeFromSta([sta([Cl,On], _)|RSta], [[Cl,On]|RSize]):-
    getSizeFromSta(RSta, RSize), !.

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
% same structure as S1
% S1 is an state template (list of nonground cl/1 and on/2)
% S2 is a state
% output: ClSubS2, OnSubS2
structsubset(S1, S2, ClSubS2, OnSubS2):-
    duplicate_term(S1, SubS2),
    append(ClSubS2, OnSubS2, SubS2),
    predInList(S2, cl, ClS2),
    predInList(S2, on, OnS2),
    subsetgen(ClS2, ClSubS2),
    subsetgen(OnS2, OnSubS2).

% take(OldState, [ClL, OnL], NewState): NewState contains the first
% ClL cl/1 and the first OnL on/2 of OldState
% OldState/NewState are sorted that cl/1 are before on/2
% TODO: this can be optimized to avoid list manipulation twice
take([ClNew|_], [1, 0], [ClNew]):- !.
take(Old, [0, OnL], OnNew) :-
    predInList(Old, on, OnOld), % take all on/2
    length(OnNew, OnL),
    prefix(OnNew, OnOld), !. % take the first OnL1 on/2

take(Old, [1, OnL], [ClNew|OnNew]):-
    Old = [ClNew|_],
    predInList(Old, on, OnOld),
    length(OnNew, OnL),
    prefix(OnNew, OnOld), !.

%%
getstruct(S, StaSize, StaStr, StackSorted):-
    extract(S),
    stateMetaData(StaSize, StaStr, StackSorted), !.

% can we optimize find_stacks(Stacks) s.t. it returns StaSize, StaStr directly
% to get rid of getstrnum/3? No, because we sort_numbers that sorts
stateMetaData(StaSize, StaStr, StackSorted):-
    collect, collect_unclear,
    collectstack, find_stacks(Stacks), !,
    % Stacks/StackSorted is a list of sta([ClL,OnL], State)
    sort_numbers(Stacks, StackSorted), !,
    getstrnum(StackSorted, StaSize, StaStr), !,
    clean, !.


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
mergestacks([], [], []) :- !.
mergestacks([Stack|RStacks], [ClStack|RCls], [OnStack|ROns]):-
    predInList(Stack, cl, ClStack),
    predInList(Stack, on, OnStack),
    mergestacks(RStacks, RCls, ROns), !.

mergestacks(Stacks, Glb):-
    mergestacks(Stacks, Cls, Ons), !,
    flatten(Cls, FlatCls), !,
    flatten(Ons, FlatOns), !,
    append(FlatCls, FlatOns, Glb), !.

% getstrnum(Stas,StrNums,Strs):
% Stas is a list of sta([ClL,OnL], State)
% StrNums is the [ClL,OnL] part
% Strs is the State part
getstrnum([], [], []) :- !.
getstrnum([sta(L,S)|Str], [L|StrNum], [S|StrS]):-
    getstrnum(Str,StrNum,StrS), !.

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

% This is for a newer version of swi prolog
predInList(OldState, Type, NewState):-
    include([X]>>functor(X, Type, _), OldState, NewState), !.

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
constructAbsorbingQs(S, q(1.0, S, Size, Str, Sta)):-
    getstruct(S, Size, Str, Sta).

% andstate/2:
% get "E1 and E2"
andstate(E1, E2, Result):-
    extract(E1), extract(E2),
    stateMetaData(_, StaStr, _),
    mergestacks(StaStr, Result), !.
% if E1 and E2 cannot be combined, return []
andstate(_, _, []):-!.


% read the setting: bounded or unbounded
% legalstate(S, _):-
%     blocks_limit(non), !,
%     legalstateUnbounded(S), !.
%
% legalstate(_, Size):-
%     \+blocks_limit(non), !,
%     legalstatebounded(Size), !.
%
%
% legalstateUnbounded(S):-
%   extract(S), clean, !.
%
% legalstatebounded(Size):-
%     extract(S), clean, !,
%     blocks_limit(MaxBlocks),
%     onSize(Size, OnSize),
%     sum_list(OnSize, B), !,
%     B =< MaxBlocks.

boundedstate(S,B):-
    termsInState(S),
    list_to_set(Terms, TermSet),
    length(TermSet, BB),
    BB =< B.

legalstate(S):-
  extract(S), clean, !.

onSize([], []):-!.
onSize([[_,On]|Size], [On1|R]):-
    On1 is On+1,
    onSize(Size, R), !.

% min_num_blocks(S, N): state S has at least N blocks
min_num_blocks(S, N):-
    termsInState(S, ListBlocks), !,
    duplicate_term(ListBlocks, ListBlocks1), !,
    list_to_set1(ListBlocks1, SetBlocks), !,
    length(SetBlocks, N), !.

% termsInState(S, B): state S has a set of blocks B
termsInState([], []) :- !.
termsInState([cl(X)|S], [X|B]):-
    termsInState(S, B), !.
termsInState([on(X,Y)|S], [X,Y|B]):-
    termsInState(S, B), !.

%
% this uses backtracking to create all specifications
list2set([] , []).
list2set([E|Es] , Set) :-
    member(E, Es) ,
    list2set(Es , Set).
list2set([E|Es] , [E|Set] ) :-
    maplist(mydif(E), Es),
    list2set(Es , Set).



%%
oi(State, non):-
    termsInState(State, Terms),
    list_to_set(Terms, TermSet),
    list2set(TermSet, TermUnifiedSet),
    legalstate(State).
    
oi(State, B):-
    B \= non,
    termsInState(State, Terms),
    list_to_set(Terms, TermSet),
    list2set(TermSet, TermUnifiedSet),
    legalstate(State),
    boundedstate(State, B).

%%
printall([]):- !.
printall([E|R]):-
    writeln(E),
    printall(R),!.

%%
printall_format([]):-!.
printall_format([q(Prob, S, Size, _, _)|R]):-
    writeln(q(Prob,S,Size)),
    printall_format(R),!.
printall_format([partialQ(Q1,A1,S1,Size1,_,_)|R]):-
    writeln(partialQ(Q1,A1,S1,Size1)),
    printall_format(R),!.
printall_format([v(Q,S)|R]):-
    writeln(v(Q,S)),
    printall_format(R),!.
