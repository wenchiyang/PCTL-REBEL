:- use_module(library(chr)).
:- use_module(precond).
:- use_module(sorting).
:- use_module(util).
:- set_prolog_flag(stack_limit, 12000000000).
:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize,full).

:- dynamic memo_/1.
% memo/1:
% if Goal has been executed, use the result,
% otherwise execute Goal and insert the result into the database
memo(Goal) :-
        (   memo_(Goal)
        ->  true
        ;   once(Goal),
            assertz(memo_(Goal))
        ).

experiment1 :-
    protocol('experiments/exp1_multithread.txt'),
    evaluate(until(10, [[]], [[on(a,b)]], >=, 0.9, Res), Res), !,
    noprotocol.

blocks_limit(10).

% Syntax checking -- Do not allow [].
% evaluate(Phi, []) :- functor(Phi, [], _), !.
% Phi is a set
evaluate(Phi, Phi) :-
    functor(Phi, '[|]', _), !.

% evaluate step-bounded until formula:
% PhiStates == P_{Op Threshold} [Phi1 U^{<= Steps} Phi2]
evaluate(Phi, PhiStates) :-
    Phi =.. [until, Steps, Phi1, Phi2, Op, Threshold, PhiStates],
    evaluate(Phi1, Phi1States), !,
    evaluate(Phi2, Phi2States), !,
    apply(until, [Steps, Phi1States, Phi2States, Op, Threshold, PhiStates]), !,
    print_message(informational, phistates(Phi, PhiStates)).

% evaluate next formula:
% PhiStates == P_{Op Threshold} [X Phi2]
evaluate(Phi, PhiStates) :-
    Phi =.. [next, Phi2, Op, Threshold, PhiStates],
    evaluate(Phi2, Phi2States), !,
    apply(next, [Phi2States, Op, Threshold, PhiStates]), !,
    print_message(informational, phistates(Phi, PhiStates)).

% evaluate and formula:
% PhiStates == Phi1 and Phi2
% evaluate(Phi, PhiStates) :-
%     Phi =.. [and, Phi1, Phi2, PhiStates],
%     evaluate(Phi1, Phi1States), !,
%     evaluate(Phi2, Phi2States), !,
%     apply(and, [Phi1States, Phi2States, PhiStates]), !,
%     list_to_set1(PhiStates, SortedStates),
%     length(PhiStates, L1), length(SortedStates, L2), writeln([L1,L2]),
%     print_message(informational, phistates(Phi, SortedStates)).


% get cartesian product of two state lists,
% Res is a list of combined states
% and([], _, []):-!.
% and([E1|States1], States2, Res):-
%     maplist(andstate(E1), States2, PartialRes),
%     delete(PartialRes, [], PartialRes1),
%     append(PartialRes1, PhiStates, Res),
%     and(States1, States2, PhiStates).

until(Steps, Phi1s, Phi2s, Op, Threshold, SortedStates) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    maplist(constructAbsorbingQs,Phi2s,Phi2sQs), !,
    valueIteration(Steps, 1, InitV, _, Phi1s, Phi2sQs, FinalVs), !,
    % TODO combine filter and getVFStates to optimize
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.

next(Phi2s, Op, Threshold, SortedStates) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    valueIteration(1, 1, InitV, _, [[]], [], FinalVs), !,
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.


valueIteration(TotalSteps, CurrentStep, _, [_, FinalVS], _, _, FinalVS) :-
    CurrentStep =:= TotalSteps + 1, !.
valueIteration(TotalSteps, CurrentStep, InitV, Vs, Phi1s, Phi2sQs, FinalVs):-
    RemoveMemoryIndex is CurrentStep - 2,
    retractall(memo_(valueIteration_helper(RemoveMemoryIndex,_,_,_,_))),
    print_message(informational, iteration(CurrentStep)),
    memo(valueIteration_helper(CurrentStep, InitV, Vs, Phi1s, Phi2sQs)), !,
    Vs = [CurrentVs,_],
    NextStep is CurrentStep + 1,
    printall_format(CurrentVs), nl, nl,
    valueIteration(TotalSteps, NextStep, InitV, [_, CurrentVs], Phi1s, Phi2sQs, FinalVs),
    !.
%
valueIteration_helper(0, InitV, [InitV,[]], _, _):-!.
valueIteration_helper(CurrentStep, InitV, [CurrentV,PreviousV], Phi1s, Phi2sQs):-
    PreviousStep is CurrentStep-1,
    memo(valueIteration_helper(PreviousStep, InitV, [PreviousV,_], Phi1s, Phi2sQs)),
    oneIteration(PreviousV, CurrentV, Phi1s, Phi2sQs),
    !.

oneIteration(VFs, NewVFs, Phi1s, Phi2sQs):-
    %writeln(oneIteration(Phi1s, Phi2sQs)),
    append(VFs, [v(0.0, [])], VFs1),
    %%% step 1: regression, producing partial Q rules
        %statistics(runtime, [Start1|_]),
        %statistics(global_stack, [Used1,_]),
        %write("stack before partialQ1 : "),
        %print_message(informational, stackusage(Used1)),
    thread_create(getPartialQwp1(VFs1, Phi1s, SPQs1),Id1), !,
    thread_join(Id1, exited([SPQs1, Time1])),
        %statistics(global_stack, [Used2,_]),
        %write("stack before partialQ2 : "),
        %print_message(informational, stackusage(Used2)),
    thread_create(getPartialQwp2(VFs1, Phi1s, SPQs2),Id2), !,
        %statistics(global_stack, [Used3,_]),
        %write("stack before combining partialQs : "),
        %print_message(informational, stackusage(Used3)),
    thread_join(Id2, exited([SPQs2, Time2])),
        length(SPQs1, LSPQs1), length(SPQs2, LSPQs2),
        write("partialQs: "), writeln([LSPQs1, LSPQs2]),
    %printall_format(SPQs1), nl,
    %printall_format(SPQs2), nl,
        %statistics(runtime, [Stop1|_]),
    print_message(informational, partialQtime(Time1, Time2)),
        %write("    step 1 : "), write(Step1), writeln(" s"),
    %%% step 2: combining, producing Q rules
        %statistics(runtime, [Start2|_]),
    thread_create(findall_Qrules_wrapper(Q, Phi2sQs, getQ(SPQs1, SPQs2, Q), QRules), Id3), !,
    thread_join(Id3, exited([QRules, Time3])),

    %findall(Q, getQ(SPQs1, SPQs2, Q), QRules), !,
        %statistics(runtime, [Stop2|_]),
        %write("    step 2 : "), write(Step2), writeln(" s"),
        %statistics(global_stack, [Used4,_]),
        %write("stack after combining partialQs : "),
        %print_message(informational, stackusage(Used4)),
    print_message(informational, qtime(Time3)),
        %garbage_collect,
    %%% step 3: filtering, producing value functions
        %statistics(runtime, [Start3|_]),
    %printall(QRules),
    qTransfer(QRules, NewVFs),
    Time is Time1 + Time2 + Time3,
    write("Iteration CPU Time : "), writeln(Time),
    %length(AllQRules, L1), length(SortedAllQRules,L2),
    %write("q : "), writeln([L1,L2]),
    !.


% use backtracking to generate all Q rules from all possible
% combinations from partialQ1s and PartialQ2s
getQ(SPQs1, SPQs2, Q):-
    % try out all partialQ combinations from wp1 and wp2
    member(PartialQ1, SPQs1),
    member(PartialQ2, SPQs2),
    PartialQ1 = partialQ(Q1,A1,_,_,_,_),
    PartialQ2 = partialQ(_,A2,_,_,_,_),
    Q1 > 0,
    A1=A2, legalaction(A1),
    partialQstoQ(PartialQ1, PartialQ2, Q).

partialQstoQ(partialQ(Q1,_,_,Size1,Str1,_),
             partialQ(Q2,_,S2,Size2,Str2,Sta2),
             q(Q, S2, Size2, Str2, Sta2)):-
    subsumess(Size1,Str1,Size2,Sta2),
    legalstate(S2), !,
    Q is Q1 + Q2, !.

partialQstoQ(partialQ(Q1,_,S1,Size1,Str1,Sta1),
             partialQ(Q2,_,_,Size2,Str2,_),
             q(Q, S1,Size1,Str1,Sta1)):-
    subsumess(Size2,Str2,Size1,Sta1),
    legalstate(S1), !,
    Q is Q1 + Q2, !.

getPartialQwp1(VFs, Phi1s, SPQs1):-
    garbage_collect,
    statistics(runtime, [Start|_]),
    findall_partialQs(PQ1, wp1(VFs,Phi1s,PQ1), SPQs1), !,
    statistics(runtime, [Stop|_]),
    garbage_collect,
    Time is (Stop-Start)/1000,
    thread_exit([SPQs1, Time]),
    !.

getPartialQwp2(VFs, Phi1s, SPQs2):-
    garbage_collect,
    statistics(runtime, [Start|_]),
    findall_partialQs(PQ2, wp2(VFs,Phi1s,PQ2), SPQs2), !,
    statistics(runtime, [Stop|_]),
    garbage_collect,
    Time is (Stop-Start)/1000,
    thread_exit([SPQs2, Time]),
    !.

wp1(VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    VFValue > 0,
    mydif(X,Y), mydif(Y,Z), mydif(X,Z),
    wpi([cl(X), cl(Z), on(X,Y)], 0.9, move(X,Y,Z), [cl(X), cl(Y), on(X,Z)],
        Phi1s, VFValue, VFState, PQ).

wp2(VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(X,Y), mydif(Y,Z), mydif(X,Z),
    wpi([cl(X), cl(Y), on(X,Z)], 0.1, move(X,Y,Z), [cl(X), cl(Y), on(X,Z)],
        Phi1s, VFValue, VFState, PQ).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

% Takes an action rule "ActionHead <----Prob:[Act]---- ActionBody"
% and a value function "VFValue <----- VFState"
wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState,
    partialQ(Q,A,S,Size,Str,Sta)):-
    % get a subH
    subsetgen(Head, SubH),
    % get SubVFState
    predInList(SubH, cl, ClSubH), predInList(SubH, on, OnSubH),
    %length(ClSubH, LClSubH), length(OnSubH, LOnSubH),
    %% SubVFState=[ClSpp, OnSpp] and has structure [LClSubH, LOnSubH]
    structsubset(SubH, VFState, ClSpp, OnSpp), %%%% This can be optimized!!!!!
    %writeln([ClSpp, OnSpp]),
    % SubHp = SubVFState, % Then SubHp and SubVFState becomes identical
    permutation(ClSubH, ClSubHp),
    ClSubHp = ClSpp,
    permutation(OnSubH, OnSubHp),
    OnSubHp = OnSpp,
    append(ClSubHp, OnSubHp, SubHp),
    % if \theta exists
    sort(VFState, VFStateTT), sort(SubHp, SubHpTT),
    ord_subtract(VFStateTT, SubHpTT, VFSTail),
    headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
             partialQ(Q,A,S,Size,Str,Sta)).

%%%%% Step 1 : get weakest precondition
% Output : s(Prob, Act, S, VFValue)
headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
        partialQ(NewVFValue, Act, Glb, StaSize, StaStr, StackSorted)) :-
    member(Phi1, Phi1s), extract(Phi1), extract(VFSTail), extract(Body),
    stateMetaData(StaSize, StaStr, StackSorted),
    mergestacks(StaStr, Glb),
%%%%% Step 2A : get partial Q rules
% Output: partialQ(NewVFValue, Act, S)
    ord_union(Head, Body, Lpp),
    cartesian_dif(VFSTail, Lpp), !,
    NewVFValue is Prob * VFValue, !.


%%
cartesian_dif([], _):- !.
cartesian_dif([E|L1], L2):-
    maplist(mydif(E), L2), !,
    cartesian_dif(L1,L2), !.


%%
qTransfer([],[]):- !.
qTransfer([q(Q,S,_,_,_)|Qs],[v(Q,S)|Vs]):-
    qTransfer(Qs, Vs), !.




%%
findall_Qrules_wrapper(X, InitQs, Goal, Results) :-
    statistics(runtime, [Start|_]),
    findall_Qrules(X, InitQs, Goal, Results), !,
    statistics(runtime, [Stop|_]),
    Time is (Stop-Start)/1000,
    thread_exit([Results, Time]).


% findall_Qrules(Q, getQ(SPQs1, SPQs2, Q), QRules), !,
findall_Qrules(X, InitQs, Goal, Results) :-
    State = state(InitQs),
    (Goal,
    arg(1, State, S0),
    addQ(S0, X, S),
    sortByQValue(S, SortedS),
    nb_setarg(1, State, SortedS),
    fail
    ;
    arg(1, State, Results)
    ).

%gggfindall_Qrules(X, InitQs, Goal, Results) :-
%    garbage_collect,
%    findall(X, Goal, RawResults),
%    garbage_collect,
%    sortByQValue_new(RawResults, SortedRawResults),
%    append(InitQs, SortedRawResults, AllResults),
%    %printall(AllResults),
%    list_to_set1(AllResults, Results),
%    %garbage_collect,
%    !.



% assume List_Of_QRules is sorted
% Base case 1:
addQ([], New_QRule, [New_QRule]):-!.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addQ([q(Q1,S1,Size1,Str1,Sta1)|T0],
      q(Q,_,Size,_,Sta),
     [q(Q1,S1,Size1,Str1,Sta1)|T0]) :-
        Q1 >= Q,
        %allgroundin(S1, S),
        subsumess(Size1,Str1,Size,Sta), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addQ([q(Q1,_,Size1,_,Sta1)|T0],
     q(Q,S,Size,Str,Sta),
     T) :-
    Q1 =< Q,
    %allgroundin(S, S1),
    subsumess(Size,Str,Size1,Sta1), !,
    addQ(T0, q(Q,S,Size,Str,Sta), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addQ([QRule1|T0], New_QRule, [QRule1|T]) :-
    addQ(T0, New_QRule, T), !.


%%
%%
%% findall_partialQs(PQ1, wp1(VFs,Phi1s,PQ1), PQs1U)
findall_partialQs(X, Goal, Results) :-
  State = state([]),
  (  Goal,
     arg(1, State, S0),
     %addpartialQ(S0, X, SortedS),
     %append(S0, [X], SortedS),
     %sortByQValuep_new(S, SortedS),
     nb_setarg(1, State, [X|S0]),
     fail
  ;  arg(1, State, Reverse),
  reverse(Reverse, Results)
  ).


legalaction(move(X,Y,Z)):-
  X\=Y, Y\=Z, Z\=X, !.

legalstate(S):-
  extract(S), clean, !,
  % check state-boundedness
  blocks_limit(MaxBlocks),
  min_num_blocks(S, N),
  N =< MaxBlocks.

% min_num_blocks(S, N): state S has at least N blocks
min_num_blocks(S, N):-
    stateblocks(S, ListBlocks), !,
    duplicate_term(ListBlocks, ListBlocks1), !,
    list_to_set1(ListBlocks1, SetBlocks), !,
    length(SetBlocks, N), !.

% stateblocks(S, B): state S has a set of blocks B
stateblocks([], []) :- !.
stateblocks([cl(X)|S], [X|B]):-
    stateblocks(S, B), !.
stateblocks([on(X,Y)|S], [X,Y|B]):-
    stateblocks(S, B), !.


message_hook(exetime(Start, Stop), informational, _):-
  Time is (Stop-Start)/1000,
  write("Execution time : "),
  write(Time),
  writeln(" s").

message_hook(iteration(CurrentStep), informational, _):-
    write("Iteration "),
    writeln(CurrentStep).

message_hook(stackusage(Used), informational, _):-
    U is Used/1000000,
    write(U),
    writeln(" mb").

message_hook(partialQtime(Time1,Time2), informational, _):-
    Time is Time1+Time2,
    write("partialQ time : "),
    write(Time),
    writeln(" s").

message_hook(qtime(Time), informational, _):-
    write("       Q time : "),
    write(Time),
    writeln(" s").


without_last(WithLast, WithoutLast) :-
    length(WithLast, L),
    length(WithoutLast, L1),
    L1 is L-1,
    prefix(WithoutLast, WithLast).

message_hook(phistates(Phi, PhiStates), informational, _):-
  !.
  % Phi =.. Operation,
  % without_last(Operation, Output), nl, nl,
  % writeln("query: "), writeln(Output), nl,
  % writeln("answer: "), printall(PhiStates), nl, nl.
