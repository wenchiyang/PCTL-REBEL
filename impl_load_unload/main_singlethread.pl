:- use_module(library(chr)).
:- use_module(precond).
:- use_module(sorting).
:- use_module(util).
:- set_prolog_flag(stack_limit, 10 000 000 000).
:- use_module(chr(chr_runtime)).
:- use_module(setting).
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
    protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(100, [[]], [[bin(b1,paris)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).
    noprotocol.



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
evaluate(Phi, PhiStates) :-
    Phi =.. [and, Phi1, Phi2, PhiStates],
    evaluate(Phi1, Phi1States), !,
    evaluate(Phi2, Phi2States), !,
    apply(and, [Phi1States, Phi2States, PhiStates]), !,
    list_to_set1(PhiStates, SortedStates),
    length(PhiStates, L1), length(SortedStates, L2), writeln([L1,L2]),
    print_message(informational, phistates(Phi, SortedStates)).


% get cartesian product of two state lists,
% Res is a list of combined states
and([], _, []):-!.
and([E1|States1], States2, Res):-
    maplist(andstate(E1), States2, PartialRes),
    delete(PartialRes, [], PartialRes1),
    append(PartialRes1, PhiStates, Res),
    and(States1, States2, PhiStates).

until(Steps, Phi1s, Phi2s, Op, Threshold, SortedStates) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    maplist(constructAbsorbingQs,Phi2s,Phi2sQs), !,
    vi(Steps, 1, InitV, _, [], Phi1s, Phi2sQs, FinalVs), !,
    % TODO combine filter and getVFStates to optimize
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.

next(Phi2s, Op, Threshold, SortedStates) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    vi(1, 1, InitV, _, [], [[]], [], FinalVs), !, % Need to be tested
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.

convergence([], [], _) :- !.
convergence([v(Q1, _)|PreviousVs], [v(Q2, _)|CurrentVs], ConvergenceThreshold) :-
    Residual is abs(Q1-Q2),
    Residual < ConvergenceThreshold,
    convergence(PreviousVs,CurrentVs,ConvergenceThreshold), !.


% base case 1: when the step bound is met
vi(TotalSteps, CurrentStep, _, _, FinalVS, _, _, FinalVS):-
    CurrentStep =:= TotalSteps + 1, !.

vi(TotalSteps, CurrentStep, InitV, CurrentVs, PreviousVs, Phi1s, Phi2sQs, FinalVs):-
    valueIteration(TotalSteps, CurrentStep, InitV, [CurrentVs,PreviousVs], Phi1s, Phi2sQs, FinalVs), !.

valueIteration(TotalSteps, CurrentStep, InitV, Vs, Phi1s, Phi2sQs, FinalVs):-
    RemoveMemoryIndex is CurrentStep - 2,
    retractall(memo_(valueIteration_helper(RemoveMemoryIndex,_,_,_,_))),
    print_message(informational, iteration(CurrentStep)), nl,
    memo(valueIteration_helper(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs)), !,
    Vs = [CurrentVs,PreviousVs],
    % printall_format(CurrentVs),
    length(CurrentVs, LLL), write("#abstract states: "), writeln(LLL),
    nl, nl,
    (
    is_list(PreviousVs),
    is_list(CurrentVs),
    length(PreviousVs, L1),
    length(CurrentVs, L2),
    L1 == L2,
    convergence_threshold(ConvergenceThreshold),
    convergence(PreviousVs,CurrentVs,ConvergenceThreshold)
    ->
        CurrentVs=FinalVs, !
    ;
    NextStep is CurrentStep + 1,
    vi(TotalSteps, NextStep, InitV, _, CurrentVs, Phi1s, Phi2sQs, FinalVs),
    !
    ), !.
%
valueIteration_helper(0, InitV, InitV, _, _):-!.
valueIteration_helper(CurrentStep, InitV, CurrentV, Phi1s, Phi2sQs):-
    PreviousStep is CurrentStep-1,
    memo(valueIteration_helper(PreviousStep, InitV, PreviousV, Phi1s, Phi2sQs)),
    oneIteration(PreviousV, CurrentV, Phi1s, Phi2sQs),
    !.

oneIteration(VFs, NewVFs, Phi1s, Phi2sQs):-
    nonDetActions(det), !,
    append(VFs, [v(0.0, [])], VFs1),
        statistics(runtime, [Start1|_]),
    getPartialQwp1Det(VFs1, Phi2sQs, Phi1s, SPQs1), !,
        length(SPQs1, LSPQs1),
        write("partialQs: "), writeln([LSPQs1]),
    %printall_format(SPQs1), nl,
        statistics(runtime, [Stop1|_]),
        print_message(informational, partialQtime(Start1, Stop1)),
    %%% step 2: combining, producing Q rules
        statistics(runtime, [Start2|_]),
    %list_to_set1(SPQs1, QRules), !,
        statistics(runtime, [Stop2|_]),
        print_message(informational, qtime(Start2,Stop2)),
    %printall(SPQs1),
    qTransfer(SPQs1, NewVFs),
    !.

oneIteration(VFs, NewVFs, Phi1s, Phi2sQs):-
    nonDetActions(nondet), !,
    append(VFs, [v(0.0, [])], VFs1),
    %%% step 1: regression, producing partial Q rules
        statistics(runtime, [Start1|_]),
        %statistics(global_stack, [Used1,_]),
        %write("stack before partialQ1 : "),
        %print_message(informational, stackusage(Used1)),
    getPartialQwp1(VFs1, Phi1s, SPQs1), !,
        %statistics(global_stack, [Used2,_]),
        %write("stack before partialQ2 : "),
        %print_message(informational, stackusage(Used2)),
    getPartialQwp2(VFs1, Phi1s, SPQs2), !,
        %statistics(global_stack, [Used3,_]),
        %write("stack before combining partialQs : "),
        %print_message(informational, stackusage(Used3)),
        length(SPQs1, LSPQs1), length(SPQs2, LSPQs2),
        write("partialQs: "), writeln([LSPQs1, LSPQs2]),
    %printall_format(SPQs1), nl,
    %printall_format(SPQs2), nl,
        statistics(runtime, [Stop1|_]),
        print_message(informational, partialQtime(Start1, Stop1)),
        %write("    step 1 : "), write(Step1), writeln(" s"),
    %%% step 2: combining, producing Q rules
        statistics(runtime, [Start2|_]),
    findall_Qrules(Q, Phi2sQs, getQ(SPQs1, SPQs2, Q), QRules), !,
    %findall(Q, getQ(SPQs1, SPQs2, Q), QRules), !,
        statistics(runtime, [Stop2|_]),
        %write("    step 2 : "), write(Step2), writeln(" s"),
        %statistics(global_stack, [Used4,_]),
        %write("stack after combining partialQs : "),
        %print_message(informational, stackusage(Used4)),
        print_message(informational, qtime(Start2,Stop2)),
        %garbage_collect,
    %%% step 3: filtering, producing value functions
        %statistics(runtime, [Start3|_]),
    %printall_format(QRules),
    qTransfer(QRules, NewVFs),
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
    A1=A2,
    legalaction(A1),
    partialQstoQ(PartialQ1, PartialQ2, Q).

partialQstoQ(partialQ(Q1,_,_,_,_,Sta1),
             partialQ(Q2,_,S2,Size2,Str2,Sta2),
             q(Q, S2, Size2, Str2, Sta2)):-
    subsumess(Sta1,Sta2),
    legalstate(S2, Size2), !,
    Q is Q1 + Q2, !.

partialQstoQ(partialQ(Q1,_,S1,Size1,Str1,Sta1),
             partialQ(Q2,_,_,_,_,Sta2),
             q(Q, S1,Size1,Str1,Sta1)):-
    subsumess(Sta2,Sta1),
    legalstate(S1, Size1), !,
    Q is Q1 + Q2, !.

getPartialQwp1Det(VFs, Phi2sQs, Phi1s, SPQs1):-
    garbage_collect,
    findall_partialQsDet(PQ1, Phi2sQs, wp1(_,VFs,Phi1s,PQ1), SPQs1), !,
    garbage_collect,
    !.

getPartialQwp1(VFs, Phi1s, SPQs1):-
    garbage_collect,
    findall_partialQs(PQ1, wp1(_,VFs,Phi1s,PQ1), SPQs1), !,
    garbage_collect,
    !.

getPartialQwp2(VFs, Phi1s, SPQs2):-
    garbage_collect,
    findall_partialQs(PQ2, wp2(_,VFs,Phi1s,PQ2), SPQs2), !,
    garbage_collect,
    !.

% read the setting: det or nondet actions
wp1(A, VFs, Phi1s, PQ):-
    nonDetActions(nondet), !,
    wp1_nondet(A, VFs, Phi1s, PQ).

wp1(A, VFs, Phi1s, PQ):-
    nonDetActions(det), !,
    wp1_det(A, VFs, Phi1s, PQ).

wp2(A, VFs, Phi1s, PQ):-
    nonDetActions(nondet), !,
    wp2_nondet(A, VFs, Phi1s, PQ).

%%
wp1_det(unload, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([bin(B,C), tin(T,C)], 1.0, unload(B,T), [tin(T,C), on(B,T)],
        Phi1s, VFValue, VFState, PQ).


wp1_det(load, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([tin(T,C), on(B,T)], 1.0, load(B,T), [bin(B,C), tin(T,C)],
        Phi1s, VFValue, VFState, PQ).

wp1_det(drive, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(C1,C2), mydif(T,C1), mydif(T,C2),
    highway(C1,C2),
    wpi([tin(T,C1)], 1.0, drive(T,C1), [tin(T,C2)],
        Phi1s, VFValue, VFState, PQ).


wp1_nondet(unload, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([bin(B,C), tin(T,C)], 0.9, unload(B,T), [tin(T,C), on(B,T)],
        Phi1s, VFValue, VFState, PQ).


wp1_nondet(load, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([tin(T,C), on(B,T)], 0.9, load(B,T), [bin(B,C), tin(T,C)],
        Phi1s, VFValue, VFState, PQ).

wp1_nondet(drive, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(C1,C2), mydif(T,C1), mydif(T,C2),
    highway(C1,C2),
    wpi([tin(T,C1)], 1.0, drive(T,C1), [tin(T,C2)],
        Phi1s, VFValue, VFState, PQ).

wp2_nondet(unload, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([tin(T,C), on(B,T)], 0.1, unload(B,T), [tin(T,C), on(B,T)],
        Phi1s, VFValue, VFState, PQ).

wp2_nondet(load, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C), mydif(T,B), mydif(C,B),
    wpi([bin(B,C), tin(T,C)], 0.1, load(B,T), [bin(B,C), tin(T,C)],
        Phi1s, VFValue, VFState, PQ).

wp2_nondet(drive, VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    mydif(T,C1), %mydif(C1, C2), mydif(T,C2),
    wpi([], 0.0, drive(T,C1), [],
        Phi1s, VFValue, VFState, PQ).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

% Takes an action rule "ActionHead <----Prob:[Act]---- ActionBody"
% and a value function "VFValue <----- VFState"
wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState,
    partialQ(Q,Act,S,Size,Str,Sta)):-
    % get a subH
    subsetgen(Head, SubH),
    % get SubVFState
    predInList(SubH, bin, BinSubH),
    predInList(SubH, tin, TinSubH),
    predInList(SubH, on, OnSubH),
    %length(ClSubH, LClSubH), length(OnSubH, LOnSubH),
    %% SubVFState=[ClSpp, OnSpp] and has structure [LClSubH, LOnSubH]
    structsubset(SubH, VFState, BinSpp, TinSpp, OnSpp), %%%% This can be optimized!!!!!
    permutation(BinSubH, BinSubHp),
    BinSubHp = BinSpp,
    permutation(TinSubH, TinSubHp),
    TinSubHp = TinSpp,
    permutation(OnSubH, OnSubHp),
    OnSubHp = OnSpp,
    flatten([BinSubHp, TinSubHp, OnSubHp], SubHp),
    % if \theta exists
    sort(VFState, VFStateTT), sort(SubHp, SubHpTT),
    ord_subtract(VFStateTT, SubHpTT, VFSTail),
    headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
             partialQ(Q,Act,S,Size,Str,Sta)).

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
    cartesian_dif(VFSTail, Lpp),
    discountfactor(Discount),
    NewVFValue is Prob * VFValue * Discount.


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



% assume List_Of_QRules is sorted
% Base case 1:
addQ([], New_QRule, [New_QRule]):-!.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addQ([q(Q1,S1,Size1,Str1,Sta1)|T0],
      q(Q,_,_,_,Sta),
     [q(Q1,S1,Size1,Str1,Sta1)|T0]) :-
        Q1 >= Q,
        subsumess(Sta1,Sta), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addQ([q(Q1,_,_,_,Sta1)|T0],
     q(Q,S,Size,Str,Sta),
     T) :-
    Q1 =< Q,
    subsumess(Sta,Sta1), !,
    addQ(T0, q(Q,S,Size,Str,Sta), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addQ([QRule1|T0], New_QRule, [QRule1|T]) :-
    addQ(T0, New_QRule, T), !.


findall_partialQsDet(X, InitQs, Goal, Results) :-
    State = state(InitQs),
    (  Goal,
       arg(1, State, S0),

       addpartialQ(S0, X, S),

       sortByQValue(S, SortedS),
       nb_setarg(1, State, SortedS),
       fail
    ;
       arg(1, State, Results)
    ).

% assume List_Of_QRules is sorted
% Base case 1:
addpartialQ([], partialQ(Q,_,S,Size,Str,Sta), [q(Q,S,Size,Str,Sta)]):-!.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addpartialQ([q(Q1,S1,Size1,Str1,Sta1)|T0],
      partialQ(Q,_,_,Size,_,Sta),
     [q(Q1,S1,Size1,Str1,Sta1)|T0]) :-
        Q1 >= Q,
        subsumess(Size1,Str1,Size,Sta), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addpartialQ([q(Q1,_,Size1,_,Sta1)|T0],
     partialQ(Q,_,S,Size,Str,Sta),
     T) :-
    Q1 =< Q,
    subsumess(Size,Str,Size1,Sta1), !,
    addpartialQ(T0, partialQ(Q,_,S,Size,Str,Sta), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addpartialQ([QRule1|T0], New_QRule, [QRule1|T]) :-
    addpartialQ(T0, New_QRule, T), !.


%%
%%
%% findall_partialQs(PQ1, wp1(VFs,Phi1s,PQ1), PQs1U)
findall_partialQs(X, Goal, Results) :-
  State = state([]),
  (  Goal,
     arg(1, State, S0),
     nb_setarg(1, State, [X|S0]),
     fail
  ;
    arg(1, State, Reverse),
    reverse(Reverse, Results)
  ).


legalaction(load(_,_)).
legalaction(unload(_,_)).
legalaction(drive(_,_)).





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

message_hook(partialQtime(Start,Stop), informational, _):-
    Time is (Stop-Start)/1000,
    write("partialQ time : "),
    write(Time),
    writeln(" s").

message_hook(qtime(Start,Stop), informational, _):-
    Time is (Stop-Start)/1000,
    write("       Q time : "),
    write(Time),
    writeln(" s").


without_last(WithLast, WithoutLast) :-
    length(WithLast, L),
    length(WithoutLast, L1),
    L1 is L-1,
    prefix(WithoutLast, WithLast).

message_hook(phistates(_, _), informational, _):-!.
%message_hook(phistates(Phi, PhiStates), informational, _):-!.
  % Phi =.. Operation,
  % without_last(Operation, Output), nl, nl,
  % writeln("query: "), writeln(Output), nl,
  % writeln("answer: "), printall(PhiStates), nl, nl.
