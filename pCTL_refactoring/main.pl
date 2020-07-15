:- module(main, [evaluate/2]).
:- use_module(library(chr)).
:- use_module(precond).
:- use_module(sorting).
:- use_module(util).
:- set_prolog_flag(stack_limit, 12000000000).
:- use_module(chr(chr_runtime)).
:- use_module(setting).
%:- set_prolog_flag(optimize,full).


memo(Step, Goal) :-
        (   recorded(Step,Goal)
        -> true
        ;   once(Goal),
            recorda(Step, Goal)
        ).

experiment1 :-
    protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(1, [[]], [[on(a,b)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experimentX_iter_1 :-
    protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(next([[on(a,b)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experimentF_iter_1 :-
    protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(1, [[]], [[on(a,b)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experimentU_iter_1 :-
    protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(1, [[on(c,d)]],[[on(a,b)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.


experiment2 :-
    protocol('experiments/exp1_unbounded_until.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(100, [[on(c,d)]], [[on(a,b)]], >=, 0.9, Res), Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_inner1 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(1,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_inner2 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_inner3 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(3,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_inner4 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(4,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_inner5 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(5,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.


experiment5_outer1 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(1, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_outer2 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(2, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_outer3 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(3, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_outer4 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(4, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.

experiment5_outer5 :-
    protocol('experiments/exp5.txt'),
    statistics(runtime, [Start|_]),
    evaluate(
        until(5, [[cl(a)]],
            and(
                [[on(a,b)]],
                until(2,
                    next([[cl(e)]], >=, 0.9, _),
                    [[on(c,d)]],
                    >=, 0.9, _
                ),
                _
            ),
        >=, 0.5, Res),
    Res), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)),
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
    vi(Steps, 1, InitV, _, Phi1s, Phi2sQs, FinalVs), !,
    % TODO combine filter and getVFStates to optimize
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.

next(Phi2s, Op, Threshold, SortedStates) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    vi(1, 1, InitV, _,[[]], [], FinalVs), !,
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    %list_to_set1(States, SortedStates),
    !.

convergence([], [], _) :- !.
convergence([v(Q1, _)|PreviousVs], [v(Q2, _)|CurrentVs], ConvergenceThreshold) :-
    Residual is abs(Q1-Q2),
    Residual < ConvergenceThreshold,
    convergence(PreviousVs,CurrentVs,ConvergenceThreshold), !.

% % case 1: stop when the step bound is met
vi(TotalSteps, CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs, FinalVs):-
    TotalSteps =:= CurrentStep,
    valueIteration(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs), nl,
    FinalVs = CurrentVs, !.

vi(TotalSteps, CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs, FinalVs):-
    valueIteration(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs), nl,
    % case 2: stop when the value function converges TODO to be checked
    (
        PreviousStep is CurrentStep - 1,
        memo(PreviousStep, valueIteration_helper(PreviousStep, _, PreviousVs, _, _)),
        is_list(PreviousVs), is_list(CurrentVs),
        length(PreviousVs, L), length(CurrentVs, L),
        convergence_threshold(ConvergenceThreshold),
        convergence(PreviousVs, CurrentVs, ConvergenceThreshold)
    ->
        CurrentVs=FinalVs
    ;
    % case 3: calculate the new iteration
        NextStep is CurrentStep + 1,
        vi(TotalSteps, NextStep, InitV, _, Phi1s, Phi2sQs, FinalVs)
    ),
    !.


valueIteration(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs):-
    % RemoveMemoryIndex is CurrentStep - 2,
    % retractall(memo_(valueIteration_helper(RemoveMemoryIndex,_,_,_,_))),
    print_message(informational, iteration(CurrentStep)), nl,
    memo(CurrentStep, valueIteration_helper(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs)), !,
    printall(CurrentVs),
    length(CurrentVs, LLL),
    write("#abstract states: "), writeln(LLL),
    nl, nl,
    !.

valueIteration_helper(0, InitV, InitV, _, _):-!.
valueIteration_helper(CurrentStep, InitV, CurrentV, Phi1s, Phi2sQs):-
    PreviousStep is CurrentStep-1,
    memo(PreviousStep, valueIteration_helper(PreviousStep, InitV, PreviousV, Phi1s, Phi2sQs)),
    oneIteration(PreviousV, CurrentV, Phi1s, Phi2sQs),
    !.

% deterministic acrtions
% oneIteration(VFs, NewVFs, Phi1s, Phi2sQs):-
%     nonDetActions(det), !,
%     append(VFs, [v(0.0, [])], VFs1),
%         statistics(runtime, [Start1|_]),
%     getPartialQwp1Det(VFs1, Phi2sQs, Phi1s, SPQs1), !,
%         length(SPQs1, LSPQs1),
%         write("partialQs: "), writeln([LSPQs1]),
%         statistics(runtime, [Stop1|_]),
%         print_message(informational, partialQtime(Start1, Stop1)),
%     %%% step 2: combining, producing Q rules
%         statistics(runtime, [Start2|_]),
%     %list_to_set1(SPQs1, QRules), !,
%         statistics(runtime, [Stop2|_]),
%         print_message(informational, qtime(Start2,Stop2)),
%     qTransfer(SPQs1, NewVFs),
%     !.

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
    % printall(SPQs1), nl,nl,
    getPartialQwp2(VFs1, Phi1s, SPQs2), !,
        %statistics(global_stack, [Used3,_]),
        %write("stack before combining partialQs : "),
        %print_message(informational, stackusage(Used3)),
    % printsp(SPQs2), nl,nl,
        length(SPQs1, LSPQs1), length(SPQs2, LSPQs2),
        write("partialQs: "), writeln([LSPQs1, LSPQs2]),
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
    % printall(QRules), nl, nl,
    qTransfer(QRules, NewVFs),
    !.

% use backtracking to generate all Q rules from all possible
% combinations from partialQ1s and PartialQ2s
getQ(SPQs1, SPQs2, Qrule):-
    % try out all partialQ combinations from wp1 and wp2
    member(PartialQ1, SPQs1),
    member(PartialQ2, SPQs2),
    PartialQ1 = partialQ(_,A1,_),
    PartialQ2 = partialQ(_,A2,_),
    % Q1 > 0,
    A1=A2,
    partialQstoQ(PartialQ1, PartialQ2, Qrule).


partialQstoQ(partialQ(Q1,A,S1), partialQ(Q2,A,S2),
             q(Q, A, S2)):-
    legalstate(S2),
    thetasubsumes(S1, S2), !,
    Q is Q1 + Q2, !.

partialQstoQ(partialQ(Q1,A,S1), partialQ(Q2,A,S2),
             q(Q, A, S1)):-
    legalstate(S1),
    thetasubsumes(S2, S1), !,
    Q is Q1 + Q2, !.

% getPartialQwp1Det(VFs, Phi2sQs, Phi1s, SPQs1):-
%     garbage_collect,
%     findall_partialQsDet(PQ1, Phi2sQs, wp1(VFs,Phi1s,PQ1), SPQs1), !,
%     garbage_collect,
%     !.

getPartialQwp1(VFs, Phi1s, SPQs1):-
    % garbage_collect,
    findall_partialQs(PQ1, wp1(VFs,Phi1s,PQ1), SPQs1), !,
    % garbage_collect,
    !.

getPartialQwp2(VFs, Phi1s, SPQs2):-
    % garbage_collect,
    findall_partialQs(PQ2, wp2(VFs,Phi1s,PQ2), SPQs2), !,
    % garbage_collect,
    !.


%%

% wp1_det(VFs, Phi1s, PQ) :-
%     member(v(VFValue, VFState), VFs),
%     VFValue > 0,
%     mydif(X,Y), mydif(Y,Z), mydif(X,Z),
%     wpi([cl(X), cl(Z), on(X,Y)], 1.0, move(X,Y,Z), [cl(X), cl(Y), on(X,Z)],
%         Phi1s, VFValue, VFState, PQ).

wp1(VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    % VFValue > 0,
    transition(Action, 1, Prob, Head_i, Body),
    wpi(Head_i, Prob, Action, Body, Phi1s, VFValue, VFState, PQ).

wp2(VFs, Phi1s, PQ) :-
    member(v(VFValue, VFState), VFs),
    transition(Action, 2, Prob, Head_i, Body),
    wpi(Head_i, Prob, Action, Body, Phi1s, VFValue, VFState, PQ).


% Takes an action rule "ActionHead <----Prob:[Act]---- ActionBody"
% and a value function "VFValue <----- VFState"
%%%%% Step 1 : get weakest precondition
wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState, partialQ(Q,A,S)):-
    % get a subH
    subsetgen(Head, SubH),
    % create all possible \theta
    structsubset(SubH, VFState, SubHp),
    % create VFSTail using \theta
    sort(VFState, VFStateTT), sort(SubHp, SubHpTT),
    ord_subtract(VFStateTT, SubHpTT, VFSTail),
    headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
             partialQ(Q,A,S))
    .

headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
        partialQ(NewVFValue, Act, Glb)) :-
    member(Phi1, Phi1s), extract(Phi1), extract(VFSTail), extract(Body),
    getallstuff(GlbTT),
    subsumesort(GlbTT, Glb),
    sort(Head, HeadTT), sort(Body, BodyTT),
    ord_union(HeadTT, BodyTT, Lpp),
    cartesian_dif(VFSTail, Lpp),
    %========================
    % blocks_limit(B),
    % oi(Glb, B),
    %========================
    discountfactor(Discount),
    NewVFValue is Prob * VFValue * Discount.


%%
qTransfer([],[]):- !.
qTransfer([q(Q,_,S)|Qs],[v(Q,S)|Vs]):-
    qTransfer(Qs, Vs), !.


%%
findall_Qrules(X, InitQs, Goal, Results) :-
    State = state(InitQs),
    (
    Goal,
    arg(1, State, S0),
    addQ(S0, X, S),
    sortByQValue(S, SortedS), % OPTIMIZE
    nb_setarg(1, State, SortedS),
    fail
    ;
    arg(1, State, Results)
    ).


% assume List_Of_QRules is sorted
% Base case 1:
addQ([], New_QRule, [New_QRule]) :- !.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addQ([q(Q1,A1,S1)|T0], q(Q,_,S), [q(Q1,A1,S1)|T0]) :-
    Q1 >= Q,
    thetasubsumes(S1,S), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
% and add New_QRule
addQ([q(Q1,_,S1)|T0], q(Q,A,S), T) :-
    Q1 =< Q,
    thetasubsumes(S, S1), !,
    addQ(T0, q(Q,A,S), T), !.

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
     % addpartialQ(S0, X, S),
     % sortByQValue(S, SortedS),
     sortByQValue([X|S0], SortedS),
     nb_setarg(1, State, SortedS),
     fail
  ;
     arg(1, State, Results)
  ).

% assume List_Of_QRules is sorted
% Base case 1:
addpartialQ([], New_partialQ, [New_partialQ]) :- !.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addpartialQ([partialQ(Q1,A1,S1)|T0], partialQ(Q,A,S), [partialQ(Q1,A1,S1)|T0]) :-
    Q1 == Q,
    thetasubsumes([A1|S1],[A|S]),
    % (
    %     Q = 0.0729
    % ->
    %     % writeln("discarded: "),
    %     writeln(partialQ(Q,A,S))
    % ; true
    % ),
    !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addpartialQ([partialQ(Q1,A1,S1)|T0], partialQ(Q,A,S), T) :-
    Q1 =< Q,
    thetasubsumes([A|S], [A1|S1]),
    addpartialQ(T0, partialQ(Q,A,S), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addpartialQ([PartialQ1|T0], New_partialQ, [PartialQ1|T]) :-
    addpartialQ(T0, New_partialQ, T), !.

legalaction(move(X,Y,Z)):-
  X\=Y, Y\=Z, Z\=X, !.