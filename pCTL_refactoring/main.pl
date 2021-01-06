:- module(main, [evaluate/1]).
:- use_module(sorting).
:- use_module(util).
:- use_module(setting).
:- set_prolog_flag(stack_limit, 12000000000).
%:- set_prolog_flag(optimize,full).


memo(Step, Goal) :-
        (   recorded(Step,Goal)
        -> true
        ;   once(Goal),
            recorda(Step, Goal)
        ).

% Syntax checking -- Do not allow [].
% evaluate(Phi, []) :- functor(Phi, [], _), !.
% Phi is a set
evaluate(Phi) :-
    Phi =.. [states, _], !.
    % functor(Phi, '[|]', _), !.

% evaluate step-bounded until formula:
% PhiStates == P_{Op Threshold} [Phi1 U^{<= Steps} Phi2]
evaluate(Phi) :-
    Phi =.. [until, PhiStates, Steps, Phi1, Phi2, Op, Threshold],
    evaluate(Phi1), !, arg(1, Phi1, Phi1States),
    evaluate(Phi2), !, arg(1, Phi2, Phi2States),
    apply(until, [PhiStates, Steps, Phi1States, Phi2States, Op, Threshold]), !.
    % print_message(informational, phistates(Phi)).

% evaluate next formula:
% PhiStates == P_{Op Threshold} [X Phi2]
evaluate(Phi) :-
    Phi =.. [next, PhiStates, Phi2, Op, Threshold],
    evaluate(Phi2), !, arg(1, Phi2, Phi2States),
    apply(next, [PhiStates, Phi2States, Op, Threshold]), !.
    % print_message(informational, phistates(Phi)).

% evaluate and formula:
% PhiStates == Phi1 and Phi2
evaluate(Phi) :-
    Phi =.. [and, PhiStates, Phi1, Phi2],
    evaluate(Phi1), !, arg(1, Phi1, Phi1States),
    evaluate(Phi2), !, arg(1, Phi2, Phi2States),
    apply(and, [PhiStates, Phi1States, Phi2States]), !.
    % list_to_set1(PhiStates, SortedStates),
    % length(PhiStates, L1), length(SortedStates, L2),
    % print_message(informational, phistates(Phi)).


% get cartesian product of two state lists,
% Res is a list of combined states
and([], [], _) :- !.
and(Res, [E1|States1], States2):-
    maplist(andstate(E1), States2, PartialRes),
    delete(PartialRes, [], PartialRes1),
    append(PartialRes1, PhiStates, Res),
    and(PhiStates, States1, States2).

until(SortedStates, Steps, Phi1s, Phi2s, Op, Threshold) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    maplist(constructAbsorbingQs,Phi2s,Phi2sQs), !,
    vi(Steps, 1, InitV, _, Phi1s, Phi2sQs, FinalVs), !,
    % TODO combine filter and getVFStates to optimize
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
    !.

next(SortedStates, Phi2s, Op, Threshold) :-
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    vi(1, 1, InitV, _,[[]], [], FinalVs), !,
    filter(FinalVs, Op, Threshold, NewVN), !,
    getVFStates(NewVN, SortedStates),
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
    print_message(informational, iteration(CurrentStep)), nl,
    memo(CurrentStep, valueIteration_helper(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs)), !,
    print_message(informational, vf(CurrentVs)),
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
    % print_message(informational, vfWithAction(SPQs1)),
    getPartialQwp2(VFs1, Phi1s, SPQs2), !,
        %statistics(global_stack, [Used3,_]),
        %write("stack before combining partialQs : "),
        %print_message(informational, stackusage(Used3)),
    % printall(SPQs2), nl,nl,
    % print_message(informational, vfWithAction(SPQs2)),
        statistics(runtime, [Stop1|_]),
        print_message(informational, partialQs(SPQs1, SPQs2)),
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
    % print_message(informational, vfWithAction(QRules)),
    % print_message(informational, unfilteredQRules(UnfilteredQRules)),
    % print_message(informational, vfWithAction(QRules)),
    qTransfer(QRules, NewVFs),
    !.



% use backtracking to generate all Q rules from all possible
% combinations from partialQ1s and PartialQ2s
getQ(SPQs1, SPQs2, QruleExtra):-
    % try out all partialQ combinations from wp1 and wp2
    member(PartialQ1, SPQs1),
    member(PartialQ2, SPQs2),
    PartialQ1 = partialQ(_,A1,_,_),
    PartialQ2 = partialQ(_,A2,_,_),
    A1=A2,
    partialQstoQ(PartialQ1, PartialQ2, QruleExtra).
    % (
    % % QruleExtra = q(18.9,_,_,_)
    % true
    % ->
    % writeln(PartialQ1),
    % writeln(PartialQ2),
    % writeln(QruleExtra),nl
    % ;
    % true
    % % oi_qrule(Qrule).
    % ).


partialQstoQ(partialQ(Q1,A,S1,_), partialQ(Q2,A,S2,SS2),
             q(Q, A, S2, SS2)):-
    legalstate(S2),
    thetasubsumes(S1, S2), !,
    Q is Q1 + Q2, !.

partialQstoQ(partialQ(Q1,A,S1,SS1), partialQ(Q2,A,S2,_),
             q(Q, A, S1, SS1)):-
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

% TODO merge wp1 and wp2
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
wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState, partialQ(NewVFValue,A,S,VFState)):-
    % get a subH
    subsetgen(Head, SubH),
    % create all possible \theta
    structsubset(SubH, VFState, SubHp),
    % create VFSTail using \theta
    sort(VFState, VFStateTT), sort(SubHp, SubHpTT),
    ord_subtract(VFStateTT, SubHpTT, VFSTail),
    headbody(Head, VFSTail, Act, Body, Phi1s,
          partialQ(A,S)),
    oi_option(OI_option),
    oi_qrule(partialQ(_,A,S,VFState), OI_option),
    %%%% GET NEW VALUE %%%%
    discountfactor(Discount),
    append(Head, VFSTail, BBody),
    matchreward(A, RReward, S, BBody),
    % writeln(matchreward(A, RReward, S, BBody)),
    % writeln(RReward), nl,
    NewVFValue is Prob * (RReward + VFValue * Discount).
    % (
    % % NewVFValue = 18.9
    % true
    % ->
    %     % write(NewVFValue), write(" "), writeln(matchreward(A, RReward, S, BBody))
    %     writeln(partialQ(NewVFValue,A,S,VFState))
    %     ;
    %     true
    % ).
     %%%%            %%%%


headbody(Head, VFSTail, Act, Body, Phi1s,
        partialQ(Act, Glb)) :-
    member(Phi1, Phi1s), extract(Phi1), extract(VFSTail), extract(Body),
    getstate(GlbTT),
    subsumesort(GlbTT, Glb),
    sort(Head, HeadTT), sort(Body, BodyTT),
    ord_union(HeadTT, BodyTT, Lpp),
    cartesian_dif(VFSTail, Lpp).



%% TODO use maplist for this
qTransfer([],[]):- !.
qTransfer([q(Q,_,S,_)|Qs],[v(Q,S)|Vs]):-
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

    % writeln("=1======="),
    % printall(S0),writeln("."),
    % writeln(X),writeln("."),
    % printall(SortedS),
    % writeln("=2======="),

    fail
    ;
    arg(1, State, Results)
    ).


% assume List_Of_QRules is sorted
% Base case 1:
addQ([], New_QRule, [New_QRule]) :- !.
% Base case 2:
% if some QRule1 with Q1 >= Q subsumess New_QRule, discard New_QRule
addQ([q(Q1,A1,S1,SS1)|T0], q(Q,_,S,_), [q(Q1,A1,S1,SS1)|T0]) :-
    Q1 >= Q,
    thetasubsumes(S1,S), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
% and add New_QRule
addQ([q(Q1,_,S1,_)|T0], q(Q,A,S,SS), T) :-
    Q1 =< Q,
    thetasubsumes(S, S1), !,
    addQ(T0, q(Q,A,S,SS), T), !.

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
     addpartialQ(S0, X, S),
     sortByQValue(S, SortedS),
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
addpartialQ([partialQ(Q1,A1,S1,SS1)|T0], partialQ(Q,A,S,_), [partialQ(Q1,A1,S1,SS1)|T0]) :-
    Q1 >= Q,
    thetasubsumes([A1|S1],[A|S]),
    !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addpartialQ([partialQ(Q1,A1,S1,_)|T0], partialQ(Q,A,S,SS), T) :-
    Q1 =< Q,
    thetasubsumes([A|S], [A1|S1]),
    addpartialQ(T0, partialQ(Q,A,S,SS), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addpartialQ([PartialQ1|T0], New_partialQ, [PartialQ1|T]) :-
    addpartialQ(T0, New_partialQ, T), !.

legalaction(move(X,Y,Z)):-
  X\=Y, Y\=Z, Z\=X, !.
