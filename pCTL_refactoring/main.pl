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

%until(SortedStates, Steps, Phi1s, Phi2s, Op, Threshold) :-
until(NewVN, Steps, Phi1s, Phi2s, Op, Threshold) :- %TODO To get probabilities
    maplist(constructAbsorbingVFs,Phi2s,InitV), !,
    maplist(constructAbsorbingQs,Phi2s,Phi2sQs), !,
    vi(Steps, 1, InitV, _, Phi1s, Phi2sQs, FinalVs), !,
    % TODO combine filter and getVFStates to optimize
    filter(FinalVs, Op, Threshold, NewVN), !,
%    getVFStates(NewVN, SortedStates),
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
%    print_message(informational, iteration(CurrentStep)), nl,
    memo(CurrentStep, valueIteration_helper(CurrentStep, InitV, CurrentVs, Phi1s, Phi2sQs)), !,
%    print_message(informational, vf(CurrentVs)),
    !.

valueIteration_helper(0, InitV, InitV, _, _):-!.
valueIteration_helper(CurrentStep, InitV, CurrentV, Phi1s, Phi2sQs):-
    PreviousStep is CurrentStep-1,
    memo(PreviousStep, valueIteration_helper(PreviousStep, InitV, PreviousV, Phi1s, Phi2sQs)),
%    writeln(PreviousStep),
%    writeln(PreviousV),
    (
    1 is mod(CurrentStep, 2) %TODO: number of ghost + 1
    ->
    oneIteration(PreviousV, CurrentV, Phi1s, Phi2sQs, [pacman_move, [1,2]], QRules),!
    ;
    oneIteration(PreviousV, CurrentV, Phi1s, Phi2sQs, [ghost_move, [1,2,3,4,5]], QRules),!
    ),
    (
    CurrentStep == 3
    ->
    print_message(informational, vfWithAction(QRules))
    ;
    true
    )
    .


oneIteration(VFs, NewVFs, Phi1s, Phi2sQs, [ActionTurn, NumI], QRules):-
    append(VFs, [v(0.0, [])], VFs1),
    %%% step 1: regression, producing partial Q rules
%        statistics(runtime, [Start1|_]),
        %statistics(global_stack, [Used1,_]),
        %write("stack before partialQ1 : "),
        %print_message(informational, stackusage(Used1)),
        %statistics(global_stack, [Used2,_]),
        %write("stack before partialQ2 : "),
        %print_message(informational, stackusage(Used2)),
    getPartialQs(VFs1, Phi1s, SPQs, [ActionTurn, NumI]), !,
        %statistics(global_stack, [Used3,_]),
        %write("stack before combining partialQs : "),
        %print_message(informational, stackusage(Used3)),
%        statistics(runtime, [Stop1|_]),
%        print_message(informational, partialQs(SPQs)),
%        print_message(informational, partialQtime(Start1, Stop1)),
        %write("    step 1 : "), write(Step1), writeln(" s"),
    %%% step 2: combining, producing Q rules
%        statistics(runtime, [Start2|_]),
    findall_Qrules(Q, Phi2sQs, getQ(SPQs, Q), QRules), !,
    %findall(Q, getQ(SPQs1, SPQs2, Q), QRules), !,
%        statistics(runtime, [Stop2|_]),
        %write("    step 2 : "), write(Step2), writeln(" s"),
        %statistics(global_stack, [Used4,_]),
        %write("stack after combining partialQs : "),
        %print_message(informational, stackusage(Used4)),
%        print_message(informational, qtime(Start2,Stop2)),
        %garbage_collect,
    %%% step 3: filtering, producing value functions
        %statistics(runtime, [Start3|_]),
    % printall(QRules), nl, nl,
    % print_message(informational, vfWithAction(QRules)),
    % print_message(informational, unfilteredQRules(UnfilteredQRules)),
%    print_message(informational, vfWithAction(QRules)),
    qTransfer(QRules, NewVFs),
    !.



% use backtracking to generate all Q rules from all possible
% combinations from partialQ1s and PartialQ2s
getQ(SPQs, QruleExtra):-
%    SPQs = [SPQs1, SPQs2],
%    member(PartialQ1, SPQs1),
%    member(PartialQ2, SPQs2),
%    PartialQ1 = partialQ(Q1,A1,_,_),
%    PartialQ2 = partialQ(_,A2,_,_),
%    Q1 > 0,
%    A1=A2,
    maplist(member, PartialQs, SPQs),
    maplist([X]>>arg(2,X), PartialQs, As),
    As = [AHead|ABody],
    maplist(=(AHead),ABody),
%    printall(PartialQs),nl,
    partialQstoQ(PartialQs, QruleExtra),
    QruleExtra = q(_, AHead, _, _).
    % oi_qrule(Qrule).


% Choose the most specific one
partialQstoQ(PartialQs, q(Q, _, MostSpecS, MostSpecSS)):-
    maplist([X,[S,SS]]>>(arg(3,X,S), arg(4,X,SS)), PartialQs, SPrevS),
    select([MostSpecS, MostSpecSS], SPrevS, RestS),
    legalstate(MostSpecS),
    length(RestS, LRestS), length(ManyMostSpecS, LRestS), maplist(=(MostSpecS), ManyMostSpecS),
%    writeln(MostSpecS),printall(RestS), nl,
    maplist([X,Y]>>(Y = [General|_], thetasubsumes_number(General,X)), ManyMostSpecS, RestS), !,
%    writeln(MostSpecS), nl,
    maplist([X]>>arg(1,X), PartialQs, Qs),
    sum_list(Qs, Q),
    !.
%    PartialQs = [P1,P2],
%    writeln(P1), writeln(P2), nl.
%    Q is Q1 + Q2, !.


%partialQstoQ(PartialQs, q(Q, A, S2, SS2)):-
%    PartialQs = [partialQ(Q1,A,S1,SS1), partialQ(Q2,A,S2,SS2)],
%    legalstate(S2),
%    thetasubsumes_number(S1, S2), !,
%    Q is Q1 + Q2, !,
%    writeln(partialQ(Q1,A,S1,SS1)), writeln(partialQ(Q2,A,S2,SS2)), nl.
%
%partialQstoQ(PartialQs, q(Q, A, S1, SS1)):-
%    PartialQs = [partialQ(Q1,A,S1,SS1), partialQ(Q2,A,S2,SS2)],
%    legalstate(S1),
%    thetasubsumes_number(S2, S1), !,
%    Q is Q1 + Q2, !,
%    writeln(partialQ(Q1,A,S1,SS1)), writeln(partialQ(Q2,A,S2,SS2)), nl.


%partialQstoQ(partialQ(Q1,A,S1,_), partialQ(Q2,A,S2,SS2),
%             q(Q, A, S2, SS2)):-
%    legalstate(S2),
%    thetasubsumes_number(S1, S2), !,
%    Q is Q1 + Q2, !.
%
%partialQstoQ(partialQ(Q1,A,S1,SS1), partialQ(Q2,A,S2,_),
%             q(Q, A, S1, SS1)):-
%    legalstate(S1),
%    thetasubsumes_number(S2, S1), !,
%    Q is Q1 + Q2, !.



% get partialQ for wp1, wp2, ..., wpi
getPartialQs(VFs, Phi1s, SPQs, [ActionTurn, NumI]):-
%    findall_partialQs(PQ1, wps(VFs,Phi1s,PQ1,1), SPQs1),
%    findall_partialQs(PQ1, wps(VFs,Phi1s,PQ1,2), SPQs2),
%    maplist(findall_partialQs, [PQ1,PQ1], [wps(VFs,Phi1s,PQ1,1), wps(VFs,Phi1s,PQ1,2)], SPQs),
%    writeln("dddw"),
    maplist([I]>>findall_partialQs(PQ1,wps(VFs,Phi1s,PQ1,I,ActionTurn)), NumI, SPQs),
%    maplist([X] >> (printall(X), nl), SPQs),
    !.

%getPartialQwp1(VFs, Phi1s, SPQs1):-
%    % garbage_collect,
%    findall_partialQs(PQ1, wp1(VFs,Phi1s,PQ1), SPQs1),
%    % garbage_collect,
%    !.

%getPartialQwp2(VFs, Phi1s, SPQs2):-
%    % garbage_collect,
%    findall_partialQs(PQ2, wp2(VFs,Phi1s,PQ2), SPQs2),
%    % garbage_collect,
%    !.


%%

% wp1_det(VFs, Phi1s, PQ) :-
%     member(v(VFValue, VFState), VFs),
%     VFValue > 0,
%     mydif(X,Y), mydif(Y,Z), mydif(X,Z),
%     wpi([cl(X), cl(Z), on(X,Y)], 1.0, move(X,Y,Z), [cl(X), cl(Y), on(X,Z)],
%         Phi1s, VFValue, VFState, PQ).


wps(VFs, Phi1s, PQ, I, ActionTurn) :-
    member(v(VFValue, VFState), VFs),
    % VFValue > 0,
    transition(Action, I, Prob, Head_i, Body),
    Action =.. [ActionTurn|_],
%    writeln(transition(Action, I, Prob, Head_i, Body)),
    wpi(Head_i, Prob, Action, Body, Phi1s, VFValue, VFState, PQ).
%    writeln(PQ).


%wp1(VFs, Phi1s, PQ) :-
%    member(v(VFValue, VFState), VFs),
%    % VFValue > 0,
%    transition(Action, 1, Prob, Head_i, Body),
%    wpi(Head_i, Prob, Action, Body, Phi1s, VFValue, VFState, PQ).
%
%wp2(VFs, Phi1s, PQ) :-
%    member(v(VFValue, VFState), VFs),
%    transition(Action, 2, Prob, Head_i, Body),
%    wpi(Head_i, Prob, Action, Body, Phi1s, VFValue, VFState, PQ).


% Takes an action rule "ActionHead <----Prob:[Act]---- ActionBody"
% and a value function "VFValue <----- VFState"
%%%%% Step 1 : get weakest precondition
wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState, partialQ(Q,A,S,VFState)):-
%     writeln(wpi(Head, Prob, Act, Body, Phi1s, VFValue, VFState)),
     % get a subH
     subsetgen(Head, SubH), %TODO only for pacman
%     (Head = SubH; SubH=[]),
%     writeln(1),
     % create all possible \theta
     structsubset(SubH, VFState, SubHp),
%     writeln(SubHp),
     % create VFSTail using \theta
     sort(VFState, VFStateTT), sort(SubHp, SubHpTT),
     ord_subtract(VFStateTT, SubHpTT, VFSTail),
%     writeln(3),
     headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
              partialQ(Q,A,S)),
%     writeln([partialQ(Q,A,S), SubH]),
     oi_option(OI_option),
     oi_qrule(partialQ(Q,A,S,VFState), OI_option).


headbody(Head, VFValue, VFSTail, Prob, Act, Body, Phi1s,
        partialQ(NewVFValue, Act, Glb)) :-
%        writeln(4.5),
    member(Phi1, Phi1s), extract(Phi1), extract(VFSTail), extract(Body),
%    writeln([Phi1, VFSTail, Body]),
%    writeln(4.7),
    getstate(GlbTT),
%    writeln(5),
    subsumesort(GlbTT, Glb),
%    writeln(6),
    sort(Head, HeadTT), sort(Body, BodyTT),
%    writeln(7),
    ord_union(HeadTT, BodyTT, Lpp),
    cartesian_dif(VFSTail, Lpp),
    discountfactor(Discount),
    NewVFValue is Prob * VFValue * Discount.


%% TODO use maplist for this
qTransfer([],[]):- !.
qTransfer([q(Q,_,S,_)|Qs],[v(Q,S)|Vs]):-
    qTransfer(Qs, Vs), !.


%%
findall_Qrules(X, InitQs, Goal, Results) :-
    State = state(InitQs),
%    State = state([]),
    (
    Goal,
    arg(1, State, S0),
    % writeln(X),
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
addQ([q(Q1,A1,S1,SS1)|T0], q(Q,_,S,_), [q(Q1,A1,S1,SS1)|T0]) :-
    Q1 >= Q,
    thetasubsumes_number(S1,S), !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
% and add New_QRule
addQ([q(Q1,_,S1,_)|T0], q(Q,A,S,SS), T) :-
    Q1 =< Q,
    thetasubsumes_number(S, S1), !,
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
%     writeln(Goal),
     arg(1, State, S0),
%     writeln(addpartialQ(S0, X, S)),
     addpartialQ(S0, X, S),
%     writeln(addpartialQ(S0, X, S)),
     sortByQValue(S, SortedS),
%     writeln(sortByQValue(S, SortedS)),
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
%    writeln(thetasubsumes_number([A1|S1],[A|S])),
    thetasubsumes_number([A1|S1],[A|S]),
    !.

% if New_QRule subsumess QRule1 with Q1 =< Q, discard QRule1
addpartialQ([partialQ(Q1,A1,S1,_)|T0], partialQ(Q,A,S,SS), T) :-
    Q1 =< Q,
    thetasubsumes_number([A|S], [A1|S1]),
    addpartialQ(T0, partialQ(Q,A,S,SS), T), !.

% if New_QRule and QRule1 do not subsumess each other,
% check the next QRule1
addpartialQ([PartialQ1|T0], New_partialQ, [PartialQ1|T]) :-
    addpartialQ(T0, New_partialQ, T), !.

%legalaction(move(X,Y,Z)):-
%  X\=Y, Y\=Z, Z\=X, !.
