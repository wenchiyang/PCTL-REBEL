:- use_module(precond).
:- use_module(util).
:- use_module(sorting).
:- use_module(allgroundstates).



% Use backtracking to generate all ground states
% This produces duplicates...
% E.g. [cl(X), cl(Y))] with domain([a,b]) generates
% (1) [cl(a), cl(b))] and (2) [cl(b), cl(a))]
generateInterpretations(Interpretation):-
    domain(Domain),
    allgroundstructures(ALLStructs),
    member(Interpretation, ALLStructs),
    permutation(Domain, Domainp),
    term_variables(Interpretation,Domainp).

% Ins is the set of all Ground interpretations!!!!
allInterpretations:-
    findall_Interpretations(In, generateInterpretations(In), Ins),
    length(Ins, LIns),
    printall(Ins),
    write(LIns),
    writeln(" ground states.").

findall_Interpretations(X, Goal, Results) :-
    State = state([]),
    (
    Goal,
    arg(1, State, S0),
    add(S0, X, S),
    nb_setarg(1, State, S),
    fail
    ;
    arg(1, State, Results)
    ).

add([], NewState, [NewState]) :- !.

add([SomeState|States], NewState, [SomeState|States]):-
    thetasubsumes(SomeState, NewState), !.

add([SomeState|T0], NewState, [SomeState|T]) :-
    add(T0, NewState, T), !.





% % Two states
% initialstates([
%     [cl(a), cl(b)],
%     [cl(a), on(a,b)],
%     [cl(b), on(b,a)]]).

% Three states
% initialstates([
%     [cl(a), cl(b), cl(c)],
%     [cl(a), cl(c), on(a,b)],
%     [cl(b), cl(c), on(b,a)],
%     [cl(a), cl(b), on(a,c)],
%     [cl(b), cl(c), on(c,a)],
%     [cl(a), cl(b), on(b,c)],
%     [cl(a), cl(c), on(c,b)],
%     [cl(a), on(a,b), on(b,c)],
%     [cl(a), on(a,c), on(c,b)],
%     [cl(b), on(b,a), on(a,c)],
%     [cl(b), on(b,c), on(c,a)],
%     [cl(c), on(c,a), on(a,b)],
%     [cl(c), on(c,b), on(b,a)]
%     ]).

discountfactor(1). % used in the bellman update operator
convergence_threshold(0.01). % residual for the VI algorithm to stop
% oi_option(force).


% Transition function
% transition(
%     action,
%     probability,
%     reward,
%     head_i,
%     body)
transition(move(X,Y,Z), 0.9, -2,
        [cl(X), cl(Z), on(X,Y)],
        [cl(X), cl(Y), on(X,Z)]):-
            mydif(X,Y), mydif(Y, Z), mydif(X,Z).
transition(move(X,Y,Z), 0.1, -1,
        [cl(X), cl(Y), on(X,Z)],
        [cl(X), cl(Y), on(X,Z)]):-
            mydif(X,Y), mydif(Y, Z), mydif(X,Z).

% a special form of the reward function
% This is assumed to be an ordered list.
state_reward(10, [on(a,b)]).
%


p :-
    % domain(R),
    findall(SARS, generateSARS(SARS), SARSs),
    print_message(informational, sars(SARSs))
    .

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

% use backtracking to generate all SARS given S
generateSARS(SARS):-
    initialstates(SSs), member(SS, SSs),
    transition(A, Prob, R, Head, Body), % choose a transition
    regress(SS, t(A, Prob, R, Head, Body), SARS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% oi(State)
% This uses backtracking to generate all OI states (i.e. all variables are different)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
oi(S):-
    termsInState(S, STerms),
    % fl is a special constant
    generateOIstate([fl|STerms], OISTerms),
    term_variables(OISTerms, []),
    legalstate(S).


%
% domain_states(States):-
%     domain(Elements), .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regress(PostCond, Transition, SARSTuple)
% This uses backtracking to regress all possible ground SARS tuples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
regress(SS, t(Act, _, TransR, Head, Body), sars(S, Act, R, SS)):-
    % get a subH
    subsetgen(Head, SubH),
    % create all possible \theta
    structsubset(SubH, SS, SubHp),
    % create VFSTail using \theta
    sort(SS, SSTT), sort(SubHp, SubHpTT),
    ord_subtract(SSTT, SubHpTT, SSTail),
    % writeln(h(SSTT, SubHpTT, SSTail)),nl,
    precond(Head, SSTail, Body, S),
    oi(S),
    stateR(SS, StateR),
    R is StateR + TransR.


stateR(S, StateR):-
    state_reward(StateR, StatePattern),
    thetasubsumes(StatePattern, S), !.
stateR(_, 0):-!.


precond(Head, SSTail, Body, Glb):-
    extract(SSTail), extract(Body),
    getstate(GlbTT),
    subsumesort(GlbTT, Glb),
    sort(Head, HeadTT), sort(Body, BodyTT),
    ord_union(HeadTT, BodyTT, Lpp),
    cartesian_dif(SSTail, Lpp).

%%
printall([]):- !.
printall([E|R]):-
    writeln(E),
    printall(R),!.

%
qtov(sars(S,A,Q,SS), sars(s_(S),a_(A),r_(Q),ss_(SS))).

message_hook(sars(QRules), informational, _):-
    nl,
    writeln("## value function with action ##"),
    maplist(qtov, QRules, VRulesAct),
    printall(VRulesAct),
    length(VRulesAct, LVRulesAct),
    write("Number of abstract states: "), writeln(LVRulesAct),
    writeln("########").
