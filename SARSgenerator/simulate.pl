:- use_module(precond).
:- use_module(util).
:- use_module(sorting).

domain([a, b, fl]).
initialstate([cl(a), cl(b), cl(fl)]).
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
state_reward(10, [on(a,b)]).

mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

%
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

p :-
    initialstate(SS),
    % domain(R),
    findall(SARS, generateSARS(SS, SARS), SARSs),
    print_message(informational, sars(SARSs))
    .

% use backtracking to generate all SARS given S
generateSARS(SS, sars(S, Act, R, SS)):-
    transition(A, Prob, R, Head, Body), % choose a transition
    wpi(t(A, Prob, R, Head, Body), sars(S, Act, R, SS)),
    termsInState(S, STerms),
    generateOIstate(STerms, OISTerms),
    term_variables(OISTerms, []),
    legalstate(S).


wpi(t(Act, Prob, R, Head, Body), sars(S, Act, R, SS)):-
    % get a subH
    subsetgen(Head, SubH),
    % create all possible \theta
    structsubset(SubH, SS, SubHp),
    % create VFSTail using \theta
    sort(SS, SSTT), sort(SubHp, SubHpTT),
    ord_subtract(SSTT, SubHpTT, SSTail),
    % writeln(h(SSTT, SubHpTT, SSTail)),nl,
    headbodyo(Head, SSTail, Prob, Act, Body, partialQ(Prob,Act,S)).
    % oi_option(OI_option),
    % oi_qrule(partialQ(Prob,Act,SS), OI_option).

headbodyo(Head, SSTail, Prob, Act, Body, partialQ(Prob,Act,Glb)):-
    extract(SSTail), extract(Body),
    getstate(GlbTT),
    % writeln(GlbTT).
    subsumesort(GlbTT, Glb),
    sort(Head, HeadTT), sort(Body, BodyTT),
    ord_union(HeadTT, BodyTT, Lpp),
    cartesian_dif(SSTail, Lpp).
