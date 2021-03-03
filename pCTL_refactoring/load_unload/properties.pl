:- use_module('../main').
:- use_module(precond).



reachNsteps(N) :-
    statistics(runtime, [Start|_]),
    Phi = until(_, N, states([[]]), states([[bin(b1,city0)]]), >=, 0.9),
    evaluate(Phi), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).

reachability_tasks_with_a_policy(N):-
    statistics(runtime, [Start|_]),
    Phi = until(_, N, states([[]]), states([[bin(b1,city0)]]), >=, 0.9),
    evaluate(Phi), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).


experiment1 :-
    statistics(runtime, [Start|_]),
    Phi = until(_, 10, states([[]]), states([[bin(b1,paris)]]), >=, 0.9),
    evaluate(Phi), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).


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


message_hook(phistates(Phi), informational, _):-
  Phi =.. [ Functor, PhiStates | Rest ],
  % without_last(Operation, Output),
  nl, nl,
  writeln("query: "), write(Functor), writeln(Rest), nl,
  writeln("answer: "), printall(PhiStates), nl, nl.

message_hook(vf(CurrentVs), informational, _):-
    nl,
    writeln("## value function ##"),
    printall(CurrentVs),
    length(CurrentVs, LCurrentVs),
    write("Number of abstract states: "), writeln(LCurrentVs),
    writeln("########").


message_hook(partialQs(SPQs1, SPQs2), informational, _):-
    length(SPQs1, LSPQs1), length(SPQs2, LSPQs2),
    write("partialQs: "), writeln([LSPQs1, LSPQs2]).

%%
printall([]):- !.
printall([E|R]):-
    writeln(E),
    printall(R),!.

qtov(q(Q,A,S,SS), vf_SARS(s_(S),a_(A),r_(Q),ss_(SS))).
qtov(partialQ(Q,A,S,SS), partialQ(s_(S),a_(A),r_(Q),ss_(SS))).

%message_hook(vfWithAction(QRules), informational, _):-
%    nl,
%    writeln("## value function with action ##"),
%    maplist(qtov, QRules, VRulesAct),
%    printall(VRulesAct),
%    length(VRulesAct, LVRulesAct),
%    write("Number of abstract states: "), writeln(LVRulesAct),
%    writeln("########").