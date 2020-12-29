:- use_module('../main').
:- use_module(precond).


queryyy(CrashStates, VS) :-
    Phi = until(VS, 1, states([[]]),
    CrashStates,
     >=, 0.1),
    evaluate(Phi), !.

query(CurrentState, CrashStates) :-
    Phi = until(VS, 3, states([[]]),
    CrashStates,
     >=, 0.0),
    evaluate(Phi), !.
%
%    (
%    include([X]>>(arg(2,X,CurrentState)), VS, [v(P,CurrentState)])
%    ->
%    write("TRUE:"), write(P)
%    ;
%    write("FALSE")
%    ).


experiment1 :-
    statistics(runtime, [Start|_]),
    Phi = until(_, 2, states([[]]),
    states([[
        pacman(6,4),
        ghost(0,6,4)
    ],
    [
        pacman(6,5),
        ghost(0,6,5)
%        wall(0,0), wall(0,1), wall(0,2),wall(0,3),wall(0,4),wall(0,5),wall(0,6),
%        wall(1,0),wall(1,6),
%        wall(2,0),wall(2,2),wall(2,4),wall(2,6),
%        wall(3,0),wall(3,2),wall(3,3),wall(3,4), wall(3,6),
%        wall(4,0),wall(4,2),wall(4,4),wall(4,6),
%        wall(5,0),wall(5,2),wall(5,4),wall(5,6),
%        wall(6,0),wall(6,6),
%        wall(7,0),wall(7,1),wall(7,2),wall(7,3),wall(7,4),wall(7,5),wall(7,6)
    ]
    ]), >=, 0.6),
    evaluate(Phi), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,R|_],
    % print_message(informational, phistates(Phi)),
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

qtov(q(Q,A,S,SS), vf_SARS(s_(S),a_(A),r_(Q),ss_(SS))).
%qtov(partialQ(Q,A,S,SS), partialQ(s_(S),a_(A),r_(Q),ss_(SS))).
%
%message_hook(vfWithAction(QRules), informational, _):-
%    nl,
%    writeln("## value function with action ##"),
%    maplist(qtov, QRules, VRulesAct),
%    printall(VRulesAct),
%    length(VRulesAct, LVRulesAct),
%    write("Number of abstract states: "), writeln(LVRulesAct),
%    writeln("########").


qtov_table(q(Q,pacman_move(Dir),S,_), a(Pacman, Dir, Ghost, Q) ):-
    ground(Dir),
    member(pacman(X,Y), S), Pacman = [X,Y], ground([X,Y]),
    member(ghost(0,A,B), S), Ghost = [A,B], ground([A,B]),
    !.
qtov_table(_, []):-!.

message_hook(vfWithAction(QRules), informational, _):-
    nl,
    writeln("## value function with action ##"),
    maplist(qtov_table, QRules, VRulesAct),
    include([X]>>(X\=[]),VRulesAct,VRulesActFiltered),
    printall_table(VRulesActFiltered),
    length(VRulesActFiltered, LVRulesActFiltered),
    write("Number of abstract states: "), writeln(LVRulesActFiltered),
    writeln("########").


message_hook(partialQs(SPQs), informational, _):-
    maplist(length, SPQs, Ls),
    write("partialQs: "), writeln(Ls).

%%
printall([]):- !.
printall([E|R]):-
    writeln(E),
    printall(R),!.

printall_table([]):- !.
printall_table([a(Pacman, Dir, Ghost, Prob)|R]):-
    write("("),write((Pacman, Dir, Ghost, Prob)),writeln("), "),
    printall_table(R),!.