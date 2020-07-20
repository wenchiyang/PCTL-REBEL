:- use_module(main).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
:- use_module(precond).
% :- use_module(sorting).
:- use_module(util).
:- use_module(setting).

experiment1:-
    % protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(10, [[]], [[on(a,b)]], >=, 0.5, Res), Res), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,R|_],
    print_message(informational, exetime(Start, Stop)).
    % noprotocol.

experiment2:-
    % protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(6, [[on(c,d)]], [[on(a,b)]], >=, 0.6, Res), Res), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,_,R|_],
    print_message(informational, exetime(Start, Stop)).
    % noprotocol.
%%
%%


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

message_hook(phistates(Phi, PhiStates), informational, _):-
  Phi =.. Operation,
  without_last(Operation, Output), nl, nl,
  writeln("query: "), writeln(Output), nl,
  writeln("answer: "), printall(PhiStates), nl, nl.
