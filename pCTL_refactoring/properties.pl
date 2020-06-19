:- use_module(main).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
:- use_module(precond).
% :- use_module(sorting).
:- use_module(util).
:- use_module(setting).

ex :-
    % protocol('experiments/exp1_singlethread.txt'),
    statistics(runtime, [Start|_]),
    evaluate(until(3, [[]], [[on(a,b)]], >=, 0.8, Res), Res), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,_,_,R|_],
    print_message(informational, exetime(Start, Stop)).
    % noprotocol.

%%
%%
% Iteration 1
%
% partialQs: [3,3]
% partialQ time : 0.008 s
%        Q time : 0.001 s
% Iteration 2
%
% partialQs: [17,17]
% partialQ time : 0.005 s
%        Q time : 0.027 s
% Iteration 3
%
% partialQs: [50,50]
% partialQ time : 0.037 s
%        Q time : 0.304 s
% Iteration 4
%
% partialQs: [122,122]
% partialQ time : 0.306 s
%        Q time : 3.063 s
% Iteration 5
%
% partialQs: [233,233]
% partialQ time : 1.814 s
%        Q time : 21.378 s
% Iteration 6
%
% partialQs: [383,383]
% partialQ time : 9.084 s
%        Q time : 113.55 s
%
% Iteration 7
%
% partialQs: [572,572]
% partialQ time : 38.943 s
%        Q time : 493.808 s
% Iteration 8
%
% partialQs: [800,800]
% partialQ time : 152.299 s


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
