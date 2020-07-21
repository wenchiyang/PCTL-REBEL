:- use_module(main).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
:- use_module(precond).
% :- use_module(sorting).
:- use_module(util).
:- use_module(setting).

experiment1:-
    statistics(runtime, [Start|_]),
    evaluate(until(_, 3, states([[]]), states([[on(a,b)]]), >=, 0.5)), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,R|_],
    protocola('experiments/exp1.txt'),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.


experiment2:-
    statistics(runtime, [Start|_]),
    evaluate(until(_, 6, states([[on(c,d)]]), states([[on(a,b)]]), >=, 0.6)), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,_,R|_],
    protocola('experiments/exp2.txt'),
    print_message(informational, exetime(Start, Stop)),
    noprotocol.


experiment5_outer1 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            1,
            states([[cl(a)]]),
            and(
                _,
                states([[on(a,b)]]),
                until(
                    _,
                    2,
                    next(_, states([[cl(e)]]), >=, 0.9),
                    states([[on(c,d)]]),
                    >=, 0.9
                )
            ),
        >=, 0.5)
    ), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).


% ex:-
%     protocol('experiments/test1.txt'),
%     print_message(informational, exetime(0.0, 1.0)),
%     print_message(informational, exetime(0.0, 2.0)),
%     noprotocol.
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


message_hook(phistates(Phi), informational, _):-
  Phi =.. [ Functor, PhiStates | Rest ],
  % without_last(Operation, Output),
  nl, nl,
  writeln("query: "), write(Functor), writeln(Rest), nl,
  writeln("answer: "), printall(PhiStates), nl, nl.
