:- use_module('../main').
:- use_module(precond).


experiment1:-
    statistics(runtime, [Start|_]),
    Phi = until(_, 3, states([[]]), states([[on(a,b)]]), >=, 0.6),
    evaluate(Phi), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,R|_],
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experimentX_iter_1 :-
    statistics(runtime, [Start|_]),
    evaluate(next(_, states([[on(a,b)]]), >=, 0.9)), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).

experimentF_iter_1 :-
    statistics(runtime, [Start|_]),
    evaluate(until(_, 1, states([[]]), states([[on(a,b)]]), >=, 0.9)), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).

experimentU_iter_1 :-
    statistics(runtime, [Start|_]),
    evaluate(until(_, 1, states([[on(c,d)]]),states([[on(a,b)]]), >=, 0.9)), !,
    statistics(runtime, [Stop|_]),
    print_message(informational, exetime(Start, Stop)).

experiment2:-
    statistics(runtime, [Start|_]),
    evaluate(until(_, 3, states([[on(c,d)]]), states([[on(a,b)]]), >=, 0.6)), !,
    statistics(runtime, [Stop|_]),
    % Res = [_,_,R|_],
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experiment5_inner1 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
            states([[cl(a)]]),
            and(
                _,
                states([[on(a,b)]]),
                until(
                    _,
                    1,
                    next(_, states([[cl(e)]]), >=, 0.9),
                    states([[on(c,d)]]),
                    >=, 0.9
                )
            ),
        >=, 0.5)
    ), !,
    statistics(runtime, [Stop|_]),
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).
experiment5_inner2 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
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
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).
experiment5_inner3 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
            states([[cl(a)]]),
            and(
                _,
                states([[on(a,b)]]),
                until(
                    _,
                    3,
                    next(_, states([[cl(e)]]), >=, 0.9),
                    states([[on(c,d)]]),
                    >=, 0.9
                )
            ),
        >=, 0.5)
    ), !,
    statistics(runtime, [Stop|_]),
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).
experiment5_inner4 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
            states([[cl(a)]]),
            and(
                _,
                states([[on(a,b)]]),
                until(
                    _,
                    4,
                    next(_, states([[cl(e)]]), >=, 0.9),
                    states([[on(c,d)]]),
                    >=, 0.9
                )
            ),
        >=, 0.5)
    ), !,
    statistics(runtime, [Stop|_]),
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).
experiment5_inner5 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
            states([[cl(a)]]),
            and(
                _,
                states([[on(a,b)]]),
                until(
                    _,
                    5,
                    next(_, states([[cl(e)]]), >=, 0.9),
                    states([[on(c,d)]]),
                    >=, 0.9
                )
            ),
        >=, 0.5)
    ), !,
    statistics(runtime, [Stop|_]),
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

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
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experiment5_outer2 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            2,
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
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experiment5_outer3 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            3,
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
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experiment5_outer4 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            4,
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
    % print_message(informational, phistates(Phi)),
    print_message(informational, exetime(Start, Stop)).

experiment5_outer5 :-
    statistics(runtime, [Start|_]),
    evaluate(
        until(_,
            5,
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


message_hook(partialQs(SPQs1, SPQs2), informational, _):-
    length(SPQs1, LSPQs1), length(SPQs2, LSPQs2),
    write("partialQs: "), writeln([LSPQs1, LSPQs2]).

%%
printall([]):- !.
printall([E|R]):-
    writeln(E),
    printall(R),!.
