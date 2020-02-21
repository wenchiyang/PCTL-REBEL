:- module(precond, [on/2, cl/1, s/7, stack/1, collect/0, clean/0,
        find_stacks/1, collectstack/0, collect_unclear/0
        ]).
:- use_module(library(chr)).

:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint on(?,?), cl(?).
:- chr_constraint s(+float, ?, +float, ?, ?, ?, ?).
:- chr_constraint clean/0, stack(?), stackstruct(?, +), find_stacks/1, stacks/1.
:- chr_constraint collect_unclear/0, collect/0,collectstack/0.
%:- chr_constraint headbody/7, partialQ/6, find_partialQ/6.

% clean up after legal state checking
clean \ on(_,_) <=> true.
clean \ cl(_) <=> true.
clean \ stackstruct(_,_) <=> true.
clean \ find_stacks(_) <=> true.
clean <=> true.

% illegal states
on_itself        @ on(X, X) <=> fail.
not_clear        @ on(_,B), cl(B) <=> fail.
on_two_blocks    @ on(A,B)\ on(A,D) <=> B=D.
under_two_blocks @ on(A,B)\ on(C,B) <=> A=C.
%fl_on_top        @ on(fl,_) <=> fail.
duplicate_clear  @ cl(X) \ cl(X) <=> true.

%%%% collect converts on/2 and cl/1 into stack/1
% convert cl/1 into stack([cl/1]) so that
% all stacks with a clear top block are collected
collect \ cl(X) <=> stack([cl(X)]).

% capture stacks that does not have a cl/1 on top
collect <=> true.

% get structure: stack is reversed
% (1) collect all stacks with a clear top block
stack([cl(A)|Vars]), on(A,B) <=>
    stack([on(A,B), cl(A)|Vars]).
stack([on(C,A)|Vars]), on(A,B) <=>
    stack([on(A,B), on(C,A)|Vars]).
% (2) collect stacks without a clear top block
stack(Stack), on(A,B) <=> last(Stack,on(B,_)) |
    append(Stack, [on(A,B)], L),
    stack(L).

collect_unclear \ on(A,B), on(B,C) <=>
    stack([on(B,C),on(A,B)]).
collect_unclear \ on(A,B) <=>
    stack([on(A,B)]).
collect_unclear <=> true.

all_dif([]):-!.
all_dif([E|ListBlocks]):-
    maplist(mydif(E), ListBlocks),
    all_dif(ListBlocks), !.

%%%%% sortstack sorts all stacks and computes their structures
stack(RevStack) <=>
    reverse(RevStack, Stack),
    predInList(Stack, on, OnStack),
    predInList(Stack, cl, ClStack),
    %=====blocks of the same stack are all different=======

    %============
    length(OnStack, LOn),
    length(ClStack, LCl),
    stackstruct(Stack, [LCl, LOn]).

collectstack <=> stacks([]).
stacks(Stacks), stackstruct(SA,LA) <=>
    stacks([sta(LA,SA)|Stacks]).

% Stack1/Stack2 is a list of sta([ClL,OnL], State)
find_stacks(Stacks1)\ stacks(Stacks2) <=>
    Stacks1=Stacks2.
