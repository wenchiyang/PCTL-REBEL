:- module(precond, [bin/2, tin/2, on/2, collect/0, clean/0,
        find_stacks/1, collectstack/0
        ]).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint tin(?,?), on(?,?), bin(?,?).
:- chr_constraint s(+float, ?, +float, ?, ?, ?, ?).
:- chr_constraint clean/0, stack(?,?), stackstruct(?, ?, +), find_stacks/1, stacks/1.
:- chr_constraint collect/0,collectstack/0.
%:- chr_constraint headbody/7, partialQ/6, find_partialQ/6.

% clean up after legal state checking
clean \ tin(_,_) <=> true.
clean \ on(_,_) <=> true.
clean \ bin(_,_) <=> true.
clean \ stackstruct(_,_,_) <=> true.
clean \ find_stacks(_) <=> true.
clean <=> true.

% illegal states
tin(X, X) <=> fail.
on(X, X) <=> fail.
bin(X, X) <=> fail.

tin(T,C1)\ tin(T,C2) <=> C1 = C2.
on(B,T1)\  on(B,T2)  <=> T1 = T2.
bin(B,C1)\ bin(B,C2) <=> C1 = C2.

on(B,_), bin(B,_) <=> fail.


% union

collect \ stack(C, Vars), tin(T,C) <=> stack(C, [tin(T,C)|Vars]).
collect \ stack(C, Vars), bin(B,C) <=> stack(C, [bin(B,C)|Vars]).
collect \ stack(C, Vars), on(B,T) <=>
    member(tin(T,C), Vars) | stack(C, [on(B,T)|Vars]).


collect \ tin(T,C) <=> stack(C, [tin(T,C)]).
collect \ bin(B,C) <=> stack(C, [bin(B,C)]).

collect <=> true.


%%%%% sortstack sorts all stacks and computes their structures
stack(City, Stack) <=>
    predInList(Stack, bin, BinStack),
    predInList(Stack, tin, TinStack),
    predInList(Stack, on, OnStack),
    length(BinStack, LBin),
    length(TinStack, LTin),
    length(OnStack, LOn),
    list_to_set1(BinStack, BinStackSorted),
    list_to_set1(TinStack, TinStackSorted),
    list_to_set1(OnStack, OnStackSorted),
    flatten([BinStackSorted, TinStackSorted, OnStackSorted], StackSorted),
    stackstruct(City, StackSorted, [LBin, LTin, LOn]).

collectstack <=> stacks([]).
stacks(Stacks), stackstruct(City,SA,LA) <=>
    stacks([sta(City,LA,SA)|Stacks]).

% Stack1/Stack2 is a list of sta([ClL,OnL], State)
find_stacks(Stacks1)\ stacks(Stacks2) <=>
    Stacks1=Stacks2.
