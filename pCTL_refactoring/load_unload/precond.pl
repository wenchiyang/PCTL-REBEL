:- module(precond, [bin/2, tin/2, on/2,
                    collect/0, findstate/1, clean/0]).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint tin(?,?), on(?,?), bin(?,?).
:- chr_constraint clean/0, collect/0, state/1, findstate/1.

% clean up after legal state checking
clean \ tin(_,_) <=> true.
clean \ on(_,_) <=> true.
clean \ bin(_,_) <=> true.
clean \ findstate(_) <=> true.
clean \ collect <=> true.
clean <=> true.

% illegal states
tin(X, X) <=> fail.
on(X, X) <=> fail.
bin(X, X) <=> fail.

tin(T,C1)\ tin(T,C2) <=> C1 = C2.
on(B,T1)\  on(B,T2)  <=> T1 = T2.
bin(B,C1)\ bin(B,C2) <=> C1 = C2.

on(B,_), bin(B,_) <=> fail.

collect, tin(T,C) <=> state([tin(T,C)]).
collect, on(B,T) <=> state([on(B,T)]).
collect, bin(B,C) <=> state([bin(B,C)]).
state(S), tin(T,C) <=> state([tin(T,C)|S]).
state(S), on(B,T) <=> state([on(B,T)|S]).
state(S), bin(B,C) <=> state([bin(B,C)|S]).
findstate(S1)\state(S2) <=> S1=S2.
