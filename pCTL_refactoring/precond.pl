:- module(precond, [on/2, cl/1, clean/0, findstate/1, collect/0]).
:- use_module(library(chr)).

:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint on(?,?), cl(?).
:- chr_constraint clean/0.
:- chr_constraint collect/0.
:- chr_constraint state/1, findstate/1.

% clean up after legal state checking
clean \ on(_,_) <=> true.
clean \ cl(_) <=> true.
clean \ findstate(_) <=> true.
clean \ collect <=> true.
clean <=> true.

% illegal states
on_itself        @ on(X, X) <=> fail.
not_clear        @ on(_,B), cl(B) <=> fail.
on_two_blocks    @ on(A,B)\ on(A,D) <=> B=D.
under_two_blocks @ on(A,B)\ on(C,B) <=> A=C.
%fl_on_top        @ on(fl,_) <=> fail.
duplicate_clear  @ cl(X) \ cl(X) <=> true.

collect, cl(X) <=> state([cl(X)]).
collect, on(X,Y) <=> state([on(X,Y)]).
state(S), cl(X) <=> state([cl(X)|S]).
state(S), on(X,Y) <=> state([on(X,Y)|S]).
findstate(S1)\state(S2) <=> S1=S2.
