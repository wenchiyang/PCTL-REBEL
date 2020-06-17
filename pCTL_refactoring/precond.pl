:- module(precond, [on/2, cl/1, collect/0, clean/0, getall/0,
        allstuff/1
        ]).
:- use_module(library(chr)).

:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint on(?,?), cl(?).
:- chr_constraint clean/0.
:- chr_constraint collect_unclear/0, collect/0, collectstack/0.
:- chr_constraint getall/0, stuff/1, allstuff/1.
%:- chr_constraint headbody/7, partialQ/6, find_partialQ/6.

% clean up after legal state checking
clean \ on(_,_) <=> true.
clean \ cl(_) <=> true.
clean \ allstuff(_) <=> true.
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


getall, cl(X) <=> stuff([cl(X)]).
getall, on(X,Y) <=> stuff([on(X,Y)]).
stuff(S), cl(X) <=> stuff([cl(X)|S]).
stuff(S), on(X,Y) <=> stuff([on(X,Y)|S]).
allstuff(S1)\stuff(S2) <=> S1=S2.
