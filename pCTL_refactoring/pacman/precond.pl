:- module(precond, [pacman/2, ghost/3, clean/0, findstate/1, collect/0]).
:- use_module(library(chr)).
:- use_module(chr(chr_runtime)).
%:- set_prolog_flag(optimize, full).

% module of generating available actions, i.e. move/3, from the input precondition
:- chr_constraint pacman(?,?), ghost(?,?,?).
:- chr_constraint clean/0.
:- chr_constraint collect/0.
:- chr_constraint state/1, findstate/1.

% clean up after legal state checking
clean \ pacman(_,_) <=> true.
clean \ ghost(_,_,_) <=> true.
clean \ findstate(_) <=> true.
clean \ collect <=> true.
clean <=> true.

ghost(ID1,A1,B1) \ ghost(ID2,A2,B2) <=> ID1=ID2 | A1=A2, B1=B2. %writeln(ghost(ID1,A1,B1)), writeln(ghost(ID2,A2,B2)).
pacman(X1,Y1) \ pacman(X2,Y2) <=> X1=X2, Y1=Y2.
% illegal states
%into_wall        @ pacman(X, Y), wall(X,Y) <=> fail.


collect, pacman(X,Y) <=> state([pacman(X,Y)]).
collect, ghost(ID,X,Y) <=> state([ghost(ID,X,Y)]).
state(S), pacman(X,Y) <=> state([pacman(X,Y)|S]).
state(S), ghost(ID,X,Y) <=> state([ghost(ID,X,Y)|S]).
findstate(S1)\state(S2) <=> S1=S2.
