:- module(setting, [
%                    nonDetActions/1,
                    blocks_limit/1,
                    discountfactor/1,
                    convergence_threshold/1,
                    transition/5,
                    oi_option/1,
                    mydif/2]).
:- use_module(library(clpfd)).
% file_search_path(domain, 'blocksworld').

%nonDetActions(nondet). % nondet/det
blocks_limit(non). % non/an integer > 3
discountfactor(1). %
convergence_threshold(0.01). % residual for the VI algorithm to stop
oi_option(flexible). % flexible : create oi states when needed
                  % force : always create oi states


% transition(
%     action,
%     the ith consequece,
%     probability,
%     head_i,
%     body)
transition(pacman_move(north), 1, 0.9, [pacman(X1,Y1)], [pacman(X1,Y2)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X1,Y2)), AllWalls),
    Y2 #= Y1-1.

transition(pacman_move(south), 1, 0.9,[pacman(X1,Y1)],[pacman(X1,Y2)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X1,Y2)), AllWalls),
    Y2 #= Y1+1.

transition(pacman_move(east), 1, 0.9,[pacman(X1,Y1)],[pacman(X2,Y1)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X2,Y1)), AllWalls),
    X2 #= X1-1.

transition(pacman_move(west), 1, 0.9,[pacman(X1,Y1)],[pacman(X2,Y1)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X2,Y1)), AllWalls),
    X2 #= X1+1.

transition(pacman_move(stay), 1, 0.9,[pacman(X1,Y1)],[pacman(X1,Y1)]):-
    X1 #= X1, Y1 #= Y1.

transition(pacman_move(_), 2, 0.1,[pacman(X1,Y1)],[pacman(X1,Y1)]):-
    X1 #= X1, Y1 #= Y1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transition(ghost_move, 1, 0.2,[ghost(ID1,X1,Y1)],[ghost(ID1,X1,Y2)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X1,Y2)), AllWalls),
    Y2 #= Y1-1.

transition(ghost_move, 2, 0.2,[ghost(ID1,X1,Y1)],[ghost(ID1,X1,Y2)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X1,Y2)), AllWalls),
    Y2 #= Y1+1.

transition(ghost_move, 3, 0.2,[ghost(ID1,X1,Y1)],[ghost(ID1,X2,Y1)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X2,Y1)), AllWalls),
    X2 #= X1-1.

transition(ghost_move, 4, 0.2,[ghost(ID1,X1,Y1)],[ghost(ID1,X2,Y1)]):-
    findall(wall(X,Y),wall(X,Y),AllWalls),
    maplist(mydif(wall(X2,Y1)), AllWalls),
    X2 #= X1+1.

transition(ghost_move, 5, 0.2,[ghost(ID1,X1,Y1)],[ghost(ID1,X1,Y1)]):-
    X1 #= X1, Y1 #= Y1.



%transition(pacman_move(north), 1, 0.225, [pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1-1,
%    Y2 #= Y1-1, X1 #= X1.
%transition(pacman_move(north), 2, 0.225, [pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1+1,
%    Y2 #= Y1-1, X1 #= X1.
%transition(pacman_move(north), 3, 0.225, [pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1-1, B1 #= B1,
%    Y2 #= Y1-1, X1 #= X1.
%transition(pacman_move(north), 4, 0.225, [pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1+1, B1 #= B1,
%    Y2 #= Y1-1, X1 #= X1.
%transition(pacman_move(north), 5, 0.1, [pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls), A1 #= A1, B1 #= B1,
%    Y2 #= Y1-1, X1 #= X1.
%
%transition(pacman_move(south), 1, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1-1,
%    Y2 #= Y1+1, X1 #= X1.
%transition(pacman_move(south), 2, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1+1,
%    Y2 #= Y1+1, X1 #= X1.
%transition(pacman_move(south), 3, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1-1, B1 #= B1,
%    Y2 #= Y1+1, X1 #= X1.
%transition(pacman_move(south), 4, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1+1, B1 #= B1,
%    Y2 #= Y1+1, X1 #= X1.
%transition(pacman_move(south), 5, 0.1,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X1,Y2),ghost(ID1,A1,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X1,Y2)), AllWalls), A1 #= A1, B1 #= B1,
%    Y2 #= Y1+1, X1 #= X1.
%
%
%transition(pacman_move(east), 1, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1-1,
%    X2 #= X1+1, Y1 #= Y1.
%transition(pacman_move(east), 2, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1+1,
%    X2 #= X1+1, Y1 #= Y1.
%transition(pacman_move(east), 3, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1-1, B1 #= B1,
%    X2 #= X1+1, Y1 #= Y1.
%transition(pacman_move(east), 4, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1+1, B1 #= B1,
%    X2 #= X1+1, Y1 #= Y1.
%transition(pacman_move(east), 5, 0.1,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls), A1 #= A1, B1 #= B1,
%    X2 #= X1+1, Y1 #= Y1.
%
%
%transition(pacman_move(west), 1, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1-1,
%    X2 #= X1-1, Y1 #= Y1.
%transition(pacman_move(west), 2, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1+1,
%    X2 #= X1-1, Y1 #= Y1.
%transition(pacman_move(west), 3, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1-1, B1 #= B1,
%    X2 #= X1-1, Y1 #= Y1.
%transition(pacman_move(west), 4, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1+1, B1 #= B1,
%    X2 #= X1-1, Y1 #= Y1.
%transition(pacman_move(west), 5, 0.1,[pacman(X1,Y1),ghost(ID1,A1,B1)], [pacman(X2,Y1),ghost(ID1,A1,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(X2,Y1)), AllWalls),A1 #= A1, B1 #= B1,
%    X2 #= X1-1, Y1 #= Y1.
%
%transition(pacman_move(stay), 1, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)],[pacman(X1,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1-1,
%    X1 #= X1, Y1 #= Y1.
%transition(pacman_move(stay), 2, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)],[pacman(X1,Y1),ghost(ID1,A1,B2)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(A1,B2)), AllWalls), A1 #= A1, B2 #= B1+1,
%    X1 #= X1, Y1 #= Y1.
%transition(pacman_move(stay), 3, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)],[pacman(X1,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1-1, B1 #= B1,
%    X1 #= X1, Y1 #= Y1.
%transition(pacman_move(stay), 4, 0.225,[pacman(X1,Y1),ghost(ID1,A1,B1)],[pacman(X1,Y1),ghost(ID1,A2,B1)]):-
%    findall(wall(X,Y),wall(X,Y),AllWalls),
%    maplist(mydif(wall(A2,B1)), AllWalls), A2 #= A1+1, B1 #= B1,
%    X1 #= X1, Y1 #= Y1.
%transition(pacman_move(stay), 5, 0.1,[pacman(X1,Y1),ghost(ID1,A1,B1)],[pacman(X1,Y1),ghost(ID1,A1,B1)]):-
%    A1 #= A1, B1 #= B1,
%    X1 #= X1, Y1 #= Y1.



%transition(pacman_move(_), 2, 0.1,[pacman(X1,Y1)],[pacman(X1,Y1)]):-
%    X1 #= X1, Y1 #= Y1.


mydif(X,Y):- (X \= Y -> true; dif(X,Y)).

wall(0,0).
wall(0,1).
wall(0,2).
wall(0,3).
wall(0,4).
wall(0,5).
wall(0,6).
wall(1,0).
wall(1,6).
wall(2,0).
wall(2,2).
wall(2,4).
wall(2,6).
wall(3,0).
wall(3,2).
wall(3,3).
wall(3,4).
wall(3,6).
wall(4,0).
wall(4,2).
wall(4,4).
wall(4,6).
wall(5,0).
wall(5,2).
wall(5,4).
wall(5,6).
wall(6,0).
wall(6,6).
wall(7,0).
wall(7,1).
wall(7,2).
wall(7,3).
wall(7,4).
wall(7,5).
wall(7,6).

%pacman(6,3).
%pacman_move(north).
%ghost(0,1,2).
%ghost_move(0, north).

