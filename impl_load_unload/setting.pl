:- module(setting, [nonDetActions/1, city_limit/1, highway/2, discountfactor/1, convergence_threshold/1]).
nonDetActions(nondet). % nondet/det
city_limit(non).
% box_limit(5).
% truck_limit(4).
discountfactor(0.9).
convergence_threshold(0.0001).



highway(brussels, paris).
highway(brussels, rome).

highway(paris, brussels).

highway(rome, brussels).
% highway(rome, berlin).
% highway(berlin, rome).
% highway(brussels, berlin).
% highway(berlin, brussels).
% highway(moscow, berlin).
% highway(berlin, moscow).
% highway(moscow, brussels).
% highway(brussels, moscow).


% highway(paris, madrid).
% highway(madrid, paris).
% highway(berlin, prague).
% highway(prague, berlin).
% highway(brussels, gent).
% highway(gent, brussels).
% highway(brussels, leuven).
% highway(leuven, brussels).
% highway(paris, barcelona).
% highway(barcelona, paris).
% highway(barcelona, madrid).
% highway(madrid, barcelona).


% highway(leuven, rotterdam).
% highway(rotterdam, leuven).
% highway(rotterdam, amsterdam).
% highway(amsterdam, rotterdam).
% highway(copenhagen, amsterdam).
% highway(amsterdam, copenhagen).
% highway(amsterdam, brussels).
% highway(brussels, amsterdam).









%
% highway(stockholm, copenhagen).
% highway(copenhagen, stockholm).
% highway(madrid, lisbon).
% highway(lisbon, madrid).
% highway(rome, naples).
% highway(naples, rome).
% highway(moscow, helsinki).
% highway(helsinki, moscow).
