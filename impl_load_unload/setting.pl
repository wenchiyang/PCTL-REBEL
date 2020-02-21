:- module(setting, [nonDetActions/1, city_limit/1, highway/2, discountfactor/1, convergence_threshold/1]).
nonDetActions(nondet). % nondet/det
city_limit(non).
% box_limit(5).
% truck_limit(4).
discountfactor(0.9).
convergence_threshold(0.0001).

highway(berlin, brussels).
highway(berlin, moscow).
highway(berlin, rome).
highway(brussels, berlin).
highway(brussels, moscow).
highway(brussels, paris).
highway(brussels, rome).
highway(moscow, berlin).
highway(moscow, brussels).
highway(paris, brussels).
highway(rome, berlin).
highway(rome, brussels).


highway(copenhagen, amsterdam).
highway(amsterdam, copenhagen).
highway(amsterdam, brussels).
highway(brussels, amsterdam).
highway(paris, madrid).
highway(madrid, paris).
highway(berlin, prague).
highway(prague, berlin).
