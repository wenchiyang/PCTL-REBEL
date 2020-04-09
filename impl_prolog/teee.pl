main:-
    open('teee.txt', write, Stream),
    w(Stream),
    close(Stream).

w(Stream) :-
    print_message(informational, hello(Stream, 1)).

message_hook(hello(Stream, DD), informational, _):-
    writeln(Stream, DD).

w :-
    print_message(informational, nono(1)).

message_hook(nono(DD), informational, _):-
    writeln(DD).
%message_property(informational, stream(Stream)).
