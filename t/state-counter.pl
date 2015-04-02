:- use_module(library(hookscript)).

hook(N0, N) :-
    ignore(N0=0),  % N0 unbound when no state has been stored
    succ(N0,N),
    write(N).
