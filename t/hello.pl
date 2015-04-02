:- use_module(library(hookscript)).

hook :-
    req:param(whom,world,Whom),
    format("Hello, ~s!~n", [Whom]).
