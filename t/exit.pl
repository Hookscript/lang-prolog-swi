:- use_module(library(hookscript)).

hook :-
    req:param(try,Try),
    format(string(Msg),"Try ~d~n", [Try]),
    hook(Try,Msg).

hook(1,Msg) :-
    throw(Msg).
hook(2,Msg) :-
    format(user_error,"~s",[Msg]),
    halt(1).
