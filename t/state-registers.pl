:- use_module(library(hookscript)).

:- dynamic state:register/2.

hook :-
    req:param(register,'',Register),
    req:method(Method),
    hook(Method,Register,Value),
    write(Value).

hook(post,Register,Value) :-
    req:param(value,'default value',Value),
    ( Register = death -> throw("I am dead"); true ),
    state:retractall(register(Register,_)),
    state:assert(register(Register,Value)).
hook(get,Register,Value) :-
    ( state:register(Register,Value)
    ; format(string(Value),'unknown register: ~w', [Register])
    ).
